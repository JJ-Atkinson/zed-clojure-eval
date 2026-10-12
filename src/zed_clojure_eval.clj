(ns zed-clojure-eval
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [bencode.core :as bencode]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as reader-types]
            [rewrite-clj.node :as node]
            [rewrite-clj.parser :as parser])
  (:import [java.io PushbackInputStream RandomAccessFile]
           [java.net InetSocketAddress Socket]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]
           [java.util UUID]
           [java.util.regex Pattern]))

(defn parse-port
  [value description]
  (let [value (some-> value
                      str/trim)]
    (when (str/blank? value) (throw (ex-info (str description " is empty") {})))
    (let [port (parse-long value)]
      (when-not (and port (<= 1 port 65535))
        (throw (ex-info
                 (str description " is not a valid TCP port: " (pr-str value))
                 {})))
      port)))

(defn ancestor-paths
  [path]
  (take-while some?
              (iterate #(some-> %
                                fs/parent)
                       (fs/absolutize path))))

(defn find-port-file
  [source-file]
  (let [start (if (str/blank? source-file)
                (fs/absolutize ".")
                (fs/parent (fs/absolutize source-file)))]
    (some #(let [candidate (fs/path % ".nrepl-port")]
             (when (fs/regular-file? candidate) candidate))
          (ancestor-paths start))))

(defn find-project-config-file
  [source-file]
  (let [start (if (str/blank? source-file)
                (fs/absolutize ".")
                (fs/parent (fs/absolutize source-file)))]
    (some #(let [candidate (fs/path % ".zed" "clojure-repl.edn")]
             (when (fs/regular-file? candidate) candidate))
          (ancestor-paths start))))

(defn endpoint
  ([environment] (endpoint environment false))
  ([environment allow-missing-port?]
   (let
     [source-file (get environment "ZED_CLOJURE_FILE")
      explicit-port (not-empty (get environment "ZED_CLOJURE_NREPL_PORT"))
      explicit-port-file (not-empty (get environment
                                         "ZED_CLOJURE_NREPL_PORT_FILE"))
      discovered-port-file (when-not explicit-port (find-port-file source-file))
      port-file (if explicit-port-file
                  (fs/absolutize explicit-port-file)
                  discovered-port-file)
      port
        (cond
          explicit-port (parse-port explicit-port "ZED_CLOJURE_NREPL_PORT")
          port-file (parse-port (slurp (str port-file)) (str port-file))
          allow-missing-port? nil
          :else
            (throw
              (ex-info
                "Could not find .nrepl-port; start an nREPL server or set ZED_CLOJURE_NREPL_PORT"
                {})))
      project-config-file (find-project-config-file source-file)
      project-root (cond port-file (fs/parent port-file)
                         project-config-file (fs/parent (fs/parent
                                                          project-config-file))
                         :else (fs/absolutize "."))
      output-file
        (if-let [output (not-empty (get environment "ZED_CLOJURE_OUTPUT_FILE"))]
          (fs/absolutize output)
          (fs/path (fs/normalize (fs/absolutize ".")) ".zed" "repl.repl"))]
     {:host (or (not-empty (get environment "ZED_CLOJURE_NREPL_HOST"))
                "127.0.0.1"),
      :port port,
      :port-file port-file,
      :project-root project-root,
      :project-config-file project-config-file,
      :output-file output-file})))

(defn namespace-symbol? [value] (contains? #{'ns 'clojure.core/ns} value))

(defn namespace-declaration
  [source-file]
  (if (or (nil? source-file)
          (str/blank? (str source-file))
          (not (fs/regular-file? source-file)))
    nil
    (try (let [input (reader-types/string-push-back-reader (slurp
                                                             (str source-file)))
               features
                 (if (= "cljs" (fs/extension source-file)) #{:cljs} #{:clj})]
           (loop []
             (let [form (reader/read
                          {:eof ::eof, :read-cond :allow, :features features}
                          input)]
               (cond (= form ::eof) nil
                     (and (seq? form)
                          (namespace-symbol? (first form))
                          (symbol? (second form)))
                       {:name (str (second form)), :form (pr-str form)}
                     :else (recur)))))
         (catch Exception _ nil))))

(defn infer-namespace
  [source-file]
  (or (:name (namespace-declaration source-file)) "user"))

(defn namespace-form?
  [code]
  (try (let [form (reader/read-string {:read-cond :allow, :features #{:clj}}
                                      code)]
         (and (seq? form) (namespace-symbol? (first form))))
       (catch Exception _ false)))

(defn deep-merge
  [left right]
  (merge-with #(if (and (map? %1) (map? %2)) (deep-merge %1 %2) %2) left right))

(defn compile-lambda
  [value description]
  (when-not (and (seq? value) (= 'fn (first value)))
    (throw (ex-info (str description " must be an fn form") {})))
  (let [compiled (eval value)]
    (when-not (fn? compiled)
      (throw (ex-info (str description " must evaluate to a function") {})))
    compiled))

(defn compile-config
  [config]
  (let [targets (:targets config)]
    (when-not (or (nil? targets)
                  (and (map? targets) (every? map? (vals targets))))
      (throw (ex-info "Clojure REPL :targets must map names to target maps"
                      {})))
    (cond-> config
      (seq? (:target config))
        (update :target compile-lambda "Top-level :target")
      (seq? (:render-status-line config)) (update
                                            :render-status-line
                                            compile-lambda
                                            "Top-level :render-status-line")
      (:snippets config)
        (update :snippets
                (fn [snippets]
                  (into {}
                        (map (fn [[snippet-name snippet]]
                               [snippet-name
                                (if (seq? snippet)
                                  (compile-lambda snippet
                                                  (str "Snippet " snippet-name))
                                  snippet)]))
                        snippets))))))

(defn config-paths
  [environment {:keys [project-config-file]}]
  (if-let [explicit-path (not-empty (get environment "ZED_CLOJURE_CONFIG"))]
    [(fs/absolutize explicit-path)]
    (let [config-home (or (not-empty (get environment "XDG_CONFIG_HOME"))
                          (str (fs/path (System/getProperty "user.home")
                                        ".config")))
          user-config (fs/path config-home "zed-clojure" "repl.edn")]
      (filterv #(and % (fs/regular-file? %))
        [user-config project-config-file]))))

(defn load-config
  [environment endpoint]
  (compile-config
    (reduce (fn [config path]
              (when-not (fs/regular-file? path)
                (throw (ex-info (str "Clojure REPL config does not exist: "
                                     path)
                                {})))
              (let [loaded (try (edn/read-string (slurp (str path)))
                                (catch Exception exception
                                  (throw
                                    (ex-info
                                      (str "Could not read Clojure REPL config "
                                             path
                                           ": " (ex-message exception))
                                      {}
                                      exception))))]
                (when-not (map? loaded)
                  (throw (ex-info
                           (str "Clojure REPL config must contain a map: " path)
                           {})))
                (deep-merge config loaded)))
      {}
      (config-paths environment endpoint))))

(defn parse-position [value] (when-not (str/blank? value) (parse-long value)))

(def trivia-node-tags #{:comment :comma :newline :whitespace})

(defn position-before?
  [left-row left-column right-row right-column]
  (or (< left-row right-row)
      (and (= left-row right-row) (< left-column right-column))))

(defn node-contains-position?
  [syntax-node row column]
  (let [{start-row :row,
         start-column :col,
         end-row :end-row,
         end-column :end-col}
          (meta syntax-node)]
    (and start-row
         start-column
         end-row
         end-column
         (not (position-before? row column start-row start-column))
         (position-before? row column end-row end-column))))

(defn nearest-syntax-node
  [syntax-node row column]
  (when (node-contains-position? syntax-node row column)
    (or (when (node/inner? syntax-node)
          (some #(nearest-syntax-node % row column)
                (node/children syntax-node)))
        (when-not (or (= :forms (node/tag syntax-node))
                      (contains? trivia-node-tags (node/tag syntax-node)))
          syntax-node))))

(defn nearest-form
  [source-file row column]
  (when (and source-file row column (fs/regular-file? source-file))
    (try (some-> (parser/parse-string-all (slurp (str source-file)))
                 (nearest-syntax-node row column)
                 node/string)
         (catch Exception _ nil))))

(defn ambient-information
  [environment endpoint namespace]
  (let [top-level-form (get environment "ZED_CLOJURE_CODE")
        selection (not-empty (get environment "ZED_CLOJURE_SELECTION"))
        file (not-empty (get environment "ZED_CLOJURE_FILE"))
        line (parse-position (get environment "ZED_CLOJURE_ROW"))
        column (parse-position (get environment "ZED_CLOJURE_COLUMN"))
        nearest-form (or (nearest-form file line column) top-level-form)]
    {:form (or selection nearest-form),
     :nearest-form nearest-form,
     :top-level-form top-level-form,
     :selection selection,
     :file file,
     :filename (some-> file
                       fs/file-name
                       str),
     :file-extension (some-> file
                             fs/extension),
     :line line,
     :column column,
     :symbol (not-empty (get environment "ZED_CLOJURE_SYMBOL")),
     :namespace namespace,
     :nrepl-host (:host endpoint),
     :nrepl-port (:port endpoint),
     :nrepl-port-file (some-> (:port-file endpoint)
                              str),
     :project-root (str (:project-root endpoint))}))

(def snippet-placeholder-pattern
  #"\$(?:top-level-form|nearest-form|nrepl-port|namespace|selection|filename|column|symbol|form|file|line)(?=$|[\s,()\[\]{}\";])")

(defn expand-snippet
  [snippet ambient]
  (str/replace snippet
               snippet-placeholder-pattern
               (fn [placeholder]
                 (str (or (get ambient (keyword (subs placeholder 1))) "")))))

(defn decode-bencode
  [value]
  (cond (bytes? value) (String. ^bytes value StandardCharsets/UTF_8)
        (map? value) (into {}
                           (map (fn [[key item]] [(decode-bencode key)
                                                  (decode-bencode item)]))
                           value)
        (sequential? value) (mapv decode-bencode value)
        :else value))

(defn send-request
  ([input output request] (send-request input output request nil))
  ([input output request on-sent]
   (let [request-id (str (UUID/randomUUID))]
     (bencode/write-bencode output (assoc request "id" request-id))
     (.flush output)
     (when on-sent (on-sent))
     (loop [responses []]
       (let [response (decode-bencode (bencode/read-bencode input))
             responses (if (= request-id (get response "id"))
                         (conj responses response)
                         responses)
             statuses (set (get response "status"))]
         (if (and (= request-id (get response "id"))
                  (contains? statuses "done"))
           responses
           (recur responses)))))))

(defn clone-session
  [input output]
  (let [responses (send-request input output {"op" "clone"})
        session (some #(get % "new-session") responses)]
    (when-not session
      (throw (ex-info "nREPL server did not return a cloned session" {})))
    session))

(defn send-eval
  ([input output session code namespace]
   (send-eval input output session code namespace nil))
  ([input output session code namespace on-sent]
   (send-request input
                 output
                 {"op" "eval", "session" session, "ns" namespace, "code" code}
                 on-sent)))

(def failure-statuses
  #{"error" "eval-error" "interrupted" "namespace-not-found" "unknown-op"})

(defn failed-responses?
  [responses]
  (boolean (some (fn [response]
                   (or (get response "root-ex")
                       (get response "ex")
                       (some failure-statuses (get response "status"))))
                 responses)))

(defn close-session!
  [input output session]
  (try (send-request input output {"op" "close", "session" session})
       (catch Exception exception
         (binding [*out* *err*]
           (println "Warning: failed to close nREPL session:"
                    (ex-message exception))))))

(defn evaluate
  [{:keys [host port]} code namespace namespace-form setup-namespace? setup-code
   on-sent]
  (with-open [socket (Socket.)]
    (.connect socket (InetSocketAddress. host port) 3000)
    (.setSoTimeout socket 60000)
    (let [output (.getOutputStream socket)
          input (PushbackInputStream. (.getInputStream socket))
          session (clone-session input output)]
      (try
        (let [setup-responses
                (when-not (str/blank? setup-code)
                  (send-eval input output session setup-code "user"))]
          (if (failed-responses? setup-responses)
            setup-responses
            (if (namespace-form? code)
              (send-eval input output session code "user" on-sent)
              (let
                [namespace-responses
                   (when (and setup-namespace? (not= namespace "user"))
                     (send-eval
                       input
                       output
                       session
                       (or
                         namespace-form
                         (str
                           "(let [target-ns (create-ns '"
                           (pr-str (symbol namespace))
                           ")] (binding [*ns* target-ns] (clojure.core/refer 'clojure.core)))"))
                       "user"))
                 prior-responses (mapv #(dissoc % "value") namespace-responses)]
                (if (failed-responses? namespace-responses)
                  prior-responses
                  (into prior-responses
                        (send-eval input
                                   output
                                   session
                                   code
                                   namespace
                                   on-sent)))))))
        (finally (close-session! input output session))))))

(defn summarize-responses
  [responses]
  (let [statuses (into #{} (mapcat #(get % "status" [])) responses)
        exceptions (keep #(or (get % "root-ex") (get % "ex")) responses)]
    {:out (apply str (keep #(get % "out") responses)),
     :err (apply str (keep #(get % "err") responses)),
     :values (vec (keep #(get % "value") responses)),
     :exceptions (vec exceptions),
     :statuses statuses,
     :failed? (or (some failure-statuses statuses) (seq exceptions))}))

(defn resolve-target
  [config ambient target]
  (let [target (if (fn? target) (target ambient) target)]
    (cond
      (or (nil? target) (= target :default)) {:name nil, :options nil}
      (keyword? target)
        (if-let [configured-target (get-in config [:targets target])]
          {:name target, :options configured-target}
          (throw (ex-info (str "Unknown Clojure REPL target: " target) {})))
      (map? target) {:name nil, :options target}
      :else
        (throw
          (ex-info
            (str
              "Clojure REPL target must be a keyword, map, function, or nil: "
              (pr-str target))
            {})))))

(defn endpoint-for-target
  [default-endpoint target]
  (let [resolved
          (cond (nil? target) default-endpoint
                (map? target)
                  (let [port-file
                          (when-let [configured-port-file (:port-file target)]
                            (let [configured-port-file (fs/path
                                                         configured-port-file)]
                              (if (fs/absolute? configured-port-file)
                                configured-port-file
                                (fs/path (:project-root default-endpoint)
                                         configured-port-file))))
                        port (cond (contains? target :port)
                                     (parse-port (str (:port target))
                                                 "target port")
                                   port-file (parse-port (slurp (str port-file))
                                                         (str port-file))
                                   :else (:port default-endpoint))]
                    (cond-> (merge default-endpoint
                                   (select-keys target [:host]))
                      true (assoc :port port)
                      port-file (assoc :port-file port-file)))
                :else (throw (ex-info (str "Unsupported Clojure REPL target: "
                                           (pr-str target))
                                      {})))]
    (when-not (:port resolved)
      (throw (ex-info "No nREPL port was discovered or supplied by the target"
                      {})))
    resolved))

(defn combined-result
  [results]
  {:out (apply str (map :out results)),
   :err (apply str (map :err results)),
   :values (vec (mapcat :values results)),
   :exceptions (vec (mapcat :exceptions results)),
   :statuses (into #{} (mapcat :statuses) results),
   :failed? (boolean (some :failed? results))})

(defn commented
  [text]
  (->> (str/split-lines (or text ""))
       (map #(str ";; " %))
       (str/join "\n")))

(def local-time-formatter (DateTimeFormatter/ofPattern "z dd HH:mm:ss"))

(defn local-time
  [instant]
  (.format local-time-formatter (.atZone instant (ZoneId/systemDefault))))

(defn format-code
  [config code]
  (if-let [{:keys [command arguments]} (:formatter config)]
    (if (str/blank? command)
      {:code code, :warning "Formatter command is empty"}
      (try (let [{:keys [exit out err]}
                   (apply process/shell
                     {:in code, :out :string, :err :string, :continue true}
                     command
                     arguments)]
             (if (and (zero? exit) (not (str/blank? out)))
               {:code (str/trimr out)}
               {:code code,
                :warning (str "Formatter exited with status "
                              exit
                              (when-not (str/blank? err)
                                (str ": " (str/trim err))))}))
           (catch Exception exception
             {:code code,
              :warning (str "Formatter failed: " (ex-message exception))})))
    {:code code}))

(defn format-values
  [config values]
  (reduce-kv
    (fn [formatted index value]
      (let [{formatted-value :code, :keys [warning]} (format-code config value)]
        (cond-> (update formatted :values conj formatted-value)
          warning
            (update :warnings conj (str "Result " (inc index) ": " warning)))))
    {:values [], :warnings []}
    (vec values)))

(defn target-display
  [{:keys [target-name endpoint]}]
  (or target-name (:port endpoint)))

(defn status-context
  [ambient evaluation timestamp phase]
  (merge ambient
         {:target (target-display evaluation),
          :time (local-time timestamp),
          :namespace (:namespace evaluation),
          :state-map (:state-map ambient),
          :phase phase,
          :result (:result evaluation)}))

(defn render-status-line
  [config ambient evaluation timestamp phase]
  (let [context (status-context ambient evaluation timestamp phase)
        rendered (if-let [renderer (:render-status-line config)]
                   (renderer context)
                   (str "Status: "
                        (name phase)
                        ". Target: "
                        (pr-str (:target context))
                        "."))]
    (when-not (or (nil? rendered) (string? rendered))
      (throw (ex-info
               "Top-level :render-status-line must return a string or nil"
               {})))
    (when (and rendered (re-find #"[\r\n]" rendered))
      (throw (ex-info "Top-level :render-status-line must return a single line"
                      {})))
    rendered))

(defn evaluation-marker
  [{:keys [context evaluated-at], :as evaluation}]
  (let [context (status-context context evaluation evaluated-at :resolved)]
    (str ";; Target: "
         (pr-str (:target context))
         ". Time: "
         (:time context)
         ". Evaluated in: "
         (:namespace context)
         ". Source file: "
         (or (:file context) "<buffer>")
         (when-let [line (:line context)] (str ":" line))
         "\n")))

(defn invisible-evaluation-id
  []
  (str \u2064
       (apply str
         (map (fn [character]
                (char (+ 0xFE00 (Character/digit ^char character 16))))
           (remove #{\-} (str (UUID/randomUUID)))))))

(defn format-entry
  [{:keys [displayed-code displayed-values format-warning result-format-warning
           result],
    :as evaluation}]
  (let [{:keys [out err values exceptions statuses]} result
        values (or displayed-values values)]
    (str (evaluation-marker evaluation)
         displayed-code
         "\n"
         (when-not (str/blank? format-warning)
           (str (commented format-warning) "\n"))
         (when-not (str/blank? out) (str ";; stdout\n" (commented out) "\n"))
         (when-not (str/blank? err) (str ";; stderr\n" (commented err) "\n"))
         (apply str (map #(str "#_=>\n" % "\n") values))
         (when-not (str/blank? result-format-warning)
           (str (commented result-format-warning) "\n"))
         (apply str (map #(str ";; exception: " % "\n") exceptions))
         (when (some failure-statuses statuses)
           (str ";; nREPL status: " (str/join ", " (sort statuses)) "\n"))
         "\n")))

(defn format-pending-entry
  [{:keys [displayed-code format-warning pending-id], :as evaluation}]
  (str (evaluation-marker evaluation)
       displayed-code
       "\n"
       (when-not (str/blank? format-warning)
         (str (commented format-warning) "\n"))
       "#_=>\n"
       "#_:WAITING"
       pending-id
       "\n\n"))

(defn atomic-update-file!
  [path update-content]
  (let [path (.toAbsolutePath (fs/path path))
        parent (.getParent path)
        lock-path (.resolve parent (str (.getFileName path) ".lock"))]
    (fs/create-dirs parent)
    (with-open [lock-file (RandomAccessFile. (.toFile lock-path) "rw")
                channel (.getChannel lock-file)]
      (let [file-lock (.lock channel)
            temporary (Files/createTempFile parent
                                            ".repl."
                                            ".tmp"
                                            (make-array FileAttribute 0))]
        (try (let [existing (if (Files/exists
                                  path
                                  (make-array java.nio.file.LinkOption 0))
                              (slurp (str path))
                              "")
                   content (update-content existing)]
               (spit (str temporary) content))
             (try (Files/move temporary
                              path
                              (into-array
                                StandardCopyOption
                                [StandardCopyOption/ATOMIC_MOVE
                                 StandardCopyOption/REPLACE_EXISTING]))
                  (catch Exception _
                    (Files/move temporary
                                path
                                (into-array
                                  StandardCopyOption
                                  [StandardCopyOption/REPLACE_EXISTING]))))
             (finally (fs/delete-if-exists temporary) file-lock))))))

(defn atomic-append! [path content] (atomic-update-file! path #(str % content)))

(defn atomic-write!
  [path content]
  (atomic-update-file! path (constantly content)))

(def status-line-prefix (str ";;" \u2063 " "))

(def status-line-pattern
  (re-pattern
    (str "\\A" (Pattern/quote status-line-prefix) "[^\\r\\n]*(?:\\r?\\n)?")))

(def legacy-status-footer-pattern
  (re-pattern (str "(?m)^"
                   (Pattern/quote status-line-prefix)
                   "[^\\r\\n]*(?:\\r?\\n)?\\z")))

(defn remove-status-line
  [content]
  (-> content
      (str/replace status-line-pattern "")
      (str/replace legacy-status-footer-pattern "")))

(defn transform-output!
  [path transform-body status-line]
  (atomic-update-file!
    path
    (fn [existing]
      (let [body (transform-body (remove-status-line existing))
            body (if (or (str/blank? body) (str/ends-with? body "\n"))
                   body
                   (str body "\n"))]
        (if (str/blank? status-line)
          body
          (str status-line-prefix status-line "\n" body))))))

(defn update-output!
  [path new-content status-line]
  (transform-output! path #(str new-content %) status-line))

(defn replace-output-entry!
  [path old-entry new-entry status-line]
  (transform-output! path
                     (fn [body]
                       (if-let [index (str/index-of body old-entry)]
                         (str (subs body 0 index)
                              new-entry
                              (subs body (+ index (count old-entry))))
                         (str new-entry body)))
                     status-line))

(defn state-file
  [config endpoint]
  (let [configured-path (or (:state-file config)
                            (fs/path ".zed" "clojure-repl-state.edn"))
        configured-path (fs/path configured-path)]
    (if (fs/absolute? configured-path)
      configured-path
      (fs/path (:project-root endpoint) configured-path))))

(defn load-state-map
  [path]
  (if (fs/regular-file? path)
    (let [state-map (try (edn/read-string (slurp (str path)))
                         (catch Exception exception
                           (throw (ex-info
                                    (str "Could not read Clojure REPL state "
                                           path
                                         ": " (ex-message exception))
                                    {}
                                    exception))))]
      (when-not (map? state-map)
        (throw (ex-info (str "Clojure REPL state must contain a map: " path)
                        {})))
      state-map)
    {}))

(defn serialize-state-map
  [state-map]
  (when-not (map? state-map)
    (throw (ex-info "set-state-map! requires a map" {})))
  (let [content (str (pr-str state-map) "\n")
        parsed (try (edn/read-string content)
                    (catch Exception exception
                      (throw (ex-info
                               (str
                                 "set-state-map! requires EDN-readable values: "
                                 (ex-message exception))
                               {}
                               exception))))]
    (when-not (= state-map parsed)
      (throw (ex-info
               "set-state-map! must round-trip through EDN without changes"
               {})))
    content))

(defn snippet-by-name
  [config snippet-name]
  (or (get-in config [:snippets snippet-name])
      (get-in config [:snippets (keyword snippet-name)])))

(defn invoke-snippet!
  [snippet context evaluate-against-port set-state-map!]
  (cond (string? snippet) (evaluate-against-port (expand-snippet snippet
                                                                 context))
        (fn? snippet) (snippet context evaluate-against-port set-state-map!)
        :else (throw (ex-info "Snippet must be a string or an fn form" {}))))

(defn run
  [environment]
  (let [code (get environment "ZED_CLOJURE_CODE")
        file (get environment "ZED_CLOJURE_FILE")
        snippet-name (not-empty (get environment "ZED_CLOJURE_SNIPPET"))]
    (when (str/blank? code)
      (throw
        (ex-info
          "ZED_CLOJURE_CODE is empty; invoke the task from a runnable Clojure form"
          {})))
    (let [endpoint (endpoint environment true)
          config (load-config environment endpoint)
          namespace-declaration (namespace-declaration file)
          namespace (or (:name namespace-declaration) "user")
          ambient (ambient-information environment endpoint namespace)
          state-file (state-file config endpoint)
          state-map (atom (load-state-map state-file))
          latest-status-line (atom ::not-rendered)
          status-errors (atom #{})
          evaluations (atom [])]
      (letfn
        [(current-context [] (assoc ambient :state-map @state-map))
         (record-status-error! [phase description exception]
           (let [message (str description
                              " during " (name phase)
                              ": " (str/replace (or (ex-message exception)
                                                    (str exception))
                                                #"\s+"
                                                " "))]
             (swap! status-errors conj message)
             message))
         (status-evaluation []
           (let [{:keys [name options]}
                   (resolve-target config (current-context) (:target config))]
             {:target-name name,
              :endpoint (when-not name (endpoint-for-target endpoint options)),
              :namespace (or (:namespace options) namespace)}))
         (render-status [phase evaluation]
           (let [status-line (try (render-status-line config
                                                      (current-context)
                                                      evaluation
                                                      (Instant/now)
                                                      phase)
                                  (catch Exception exception
                                    (record-status-error!
                                      phase
                                      "Status renderer failed"
                                      exception)))]
             (reset! latest-status-line status-line)
             status-line))
         (render-status! [phase evaluation]
           (let [status-line (render-status phase evaluation)]
             (update-output! (:output-file endpoint) "" status-line)))
         (set-state-map! [new-state-map]
           (atomic-write! state-file (serialize-state-map new-state-map))
           (reset! state-map new-state-map)
           (render-status! :state-changed (status-evaluation)))
         (evaluate-against-port
           ([snippet-code]
            (evaluate-against-port (:target config) snippet-code))
           ([target snippet-code]
            (when (str/blank? snippet-code)
              (throw (ex-info "Cannot evaluate blank Clojure code" {})))
            (let [{:keys [name options]}
                    (resolve-target config (current-context) target)
                  target-endpoint (endpoint-for-target endpoint options)
                  target-options (or options {})
                  target-namespace (or (:namespace target-options) namespace)
                  target-namespace-form
                    (cond (contains? target-options :namespace-form)
                            (:namespace-form target-options)
                          (contains? target-options :namespace) nil
                          :else (:form namespace-declaration))
                  setup-namespace? (get target-options :setup-namespace? true)
                  setup-code (:setup-code target-options)
                  {displayed-code :code, :keys [warning]}
                    (format-code config snippet-code)
                  evaluation {:code snippet-code,
                              :context (current-context),
                              :displayed-code displayed-code,
                              :endpoint target-endpoint,
                              :evaluated-at (Instant/now),
                              :format-warning warning,
                              :pending-id (invisible-evaluation-id),
                              :target-name name,
                              :namespace target-namespace}
                  pending-entry (format-pending-entry evaluation)
                  opened-status (render-status :opened evaluation)
                  _ (update-output! (:output-file endpoint)
                                    pending-entry
                                    opened-status)
                  result (try (summarize-responses
                                (evaluate target-endpoint
                                          snippet-code
                                          target-namespace
                                          target-namespace-form
                                          setup-namespace?
                                          setup-code
                                          #(try (render-status! :sent
                                                                evaluation)
                                                (catch Exception exception
                                                  (record-status-error!
                                                    :sent
                                                    "Status update failed"
                                                    exception)))))
                              (catch Exception exception
                                {:out "",
                                 :err "",
                                 :values [],
                                 :exceptions [(ex-message exception)],
                                 :statuses #{"connection-error"},
                                 :failed? true}))
                  {displayed-values :values, result-warnings :warnings}
                    (format-values config (:values result))
                  evaluation (assoc evaluation
                               :displayed-values displayed-values
                               :result result
                               :result-format-warning
                                 (when (seq result-warnings)
                                   (str/join "\n" result-warnings)))]
              (swap! evaluations conj evaluation)
              (let [resolved-status (render-status :resolved evaluation)]
                (replace-output-entry! (:output-file endpoint)
                                       pending-entry
                                       (format-entry evaluation)
                                       resolved-status))
              result)))]
        (if snippet-name
          (let [snippet (snippet-by-name config snippet-name)]
            (when-not snippet
              (throw (ex-info (str "Unknown Clojure REPL snippet: "
                                   snippet-name)
                              {})))
            (invoke-snippet! snippet
                             (current-context)
                             evaluate-against-port
                             set-state-map!))
          (evaluate-against-port code))
        (let [result (combined-result (map :result @evaluations))
              result (if (seq @status-errors)
                       (-> result
                           (update :exceptions into (sort @status-errors))
                           (update :statuses conj "status-error")
                           (assoc :failed? true))
                       result)]
          (when (= ::not-rendered @latest-status-line)
            (render-status! :resolved (status-evaluation)))
          {:result result, :output-file (:output-file endpoint)})))))

(defn -main
  [& _]
  (try (let [{:keys [result output-file]} (run (System/getenv))]
         (if (:failed? result)
           (do (binding [*out* *err*]
                 (println "Clojure evaluation failed; output written to"
                          (str output-file))
                 (doseq [exception (:exceptions result)] (println exception))
                 (when-not (str/blank? (:err result)) (print (:err result))))
               (System/exit 1))
           (println "Clojure evaluation output written to" (str output-file))))
       (catch Exception exception
         (binding [*out* *err*]
           (println "Clojure evaluation failed:" (ex-message exception)))
         (System/exit 1))))
