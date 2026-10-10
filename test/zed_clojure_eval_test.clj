(ns zed-clojure-eval-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]
            [zed-clojure-eval :as evaluator])
  (:import [java.time Instant]))

(deftest parses-ports
  (is (= 7888 (evaluator/parse-port " 7888\n" "test port")))
  (is (thrown? Exception (evaluator/parse-port "0" "test port")))
  (is (thrown? Exception (evaluator/parse-port "not-a-port" "test port"))))

(deftest infers-namespace
  (fs/with-temp-dir
    [directory {}]
    (let [source (fs/path directory "core.clj")]
      (spit
        (str source)
        ";; comment\n(ns ^:no-doc example.core\n  (:require [clojure.string :as str]))\n(+ 1 2)")
      (is (= "example.core" (evaluator/infer-namespace source)))
      (is (= '(ns example.core (:require [clojure.string :as str]))
             (read-string (:form (evaluator/namespace-declaration source)))))))
  (is (= "user" (evaluator/infer-namespace "/does/not/exist.clj"))))

(deftest recognizes-namespace-forms
  (is (evaluator/namespace-form? "(ns example.core)"))
  (is (evaluator/namespace-form? "(clojure.core/ns example.core)"))
  (is (not (evaluator/namespace-form? "(def answer 42)"))))

(deftest builds-ambient-information
  (let [endpoint {:host "127.0.0.1",
                  :port 7888,
                  :port-file (fs/path "/project/.nrepl-port"),
                  :project-root (fs/path "/project")}
        environment {"ZED_CLOJURE_CODE" "(+ 1 2)",
                     "ZED_CLOJURE_SELECTION" "selected",
                     "ZED_CLOJURE_FILE" "/project/src/example/core.cljs",
                     "ZED_CLOJURE_ROW" "12",
                     "ZED_CLOJURE_COLUMN" "4",
                     "ZED_CLOJURE_SYMBOL" "example"}
        ambient
          (evaluator/ambient-information environment endpoint "example.core")]
    (is (= "selected" (:form ambient)))
    (is (= "(+ 1 2)" (:top-level-form ambient)))
    (is (= "core.cljs" (:filename ambient)))
    (is (= "cljs" (:file-extension ambient)))
    (is (= 12 (:line ambient)))
    (is (= 4 (:column ambient)))
    (is (= 7888 (:nrepl-port ambient)))))

(deftest finds-the-nearest-syntax-form
  (fs/with-temp-dir
    [directory {}]
    (let [source (fs/path directory "core.clj")]
      (spit (str source)
            "(ns example.core)\n(defn add-one [value]\n  (+ value 1))\n")
      (is (= "(+ value 1)" (evaluator/nearest-form source 3 3)))
      (is (= "value" (evaluator/nearest-form source 3 6))))))

(deftest expands-snippet-placeholders
  (is (= "(tap> (+ 1 2)) ;; 7888"
         (evaluator/expand-snippet "(tap> $top-level-form) ;; $nrepl-port"
                                   {:top-level-form "(+ 1 2)",
                                    :nrepl-port 7888})))
  (is (= "(identity $line)"
         (evaluator/expand-snippet "$form"
                                   {:form "(identity $line)", :line 42})))
  (is (= "$formula"
         (evaluator/expand-snippet "$formula" {:form "replacement"}))))

(deftest invokes-string-and-function-snippets
  (let [calls (atom [])
        states (atom [])
        evaluate! (fn
                    ([code] (swap! calls conj [:default code]))
                    ([target code] (swap! calls conj [target code])))
        set-state-map! #(swap! states conj %)
        context {:form "(+ 1 2)", :top-level-form "(+ 1 2)", :nrepl-port 7888}]
    (evaluator/invoke-snippet! "(tap> $form)" context evaluate! set-state-map!)
    (evaluator/invoke-snippet!
      (fn [{:keys [form nrepl-port]} evaluate-against-port set-state-map!]
        (set-state-map! {:target :node})
        (evaluate-against-port {:port nrepl-port} (str "(identity " form ")")))
      context
      evaluate!
      set-state-map!)
    (is (= [[:default "(tap> (+ 1 2))"] [{:port 7888} "(identity (+ 1 2))"]]
           @calls))
    (is (= [{:target :node}] @states))))

(deftest resolves-fixed-targets
  (let [config {:targets {:jvm {:port 9000}}}
        named (evaluator/resolve-target config {} :jvm)
        anonymous (evaluator/resolve-target config {} {:port 9001})
        automatic (evaluator/resolve-target
                    config
                    {:file "example.cljs"}
                    (fn [{:keys [file]}]
                      (if (.endsWith file ".cljs") :jvm nil)))]
    (is (= {:name :jvm, :options {:port 9000}} named))
    (is (= {:name nil, :options {:port 9001}} anonymous))
    (is (= named automatic))
    (is (= 9000
           (:port (evaluator/endpoint-for-target {:host "127.0.0.1", :port 7888}
                                                 (:options named)))))))

(deftest loads-and-merges-edn-config
  (fs/with-temp-dir
    [directory {}]
    (let [config-home (fs/path directory "config")
          user-config (fs/path config-home "zed-clojure" "repl.edn")
          project-config
            (fs/path directory "project" ".zed" "clojure-repl.edn")]
      (fs/create-dirs (fs/parent user-config))
      (fs/create-dirs (fs/parent project-config))
      (spit
        (str user-config)
        (pr-str
          '{:targets {:jvm {:host "127.0.0.1", :port 9000}, :node {:port 9001}},
            :target
              (fn [{:keys [file]}] (if (.endsWith file ".cljs") :node :jvm)),
            :snippets
              {:custom (fn
                        [{:keys [form]} evaluate-against-port _set-state-map!]
                        (evaluate-against-port (str "(identity " form ")")))}}))
      (spit (str project-config) "{:targets {:jvm {:port 9002}}}")
      (let [config (evaluator/load-config {"XDG_CONFIG_HOME" (str config-home)}
                                          {:project-config-file project-config})
            calls (atom [])]
        (is (= {:host "127.0.0.1", :port 9002} (get-in config [:targets :jvm])))
        (is (= :node ((:target config) {:file "example.cljs"})))
        ((get-in config [:snippets :custom])
          {:form "(+ 1 2)"}
          #(swap! calls conj %)
          (fn [_]))
        (is (= ["(identity (+ 1 2))"] @calls))))))

(deftest rejects-invalid-executable-config
  (is (thrown-with-msg? Exception
                        #"Top-level :target must be an fn form"
                        (evaluator/compile-config
                          {:target '(do (println "unexpected") :jvm)})))
  (is (thrown-with-msg? Exception
                        #"Snippet :custom must be an fn form"
                        (evaluator/compile-config
                          {:snippets {:custom '(identity "unexpected")}})))
  (is (thrown-with-msg? Exception
                        #":targets must map names to target maps"
                        (evaluator/compile-config
                          {:targets {:dynamic '(fn [_] :jvm)}}))))

(deftest permits-a-target-without-a-default-port
  (fs/with-temp-dir
    [directory {}]
    (let [endpoint (evaluator/endpoint {"ZED_CLOJURE_FILE"
                                          (str (fs/path directory "core.clj")),
                                        "ZED_CLOJURE_OUTPUT_FILE"
                                          (str (fs/path directory "repl.repl"))}
                                       true)]
      (is (nil? (:port endpoint)))
      (is (= 9000
             (:port (evaluator/endpoint-for-target endpoint {:port 9000})))))))

(deftest recognizes-failed-namespace-responses
  (is (evaluator/failed-responses? [{"status" ["eval-error"],
                                     "ex" "missing dependency"}]))
  (is (evaluator/failed-responses? [{"status" ["interrupted"]}]))
  (is (not (evaluator/failed-responses? [{"status" ["done"]}]))))

(deftest finds-port-and-config-files-in-an-ancestor
  (fs/with-temp-dir
    [directory {}]
    (let [source (fs/path directory "src" "example" "core.clj")
          port-file (fs/path directory ".nrepl-port")
          config-file (fs/path directory ".zed" "clojure-repl.edn")]
      (fs/create-dirs (fs/parent source))
      (fs/create-dirs (fs/parent config-file))
      (spit (str source) "(ns example.core)")
      (spit (str port-file) "7888")
      (spit (str config-file) "{}")
      (is (= (str port-file) (str (evaluator/find-port-file (str source)))))
      (is (= (str config-file)
             (str (evaluator/find-project-config-file (str source))))))))

(deftest updates-output-and-state-atomically
  (fs/with-temp-dir
    [directory {}]
    (let [output (fs/path directory ".zed" "repl.repl")
          state-file (fs/path directory ".zed" "clojure-repl-state.edn")]
      (evaluator/atomic-append! output "first\n")
      (evaluator/atomic-append! output "second\n")
      (is (= "first\nsecond\n" (slurp (str output))))
      (is (= {} (evaluator/load-state-map state-file)))
      (evaluator/atomic-write! state-file "{:target :node}\n")
      (is (= {:target :node} (evaluator/load-state-map state-file)))
      (is (thrown-with-msg? Exception
                            #"EDN-readable values"
                            (evaluator/serialize-state-map {:callback
                                                              (fn [])}))))))

(deftest formats-code-with-a-command
  (is (= "ABC"
         (:code (evaluator/format-code {:formatter {:command "tr",
                                                    :arguments ["a-z" "A-Z"]}}
                                       "abc"))))
  (is (= "abc"
         (:code (evaluator/format-code {:formatter {:command "does-not-exist"}}
                                       "abc")))))

(deftest renders-a-status-line
  (let [config {:render-status-line
                  (fn [{:keys [target namespace state-map phase]}]
                    (str target " " namespace " " (:mode state-map) " " phase))}
        status (evaluator/render-status-line
                 config
                 {:namespace "example.core", :state-map {:mode :dev}}
                 {:target-name :jvm,
                  :endpoint {:port 7888},
                  :namespace "example.core"}
                 (Instant/parse "2026-01-01T00:00:00Z")
                 :resolved)]
    (is (= ":jvm example.core :dev :resolved" status))
    (is (= "9001 example.core :dev :sent"
           (evaluator/render-status-line
             config
             {:namespace "example.core", :state-map {:mode :dev}}
             {:endpoint {:port 9001}, :namespace "example.core"}
             (Instant/parse "2026-01-01T00:00:00Z")
             :sent)))))

(deftest keeps-a-single-status-line-at-the-top
  (fs/with-temp-dir
    [directory {}]
    (let [output (fs/path directory "repl.repl")]
      (evaluator/update-output! output "(+ 1 2)\n#_=>\n3\n\n" "first")
      (evaluator/update-output! output "(+ 2 3)\n#_=>\n5\n\n" "second")
      (let [content (slurp (str output))]
        (is (str/includes? content "(+ 1 2)"))
        (is (str/includes? content "(+ 2 3)"))
        (is (< (str/index-of content "(+ 2 3)")
               (str/index-of content "(+ 1 2)")))
        (is (not (str/includes? content "first")))
        (is (= 1
               (count (re-seq (re-pattern (java.util.regex.Pattern/quote
                                            evaluator/status-line-prefix))
                              content))))
        (is (str/starts-with? content
                              (str evaluator/status-line-prefix
                                   "second\n")))))))

(deftest migrates-the-managed-footer-to-the-top
  (fs/with-temp-dir
    [directory {}]
    (let [output (fs/path directory "repl.repl")]
      (spit (str output)
            (str "(+ 1 2)\n#_=>\n3\n\n" evaluator/status-line-prefix "old\n"))
      (evaluator/update-output! output "" "new")
      (let [content (slurp (str output))]
        (is (str/starts-with? content
                              (str evaluator/status-line-prefix "new\n")))
        (is (= 1 (count (re-seq #"\u2063" content))))
        (is (str/includes? content "(+ 1 2)"))))))

(deftest replaces-only-the-resolved-pending-entry
  (fs/with-temp-dir
    [directory {}]
    (let [output (fs/path directory "repl.repl")
          first-pending "first\n#_=>\n#_:WAITING\n\n"
          second-pending "second\n#_=>\n#_:WAITING\n\n"]
      (evaluator/update-output! output first-pending "opened first")
      (evaluator/update-output! output second-pending "opened second")
      (evaluator/replace-output-entry! output
                                       first-pending
                                       "first\n#_=>\n1\n\n"
                                       "resolved first")
      (let [content (slurp (str output))]
        (is (str/includes? content second-pending))
        (is (str/includes? content "first\n#_=>\n1"))
        (is (not (str/includes? content first-pending))))
      (evaluator/replace-output-entry! output
                                       second-pending
                                       "second\n#_=>\n2\n\n"
                                       "resolved second")
      (let [content (slurp (str output))]
        (is (not (str/includes? content "#_:WAITING")))
        (is (str/starts-with? content
                              (str evaluator/status-line-prefix
                                   "resolved second\n")))))))

(deftest formats-a-pending-entry
  (let [pending-id (evaluator/invisible-evaluation-id)
        entry (evaluator/format-pending-entry
                {:context {:file "/tmp/core.clj", :line 8},
                 :displayed-code "(slow-operation)",
                 :endpoint {:port 7888},
                 :evaluated-at (Instant/parse "2026-01-01T00:00:00Z"),
                 :namespace "example.core",
                 :pending-id pending-id,
                 :target-name :jvm})]
    (is (str/starts-with? entry ";; Target: :jvm."))
    (is (str/includes? entry "(slow-operation)\n#_=>\n#_:WAITING"))
    (is (str/includes? entry pending-id))
    (is (not= pending-id (evaluator/invisible-evaluation-id)))))

(deftest formats-an-entry
  (let [entry (evaluator/format-entry
                {:context {:file "/tmp/core.clj", :line 8},
                 :displayed-code "(+ 1 2)",
                 :endpoint {:port 7888},
                 :evaluated-at (Instant/parse "2026-01-01T00:00:00Z"),
                 :namespace "example.core",
                 :result {:out "hello\n",
                          :err "",
                          :values ["3"],
                          :exceptions [],
                          :statuses #{"done"}},
                 :target-name :jvm})]
    (is (str/starts-with? entry ";; Target: :jvm."))
    (is (re-find #"Evaluated in: example\.core" entry))
    (is (re-find #"\(\+ 1 2\)" entry))
    (is (re-find #";; hello" entry))
    (is (re-find #"#_=>\n3" entry))))

(deftest comments-every-formatter-warning-line
  (let [evaluation {:context {:file "/tmp/core.clj", :line 8},
                    :displayed-code "(+ 1 2)",
                    :endpoint {:port 7888},
                    :evaluated-at (Instant/parse "2026-01-01T00:00:00Z"),
                    :format-warning "first\nsecond",
                    :namespace "example.core",
                    :pending-id (evaluator/invisible-evaluation-id),
                    :result {:out "",
                             :err "",
                             :values ["3"],
                             :exceptions [],
                             :statuses #{"done"}},
                    :target-name :jvm}]
    (is (str/includes? (evaluator/format-pending-entry evaluation)
                       ";; first\n;; second"))
    (is (str/includes? (evaluator/format-entry evaluation)
                       ";; first\n;; second"))))

(defn -main
  [& _]
  (let [{:keys [fail error]} (run-tests 'zed-clojure-eval-test)]
    (when (pos? (+ fail error)) (System/exit 1))))
