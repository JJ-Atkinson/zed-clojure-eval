# zed-clojure-eval

`zed-clojure-eval` is a [bbin](https://github.com/babashka/bbin) command that
evaluates Clojure forms through nREPL and writes editor-friendly results to
`.zed/repl.repl`.

It is designed for the official Zed Clojure extension, but remains a separate
executable rather than running inside the extension's WebAssembly sandbox.

## Install

Install [Babashka](https://babashka.org/) and bbin, then run:

```sh
bbin install https://github.com/JJ-Atkinson/zed-clojure-eval.git --latest-sha
```

This installs `zed-clojure-eval` into the bbin executable directory, normally
`~/.local/bin`. Ensure that directory is on the environment `PATH` inherited by
Zed.

For local development:

```sh
bbin install .
```

## Editor integration

The evaluator receives source context through environment variables:

| Variable | Purpose |
| --- | --- |
| `ZED_CLOJURE_CODE` | form to evaluate |
| `ZED_CLOJURE_FILE` | current source file |
| `ZED_CLOJURE_ROW` | one-based source row |
| `ZED_CLOJURE_COLUMN` | one-based source column |
| `ZED_CLOJURE_SELECTION` | optional selected text |
| `ZED_CLOJURE_SYMBOL` | optional symbol under the cursor |
| `ZED_CLOJURE_SNIPPET` | optional configured snippet name |
| `ZED_CLOJURE_NREPL_HOST` | nREPL host; defaults to `127.0.0.1` |
| `ZED_CLOJURE_NREPL_PORT` | explicit nREPL port |
| `ZED_CLOJURE_NREPL_PORT_FILE` | explicit port-file path |
| `ZED_CLOJURE_OUTPUT_FILE` | output path |
| `ZED_CLOJURE_CONFIG` | explicit configuration path |

When no port override is supplied, the evaluator searches the source file's
ancestors for `.nrepl-port`. Output defaults to `$PWD/.zed/repl.repl`, independent
of where the port file was discovered.

The Zed Clojure extension supplies these values through its
`Clojure: Evaluate form` runnable task.

## Configuration

Configuration is read from `~/.config/zed-clojure/repl.edn` and
`.zed/clojure-repl.edn`. Values are recursively merged, with project values
winning.

```clojure
{:targets
 {:jvm {:port-file ".nrepl-port"}
  :node {:port-file ".shadow-cljs/nrepl.port"
         :setup-code "(shadow.cljs.devtools.api/nrepl-select :node)"}}

 :target
 (fn [{:keys [file state-map]}]
   (if (and file (.endsWith file ".cljs"))
     :node
     (or (:target state-map) :jvm)))

 :formatter
 {:command "zprint"
  :arguments []}

 :update-state-after-evaluation
 (fn [{:keys [state-map target]}]
   (assoc state-map :last-target target))

 :render-status-line
 (fn [{:keys [phase target]}]
   (str "Status: " (name phase)
        ". Target: " (pr-str target)
        "."))

 :snippets
 {:tap "(tap> $form)"
  :load-file
  (fn [{:keys [file]} evaluate-against-port _set-state-map!]
    (evaluate-against-port (slurp file)))
  :use-node
  (fn [{:keys [state-map]} _evaluate-against-port set-state-map!]
    (set-state-map! (assoc state-map :target :node)))}}}
```

Only use project configuration from code you trust. Literal functions in
designated configuration positions execute as Babashka code.

### Targets

`:targets` maps names to fixed target maps. Target maps support `:host`,
`:port`, `:port-file`, `:namespace`, `:namespace-form`,
`:setup-namespace?`, `:setup-code`, and `:display-name`. A display name changes
the target shown in status and evaluation markers without changing routing.

The top-level `:target` is a target keyword, an anonymous target map, a literal
function receiving the context map, or `nil`. When omitted or `nil`, normal
`.nrepl-port` discovery is used.

Each evaluation callback gets a new cloned nREPL session. Target setup,
namespace setup, and requested code run in that session before it is closed.

### Snippets

A snippet is either a string or a literal function. String snippets are
expanded and evaluated against the top-level target.

Function snippets receive:

1. The context map.
2. An `evaluate-against-port` callback.
3. A `set-state-map!` callback.

`evaluate-against-port` supports `(evaluate-against-port code)` and
`(evaluate-against-port target code)`. Function return values are ignored.

String snippets support `$form`, `$selection`, `$nearest-form`,
`$top-level-form`, `$file`, `$filename`, `$line`, `$column`, `$symbol`,
`$namespace`, and `$nrepl-port`.

### Context and state

The context map contains:

- `:form`, `:selection`, `:nearest-form`, and `:top-level-form`;
- `:file`, `:filename`, `:file-extension`, `:line`, `:column`, and `:symbol`;
- `:namespace`, `:project-root`, `:nrepl-host`, `:nrepl-port`, and
  `:nrepl-port-file`;
- `:state-map`.

`set-state-map!` atomically persists a complete replacement map. State defaults
to `.zed/clojure-repl-state.edn`; top-level `:state-file` can override it. State
must round-trip through `clojure.edn`.

The optional top-level `:update-state-after-evaluation` function receives the
status context after each nREPL evaluation. It must return the complete next
state map. The context includes `:target`, `:result`, and all normal context
fields. State-update failures preserve the evaluation output and report an
error.

### Output

The newest evaluation appears immediately after the managed first-line status:

```clojure
;;⁣ Status: resolved. Target: :jvm.
;; Target: :jvm. Time: CDT 10 14:47:15. Evaluated in: example.core. Source file: /project/src/example/core.clj:8
(+ 1 2)
#_=>
3
```

While waiting for nREPL:

```clojure
;;⁣ Status: sent. Target: :jvm.
;; Target: :jvm. Time: CDT 10 14:47:15. Evaluated in: example.core. Source file: /project/src/example/core.clj:8
(slow-operation)
#_=>
#_:WAITING
```

The managed status receives `:phase`, `:target`, `:time`, `:namespace`,
`:state-map`, and the optional `:result`. Phases are `:opened`, `:sent`,
`:resolved`, and `:state-changed`.

The formatter receives the original source and each returned nREPL value on
stdin in separate invocations. It changes display text only: the unformatted
source is always sent to nREPL, and formatter failures preserve the unformatted
value with a warning. Standard output and standard error are not formatted.

Ignore these generated files in projects:

```gitignore
/.zed/repl.repl*
/.zed/clojure-repl-state.edn*
```

## Development

Run the test suite with:

```sh
bb -cp src:test -m zed-clojure-eval-test
```

Check formatting with:

```sh
zprint -c src/zed_clojure_eval.clj test/zed_clojure_eval_test.clj bb.edn deps.edn
```
