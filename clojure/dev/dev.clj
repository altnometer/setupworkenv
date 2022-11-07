;; credit to https://github.com/seancorfield/dot-clojure /tmp/dot-clojure
(ns dev
  "Invoked via load-file from ~/.clojure/deps.edn, this
  file looks at what tooling you have available on your
  classpath and starts a REPL."
  (:require [clojure.repl :refer [demunge]]
            [clojure.string :as str]
            [clojure.tools.deps.alpha.repl :refer [add-libs]]))

;; hot-load a library with
;; (add-libs '{some/library {:mvn/version "RELEASE"}})

;; credit to https://github.com/seancorfield/dot-clojure

;; see if Rebel Readline is available so we can use when-sym:
(try (require 'rebel-readline.core) (catch Throwable _))

;; see if Figwheel is available so we can use when-sym:
(try (require 'figwheel.main) (catch Throwable _))

(when-not (resolve 'requiring-resolve)
  (throw (ex-info ":dev/repl and dev.clj require at least Clojure 1.10"
                  *clojure-version*)))

(defmacro when-sym
  "Usage: (when-sym some/thing (some/thing ...))

  Allows for conditional compilation of code that depends on a
  symbol being available (in our case below, a macro)."
  [sym expr]
  (when (resolve sym)
    `~expr))

(defn- ->long
  "Attempt to parse a string as a Long and return nil if it fails."
  [s]
  (try
    (and s (Long/parseLong s))
    (catch Throwable _)))

(defn- ellipsis [s n] (if (< n (count s)) (str "..." (subs s (- (count s) n))) s))

(comment
  (ellipsis "this is a long string" 10)
  (ellipsis "short string" 20)
  ,)

(defn- clean-trace
  "Given a stack trace frame, trim class and file to the rightmost 24
  chars so they make a nice, neat table."
  [[c f file line]]
  [(symbol (ellipsis (-> (name c)
                         (demunge)
                         (str/replace #"--[0-9]{1,}" ""))
                     24))
   f
   (ellipsis file 24)
   line])

(defn- install-reveal-extras
  "Returns a Reveal view object that tracks each tap>'d value and
  displays its metadata and class type, and its value in a table.

  In order for this to take effect, this function needs to be
  called and its result sent to Reveal, after Reveal is up and
  running. This dev.clj file achieves this by executing the
  following code when starting Reveal:

  (future (Thread/sleep 6000)
          (tap> (install-reveal-extras)))

  The six second delay should be enough for Reveal to initialize
  and display its initial view."
  []
  (try
    (let [last-tap (atom nil)
          rx-sticker (requiring-resolve 'vlaaad.reveal/sticker)
          rx-stream-as-is (requiring-resolve 'vlaaad.reveal.ext/stream-as-is)
          rx-observable-view     @(requiring-resolve 'vlaaad.reveal.ext/observable-view)
          rx-value-view   @(requiring-resolve 'vlaaad.reveal.ext/value-view)
          rx-table-view   @(requiring-resolve 'vlaaad.reveal.ext/table-view)
          rx-as           (requiring-resolve 'vlaaad.reveal.ext/as)
          rx-raw-string   (requiring-resolve 'vlaaad.reveal.ext/raw-string)]
      (add-tap #(reset! last-tap %))
      (tap> {:sticker "displaying":window "tap>'d" :for "values"})
      (rx-sticker
       {:fx/type rx-observable-view
        :ref last-tap
        :fn (fn [x]
              (let [x' (if (var? x) (deref x) x) ; get Var's value
                    c  (class x') ; class of underlying value
                    m  (meta x)   ; original metadata
                    m' (when (var? x) (meta x')) ; underlying Var metadata (if any)
                    [ex-m ex-d ex-t]
                    (when (instance? Throwable x')
                      [(ex-message x') (ex-data x') (-> x' (Throwable->map) :trace)])
                     ;; if the underlying value is a function
                     ;; and it has a docstring, use that; if
                     ;; the underlying value is a namespace,
                     ;; run ns-publics and display that map:
                    x' (cond
                         (and (fn? x') (or (:doc m) (:doc m')))
                         (or (:doc m) (:doc m'))
                         (= clojure.lang.Namespace c)
                         (ns-publics x')
                         ex-t ; show stack trace if present
                         (mapv clean-trace ex-t)
                         :else
                         x')]
                {:fx/type :v-box
                 :children
                  ;; in the top box, display metadata
                 [{:fx/type rx-value-view
                   :v-box/vgrow :always
                   :value (cond-> (assoc m :_class c)
                            m'
                            (assoc :_meta m')
                            ex-m
                            (assoc :_message ex-m)
                            ex-d
                            (assoc :_data ex-d))}
                  (cond
                     ;; display a string in raw form for easier reading:
                    (string? x')
                    {:fx/type rx-value-view
                     :v-box/vgrow :always
                     :value (rx-stream-as-is (rx-as x' (rx-raw-string x' {:fill :string})))}
                     ;; automatically display URLs using the internal browser:
                    (instance? java.net.URL x')
                    {:fx/type :web-view
                     :url (str x')}
                     ;; else display simple values as a single item in a table:
                    (or (nil? x') (not (seqable? x')))
                    {:fx/type rx-table-view
                     :items [x']
                     :v-box/vgrow :always
                     :columns [{:fn identity :header 'value}
                               {:fn str :header 'string}]}
                    :else ; display the value in a reasonable table form:
                    (let [head (first x')]
                      {:fx/type rx-table-view
                       :items x'
                       :v-box/vgrow :always
                       :columns (cond
                                  (map? head) (for [k (keys head)] {:header k :fn #(get % k)})
                                  (map-entry? head) [{:header 'key :fn key} {:header 'val :fn val}]
                                  (indexed? head) (for [i (range (bounded-count 1024 head))] {:header i :fn #(nth % i)})
                                  :else [{:header 'item :fn identity}])}))]}))}))
    (catch Throwable t
      (println "Unable to install Reveal extras!")
      (println (ex-message t)))))

(comment
  (tap> (install-reveal-extras))
  ,)

(comment
  (start-repl)

  ;; ensure a smooth exit after the REPL is closed
  (System/exit 0))

(defn add-reveal-view []
  (println "Calling \"install-reveal-extras\" ... ")
  (install-reveal-extras))

(defn- start-repl
  "Ensures we have a DynamicClassLoader, in case we want to use
  add-libs from the add-lib3 branch of clojure.tools.deps.alpha (to
  load new libraries at runtime).

  If Jedi Time is on the classpath, require it (so that Java Time
  objects will support datafy/nav).

  Attempts to start a Socket REPL server. The port is selected from:
  * SOCKET_REPL_PORT environment variable if present, else
  * socket-repl-port JVM property if present, else
  * .socket-repl-port file if present, else
  * defaults to 0 (which will automatically pick an available port)
  Writes the selected port back to .socket-repl-port for next time.

  Then pick a REPL as follows:
  * if Figwheel Main is on the classpath then start that, else
  * if Rebel Readline is on the classpath then start that, else
  * start a plain ol' Clojure REPL."
  []
  ;; set up the DCL:
  (try
    (let [cl (.getContextClassLoader (Thread/currentThread))]
      (.setContextClassLoader (Thread/currentThread) (clojure.lang.DynamicClassLoader. cl)))
    (catch Throwable t
      (println "Unable to establish a DynamicClassLoader!")
      (println (ex-message t))))

  ;; jedi-time?
  (try
    (require 'jedi-time.core)
    (println "Java Time is Datafiable...")
    (catch Throwable _))

  ;; socket repl handling:
  (let [s-port (or (->long (System/getenv "SOCKET_REPL_PORT"))
                   (->long (System/getProperty "socket-repl-port"))
                   (->long (try (slurp ".socket-repl-port") (catch Throwable _)))
                   0)]
    ;; if there is already a 'repl' Socket REPL open, don't open another:
    ;; (when-not (get (deref (requiring-resolve 'clojure.core.server/servers)) "repl")
    ;;   (try
    ;;     (let [server-name (str "REPL-" s-port)]
    ;;       ((requiring-resolve 'clojure.core.server/start-server)
    ;;        {:port s-port :name server-name
    ;;         :accept 'clojure.core.server/repl})
    ;;       (let [s-port' (.getLocalPort
    ;;                      (get-in @(requiring-resolve 'clojure.core.server/servers)
    ;;                              [server-name :socket]))]
    ;;         (println "Selected port" s-port' "for the Socket REPL...")
    ;;         ;; write the actual port we selected (for Chlorine/Clover to read):
    ;;         (spit ".socket-repl-port" (str s-port'))))
    ;;     (catch Throwable t
    ;;       (println "Unable to start the Socket REPL on port" s-port)
    ;;       (println (ex-message t)))))
    )

  ;; if Portal and clojure.tools.logging are both present,
  ;; cause all (successful) logging to also be tap>'d:
  (try
    ;; if we have Portal on the classpath...
    (require 'portal.console)
    ;; ...then install a tap> ahead of tools.logging:
    (let [log-star (requiring-resolve 'clojure.tools.logging/log*)
          log*-fn  (deref log-star)]
      (alter-var-root
       log-star
       (constantly
        (fn [logger level throwable message]
          (try
            (let [^StackTraceElement frame (nth (.getStackTrace (Throwable. "")) 2)
                  class-name (symbol (demunge (.getClassName frame)))]
              ;; only called for enabled log levels:
              (tap>
               {:form     '()
                :level    level
                :result   (or throwable message)
                :ns       (symbol (or (namespace class-name)
                                      ;; fully-qualified classname - strip class:
                                      (str/replace (name class-name) #"\.[^\.]*$" "")))
                :file     (.getFileName frame)
                :line     (.getLineNumber frame)
                :column   0
                :time     (java.util.Date.)
                :runtime  :clj}))
            (catch Throwable _))
          (log*-fn logger level throwable message)))))
    (catch Throwable _))

  ;; select and start a main REPL:
  (let [[repl-name repl-fn]
        (or (try (when-let [reveal (requiring-resolve 'vlaaad.reveal/repl)]
                   (let [kickstart-reveal
                         (fn [label repl-fn]
                           ;; a six second delay should be sufficient:
                           (future (Thread/sleep 6000)
                                   (tap> (install-reveal-extras)))
                           [label repl-fn])]
                     (cond ;; if we're in Figwheel, just start the Reveal UI
                       (resolve 'figwheel.main/-main)
                       (when-sym figwheel.main/-main
                         (kickstart-reveal
                          "Figwheel+Reveal UI"
                          #(let [fw-main (requiring-resolve 'figwheel.main/-main)]
                             (add-tap ((requiring-resolve 'vlaaad.reveal/ui)))
                             (fw-main "-b" "dev" "-r"))))
                       ;; if Rebel is also available, use it as Reveal's REPL
                       ;; courtesy of didibus on Slack (plus when-sym above):
                       (resolve 'rebel-readline.core/with-line-reader)
                       (when-sym rebel-readline.core/with-line-reader
                         (let [rebel-create-line-reader
                               (requiring-resolve 'rebel-readline.clojure.line-reader/create)
                               rebel-create-service
                               (requiring-resolve 'rebel-readline.clojure.service.local/create)
                               rebel-create-repl-read
                               (requiring-resolve 'rebel-readline.clojure.main/create-repl-read)]
                           (kickstart-reveal "Reveal+Rebel Readline"
                                             #(rebel-readline.core/with-line-reader
                                                (rebel-create-line-reader (rebel-create-service))
                                                (reveal :prompt (fn []) :read (rebel-create-repl-read))))))
                       :else
                       (kickstart-reveal "Reveal" reveal))))
                 (catch Throwable _))
            (try
              (let [figgy (requiring-resolve 'figwheel.main/-main)]
                ["Figwheel Main" #(figgy "-b" "dev" "-r")])
              (catch Throwable _))
            (try ["Rebel Readline" (requiring-resolve 'rebel-readline.main/-main)]
                 (catch Throwable _))
            ["clojure.main" (resolve 'clojure.main/main)])]
    (println "Starting" repl-name "as the REPL...")
    ;; (repl-fn)
    (if (requiring-resolve 'vlaaad.reveal/repl)
      ((requiring-resolve 'nrepl.cmdline/-main)
       "-m" "nrepl.cmdline"
       "-p" "33933"
       "--middleware"
       "[vlaaad.reveal.nrepl/middleware,refactor-nrepl.middleware/wrap-refactor,cider.nrepl/cider-middleware]"
       ;; "--interactive"
       ))))


 (def foo "defined a foo str in dev.clj")

(start-repl)
