(ns yetibot.core.handler
  (:require
    [clojure.core.async :refer [timeout chan go <! >! >!! <!! alts!]]
    [clojure.core.match :refer [match]]
    [clojure.stacktrace :as st]
    [clojure.string :refer [join]]
    [taoensso.timbre :refer [info warn error]]
    [yetibot.core.chat :refer [chat-data-structure]]
    [yetibot.core.config :refer [get-config]]
    [yetibot.core.interpreter :as interp]
    [yetibot.core.models.help :as help]
    [yetibot.core.parser :refer [parse-and-eval transformer parser]]
    [yetibot.core.util :refer [with-fresh-db]]
    [yetibot.core.util.format :refer [to-coll-if-contains-newlines format-exception-log]]))

;; timeout is measured in milliseconds
(def soft-timeout-time (or (get-config :soft-timeout) 3000))
;; logic is so make sure hard-time-out is creater than soft-time-out
(def hard-timeout-time (let [val (get-config :hard-timeout)
                             default 10000]
                         (if (or (nil? val) (< val soft-timeout-time))
                           default
                           val)))

(def timeout-messages
  {:soft-timeout "This seems to be taking long ... still working on it"
   :hard-timeout "Process took too long. Timeout out!!"})

(defn handle-unparsed-expr
  "Top-level entry point for parsing and evaluation of commands"
  ([chat-source user body]
   ; For backward compat, support setting user at this level.
   ; After deprecating, this can be removed.
   ; Currently it's used by the web API in yetibot.
   (info "handle unparsed expr:" chat-source body user)
   (binding [interp/*current-user* user
             interp/*chat-source* chat-source]
     (handle-unparsed-expr body)))
  ([body] (parse-and-eval body)))

(defn handle-parsed-expr
  "Top-level for already-parsed commands. Turns a parse tree into a string or
   collection result."
  [chat-source user parse-tree]
  (binding [interp/*current-user* user
            interp/*chat-source* chat-source]
    (transformer parse-tree)))

(def ^:private exception-format "👮 %s 👮")

(def all-event-types #{:message :leave :enter :sound :kick})

(defn command?
  "Returns true if prefix matches a built-in command or alias"
  [prefix]
  (boolean (help/get-docs-for prefix)))

(defn embedded-cmds
  "Parse a string and only return a collection of any embedded commands instead
   of the top level expression. Returns nil if there are none."
  [body]
  (->> (parser body)
       second second rest
       ; get expressions
       (filter #(= :expr (first %)))
       ; ensure prefix is actually a command
       (filter #(command? (-> % second second second)))))

(defn quacks-like-a-command?
  [body]
  (let [command  (or
                  ;; if it starts with a command prefix (!) it's a command
                  (when-let [[_ body] (re-find #"^\!(.+)" body)]
                    [(parser body)])
                  ;; otherwise, check to see if there are embedded commands
                  (embedded-cmds body))]
    (if-not (or (nil? command)
                (empty? (seq command)))
      command)))

(defn process-raw-command
  [chat-source user org-body parsed-cmds]
  (with-fresh-db
    (doall
     (map
      #(try
         (handle-parsed-expr chat-source user %)
         (catch Throwable ex
           (error "error handling expression:" org-body
                  (format-exception-log ex))
           (format exception-format ex)))
      parsed-cmds))))

(defn handle-raw
  "No-op handler for optional hooks.
   Expected event-types are:
   :message
   :leave
   :enter
   :sound
   :kick"
  [chat-source user event-type body
   & {:keys [timeout-ms input-chan] :or {timeout-ms soft-timeout-time input-chan nil}}]
  (when body
    (when-let [command (quacks-like-a-command? body)]
      (go
        (let [input (if input-chan
                      input-chan
                      (go
                        (process-raw-command chat-source user body command)))
                 message (if input-chan
                           (:hard-timeout timeout-messages)
                           (:soft-timeout timeout-messages))]
             (let [[chat-output channel]
                   (alts! [input (timeout timeout-ms)])]
               (condp = chat-output
                 nil (do
                       (if-not input-chan
                         (handle-raw chat-source user event-type body
                                     :timeout-ms hard-timeout-time
                                     :input-chan input))
                       (chat-data-structure message))
                 (chat-data-structure chat-output))))))))

(defn cmd-reader [& args] (handle-unparsed-expr (join " " args)))
