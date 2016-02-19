(ns ship
  (:require
   [org.httpkit.client :as client]
   [clojure.data.json :as json]))
(require '[clojure.core.match :refer [match]])

;; constants
(def base_url "https://api.realartists.com/api/")
(def api_version "20151105")

;; helpers
(defn body [request]
  (try
    (json/read-json (get request :body))
    (catch Exception e (merge request {:error true}))))

(defn- identifier [object]
  (match [object]
    [{:identifier x}] x
    [{"identifier" x}] x
    :else object))

(defn- construct_url [endpoint]
  (str base_url api_version "/" endpoint))

(defn- headers [token] {:Authorization (str "Ship " token)})
(defn- post_headers [token] = (headers token))
(defn- meta_data
  ([token] {:headers (headers token)
            :insecure? true})
  ([token form params]
   (let [core {:insecure? true
               :headers (merge (post_headers token) {"Content-Type" "application/json"})}
         payload (match [form]
                   [:get] {:query-params params
                           :content-type :json}
                   [:put] {:accept :json
                           :body (json/write-str params)}
                   [:delete] {}
                   :else {:body (if (or (string? params) (nil? params)) params (json/write-str params))
                          :headers (post_headers token)})]
     (merge core payload))))
(defn- debug [heads] (merge heads {:debug true :save-request? true}))

(defmacro sync-call [proto url meta]
  `(let [{:keys [status# headers# body# error#] :as resp#}
         @(~proto ~url ~meta)]
     (if error#
       nil
       (body resp#))))

(defmacro def-resource [func_name url]
  "Define the endpoints for a specific resource on the backend"
  `(defn ~func_name
     ([token#] (sync-call client/get ~(construct_url url) (meta_data token#)))
     ([token# pred#]
      (sync-call client/get ~(construct_url (str url "/search")) (meta_data token# :get pred#)))
     ([token# method# pred# & [id#]]
      (match [method#]
        [:new] (sync-call client/post ~(construct_url url) (meta_data token# :post pred#))
        [:update] (sync-call client/patch (construct_url (str ~url "/" id#)) (meta_data token# :post pred#))
        :else "Unsupported value"))))

(def-resource users "users")
(def-resource components "components")
(def-resource classifications "classifications")
(def-resource milestones "milestones")
(def-resource priorities "priorities")
(def-resource states "states")
(def-resource problems "problems")

;; keyword definitions
(defn problem_keyword_set [token identifier keyword & [value]]
  "Update and existing problem and add/update a keyword"
  (sync-call client/put
             (construct_url (str "problems/" identifier "/keywords/" keyword))
             (meta_data token :put (if (nil? value)
                                     ""
                                     value))))

(defn problem_keyword_delete [token identifier keyword]
  "delete keyword from problem"
  (client/delete (construct_url (str "problems/" identifier "/keywords/" keyword)) (meta_data token :delete nil)))
(defn me
  "Get Current User"
  [token]
  (get (users token {:predicate "identifier = $ApiUser"}) 0))

(defn users_active [token]
  "Get a list of active users"
  (users token {:predicate "inactive == NO"}))

(defn components_parent [token component]
  "Returns the parent of the target component"
  (components token {:predicate (str "ANY children.identifier = " (identifier component))}))

(defn component_children [token component]
  "Returns a list of the child components of the target component"
  (components token {:predicate (str "parent.identifier = " (identifier component))}))

(defn states_initial [token]
  "Returns the states that are initial"
  (states token {:predicate "Initial = YES"}))

(defn state_initial [token]
  "Returns the first Initial state"
  (get (states_initial token) 0))

(defn state_transition [token state]
  (states token {:predicate (str "ANY PreviousStates.identifier = " (identifier state))}))

(defn problem_search [token & {:keys [predicate savedQuery includeDetail] :or {predicate "" savedQuery "" includeDetail nil}}]
  (let
   [pred (if (clojure.string/blank? predicate) {} {:predicate predicate})
    query (if (clojure.string/blank? savedQuery) {} {:savedQuery savedQuery})
    detail (if (nil? includeDetail) {} {:includeDetail true})
    payload (merge pred query detail)]
    (problems token payload)))

(defn unresolved_problems [token]
  "return all unresolved problems"
  (problems token {:predicate "state.resolved = NO"}))
