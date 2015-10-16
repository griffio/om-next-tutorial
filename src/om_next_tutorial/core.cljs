(ns om-next-tutorial.core
  (:require [datascript.core :as dsc]
            [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(enable-console-print!)

(def app-state
  (atom
    {:app/title "Animals"
     :animals/list
                [[1 "Ant"] [2 "Antelope"] [3 "Bird"] [4 "Cat"] [5 "Dog"]
                 [6 "Lion"] [7 "Mouse"] [8 "Monkey"] [9 "Snake"] [10 "Zebra"]]}))

(defmulti read-animals (fn [env key params] key))

(defmethod read-animals :default
  [{:keys [state] :as env} key params]
  (let [st @state]
    (if-let [[_ value] (find st key)]
      {:value value}
      {:value :not-found})))

(defmethod read-animals :animals/list
  [{:keys [state] :as env} key {:keys [start end]}]
  {:value (subvec (:animals/list @state) start end)})

(defui AnimalsList
       static om/IQueryParams
       (params [this]
               {:start 0 :end 10})
       static om/IQuery
       (query [this]
              '[:app/title (:animals/list {:start ?start :end ?end})])
       Object
       (render [this]
               (let [{:keys [app/title animals/list]} (om/props this)]
                 (dom/div nil
                          (dom/title nil title)
                          (apply dom/ul nil
                                 (map
                                   (fn [[i name]]
                                     (dom/li nil (str i ". " name)))
                                   list))))))

(def reconciler
  (om/reconciler
    {:state  app-state
     :parser (om/parser {:read read-animals})}))

(om/add-root! reconciler
              AnimalsList (gdom/getElement "animals"))
;;===============================
;; People
;;===============================
(def init-data
  {:list/one [{:name "John" :points 0}
              {:name "Mary" :points 0}
              {:name "Bob" :points 0}]
   :list/two [{:name "Mary" :points 0 :age 27}
              {:name "Gwen" :points 0}
              {:name "Jeff" :points 0}]})


(defmulti read-people om/dispatch)

(defn get-people [state key]
  (let [st @state]
    (into [] (map #(get-in st %)) (get st key))))

(defmethod read-people :list/one
  [{:keys [state] :as env} key params]
  {:value (get-people state key)})

(defmethod read-people :list/two
  [{:keys [state] :as env} key params]
  {:value (get-people state key)})

(defmulti mutate om/dispatch)

(defmethod mutate 'points/increment
  [{:keys [state]} _ {:keys [name]}]
  {:action
   (fn []
     (swap! state update-in
            [:person/by-name name :points]
            inc))})

(defmethod mutate 'points/decrement
  [{:keys [state]} _ {:keys [name]}]
  {:action
   (fn []
     (swap! state update-in
            [:person/by-name name :points]
            #(let [n (dec %)] (if (neg? n) 0 n))))})

;; -----------------------------------------------------------------------------
;; Components

(defui Person
       static om/Ident
       (ident [this {:keys [name]}]
              [:person/by-name name])
       static om/IQuery
       (query [this]
              '[:name :points :age])
       Object
       (render [this]
               (println "Render Person" (-> this om/props :name))
               (let [{:keys [points name foo] :as props} (om/props this)]
                 (dom/li nil
                         (dom/label nil (str name ", points: " points))
                         (dom/button
                           #js {:onClick
                                (fn [e]
                                  (om/transact! this
                                                `[(points/increment ~props)]))}
                           "+")
                         (dom/button
                           #js {:onClick
                                (fn [e]
                                  (om/transact! this
                                                `[(points/decrement ~props)]))}
                           "-")))))

(def person (om/factory Person {:keyfn :name}))

(defui ListView
       Object
       (render [this]
               (println "Render ListView" (-> this om/path first))
               (let [list (om/props this)]
                 (apply dom/ul nil
                        (map person list)))))

(def list-view (om/factory ListView))

(defui RootView
       static om/IQuery
       (query [this]
              (let [subquery (om/get-query Person)]
                `[{:list/one ~subquery} {:list/two ~subquery}]))
       Object
       (render [this]
               (println "Render RootView")
               (let [{:keys [list/one list/two]} (om/props this)]
                 (apply dom/div nil
                        [(dom/h2 nil "List A")
                         (list-view one)
                         (dom/h2 nil "List B")
                         (list-view two)]))))

(def reconciler
  (om/reconciler
    {:state  init-data
     :parser (om/parser {:read read-people :mutate mutate})}))

(om/add-root! reconciler
              RootView (gdom/getElement "people"))


;;===============================
;; DataScript
;;===============================

(def conn (dsc/create-conn {}))

(dsc/transact! conn
               [{:db/id     -1
                 :app/title "A title is click bait!"
                 :app/count 0}])

(defmulti read-dsc om/dispatch)

(defmethod read-dsc :app/counter
  [{:keys [state selector]} _ _]
  {:value (dsc/q '[:find [(pull ?e ?selector) ...]
                   :in $ ?selector
                   :where [?e :app/title]]
                 (dsc/db state) selector)})

(defmulti mutate-dsc om/dispatch)

(defmethod mutate-dsc 'app/increment
  [{:keys [state]} _ entity]
  {:value  [:app/counter]
   :action (fn [] (dsc/transact! state
                                 [(update-in entity [:app/count] inc)]))})

(defui Counter
       static om/IQuery
       (query [this]
              [{:app/counter [:db/id :app/title :app/count]}])
       Object
       (render [this]
               (let [{:keys [app/title app/count] :as entity}
                     (get-in (om/props this) [:app/counter 0])]
                 (dom/div nil
                          (dom/h2 nil title)
                          (dom/label nil (str "Count: " count))
                          (dom/button
                            #js {:onClick
                                 (fn [_]
                                   (om/transact! this
                                                 `[(app/increment ~entity)]))}
                            "Click me!")))))

(def reconciler-dsc
  (om/reconciler
    {:state  conn
     :parser (om/parser {:read read-dsc :mutate mutate-dsc})}))

(om/add-root! reconciler-dsc
              Counter (gdom/getElement "app-counter"))