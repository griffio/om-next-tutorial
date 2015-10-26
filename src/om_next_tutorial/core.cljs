(ns om-next-tutorial.core
  (:require [datascript.core :as dsc]
            [cljs.pprint]
            [goog.dom :as gdom]
            [cognitect.transit :as tt]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [clojure.test.check :as tsck]
            [clojure.test.check.generators :as tgen]
            [clojure.test.check.properties :as prop])
  (:import [goog.crypt Sha256]))

(enable-console-print!)

;;===============
;; Hello World - basic stateless component
;;===============
(defui HelloWorld
       Object
       (render [this]
               (dom/div nil (get (om/props this) :title))))

(def greetings (om/factory HelloWorld))

(js/React.render
  (greetings {:title "Hello World!"})
  (gdom/getElement "greetings"))

;;===============
;; App State
;;===============
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
                          (dom/h3 nil title)
                          (apply dom/ul nil
                                 (map
                                   (fn [[i name]]
                                     (dom/li nil (str i ". " name)))
                                   list))))))

(def animals-reconciler
  (om/reconciler
    {:state  app-state
     :parser (om/parser {:read read-animals})}))

(om/add-root! animals-reconciler
              AnimalsList (gdom/getElement "animals"))
;;===============================
;; People
;;===============================
(def init-data
  {:list/one [{:name "Yoda" :points 0 :age 800}
              {:name "Mary" :points 0}
              {:name "Bob" :points 0}]
   :list/two [{:name "Mary" :points 0 :age 27}
              {:name "Gwen" :points 0}
              {:name "Jeff" :points 0}
              {:name "Yoda" :points 0}]})

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
                                (fn [_] (om/transact! this `[(points/increment ~props)]))}
                           "+")
                         (dom/button
                           #js {:onClick
                                (fn [_] (om/transact! this `[(points/decrement ~props)]))}
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
                        [(dom/h3 nil "List A")
                         (list-view one)
                         (dom/h3 nil "List B")
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
                 :app/title "This title is click bait!"
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

;;==============================
;;Dashboard
;;==============================

(def init-dashboard-data
  {:dashboard/items
   [{:id      0 :type :dashboard/post
     :author  "Laura Smith"
     :title   "A Post!"
     :content "Lorem ipsum dolor sit amet, quem atomorum te quo"}
    {:id      1 :type :dashboard/photo
     :title   "A Photo!"
     :image   "photo.jpg"
     :caption "Lorem ipsum"}
    {:id      2 :type :dashboard/post
     :author  "Jim Jacobs"
     :title   "Another Post!"
     :content "Lorem ipsum dolor sit amet, quem atomorum te quo"}
    {:id    3 :type :dashboard/graphic
     :title "Charts and Stuff!"
     :image "chart.jpg"}
    {:id      4 :type :dashboard/post
     :author  "May Fields"
     :title   "Yet Another Post!"
     :content "Lorem ipsum dolor sit amet, quem atomorum te quo"}]})

(defui Post
       static om/IQuery
       (query [this]
              [:id :type :title :author :content])
       Object
       (render [this]
               (let [{:keys [title author content] :as props} (om/props this)]
                 (dom/div nil
                          (dom/h3 nil title)
                          (dom/h4 nil author)
                          (dom/p nil content)))))

(def post (om/factory Post))

(defui Photo
       static om/IQuery
       (query [this]
              [:id :type :title :image :caption])
       Object
       (render [this]
               (let [{:keys [title image caption]} (om/props this)]
                 (dom/div nil
                          (dom/h3 nil (str "Photo: " title))
                          (dom/div nil image)
                          (dom/p nil "Caption: ")))))

(def photo (om/factory Photo))

(defui Graphic
       static om/IQuery
       (query [this]
              [:id :type :image])
       Object
       (render [this]
               (let [{:keys [title image]} (om/props this)]
                 (dom/div nil
                          (dom/h3 nil (str "Graphic: " title))
                          (dom/div nil image)))))

(def graphic (om/factory Graphic))

(defui DashboardItem
       static om/Ident
       (ident [this {:keys [id type]}]
              [type id])
       static om/IQuery
       (query [this]
              (zipmap
                [:dashboard/post :dashboard/photo :dashboard/graphic]
                (map #(conj % :favorites)
                     [(om/get-query Post)
                      (om/get-query Photo)
                      (om/get-query Graphic)])))
       Object
       (render [this]
               (let [{:keys [id type favorites] :as props} (om/props this)]
                 (dom/li
                   #js {:style #js {:padding 10 :borderBottom "1px solid black"}}
                   (dom/div nil
                            (({:dashboard/post    post
                               :dashboard/photo   photo
                               :dashboard/graphic graphic} type)
                              (om/props this)))
                   (dom/div nil
                            (dom/p nil (str "Favourites: " favorites))
                            (dom/button
                              #js {:onClick
                                   (fn [e]
                                     (om/transact! this
                                                   `[(dashboard/favorite {:ref [~type ~id]})]))}
                              "Favorite!"))))))

(def dashboard-item (om/factory DashboardItem))

(defui Dashboard
       static om/IQuery
       (query [this]
              [{:dashboard/items (om/get-query DashboardItem)}])
       Object
       (render [this]
               (let [{:keys [dashboard/items]} (om/props this)]
                 (apply dom/ul
                        #js {:style #js {:padding 0}}
                        (map dashboard-item items)))))

(defmulti read-dashboard om/dispatch)

(defmethod read-dashboard :dashboard/items
  [{:keys [state ast]} k _]
  (let [st @state]
    {:value   (into [] (map #(get-in st %)) (get st k))
     :dynamic (update-in ast [:sel]
                         #(->> (for [[k _] %]
                                 [k [:favorites]])
                               (into {})))
     :static  (update-in ast [:sel]
                         #(->> (for [[k v] %]
                                 [k (into [] (remove #{:favorites}) v)])
                               (into {})))}))

(defmulti mutate-dashboard om/dispatch)

(defmethod mutate-dashboard 'dashboard/favorite
  [{:keys [state]} k {:keys [ref]}]
  {:action
   (fn []
     (swap! state update-in (conj ref :favorites) inc))})

(def dashboard-reconciler
  (om/reconciler
    {:state  init-dashboard-data
     :parser (om/parser {:read read-dashboard :mutate mutate-dashboard})}))

(om/add-root! dashboard-reconciler Dashboard (gdom/getElement "dashboard"))

;;**********************
;;People
;;**********************
(def init-people-data
  {:people [{:id 0 :name "Bob" :friends []}
            {:id 1 :name "Laura" :friends []}
            {:id 2 :name "Mary" :friends []}]})

(defui Friend
       static om/Ident
       (ident [this props]
              [:person/by-id (:id props)])
       static om/IQuery
       (query [this]
              [:id :name]))

(defui Persona
       static om/Ident
       (ident [this props]
              [:person/by-id (:id props)])
       static om/IQuery
       (query [this]
              [:id :name {:friends (om/get-query Friend)}]))

(defui People
       static om/IQuery
       (query [this]
              [{:people (om/get-query Persona)}]))


(defn add-friend [state id friend]
  (letfn [(add* [friends ref]
                (cond-> friends
                        (not (some #{ref} friends)) (conj ref)))]
    (if-not (= id friend) ;; FIXED
      (-> state
          (update-in [:person/by-id id :friends]
                     add* [:person/by-id friend]))
      state)))

(defn remove-friend [state id friend]
  (letfn [(remove* [friends ref]
                   (cond->> friends
                            (some #{ref} friends) (into [] (remove #{ref}))))]
    (-> state
        (update-in [:person/by-id id :friends]
                   remove* [:person/by-id friend])
        (update-in [:person/by-id friend :friends] ;; FIXED
                   remove* [:person/by-id id]))))

(defmulti read-people om/dispatch)

(defmethod read-people :people
  [{:keys [state selector] :as env} key _]
  (let [st @state]
    (println selector)
    {:value (om/denormalize selector (get st key) st)}))

(defmulti mutate-people om/dispatch)

(defmethod mutate-people 'friend/add
  [{:keys [state] :as env} key {:keys [id friend] :as params}]
  {:action
   (fn [] (swap! state add-friend id friend))})

(defmethod mutate-people 'friend/remove
  [{:keys [state] :as env} key {:keys [id friend] :as params}]
  {:action (fn [] (swap! state remove-friend id friend))})

(def people-app-state
  (atom (om/normalize People init-data true)))

(def people-parser (om/parser {:read read-people :mutate mutate-people}))

;;***************
;;Gen tests
;;***************
(def gen-tx-add-remove
  (tgen/vector
    (tgen/fmap seq
               (tgen/tuple
                 (tgen/elements '[friend/add friend/remove])
                 (tgen/fmap (fn [[n m]] {:id n :friend m})
                            (tgen/tuple
                              (tgen/elements [0 1 2])
                              (tgen/elements [0 1 2])))))))

(defn self-friended? [{:keys [id friends]}]
  (boolean (some #{id} (map :id friends))))

(defn prop-no-self-friending []
  (prop/for-all [tx gen-tx-add-remove]
                (let [parser (om/parser {:read read-people :mutate mutate-people})
                      state (atom (om/normalize People init-data true))]
                  (parser {:state state} tx)
                  (let [ui (parser {:state state} (om/get-query People))]
                    (not (some self-friended? (:people ui)))))))

(defn friends-consistent? [people]
  (let [indexed (zipmap (map :id people) people)]
    (letfn [(consistent? [[id {:keys [friends]}]]
                         (let [xs (map (comp :friends indexed :id) friends)]
                           (every? #(some #{id} (map :id %)) xs)))]
      (every? consistent? indexed))))

(defn prop-friend-consistency []
  (prop/for-all [tx gen-tx-add-remove]
                (let [parser (om/parser {:read read-people :mutate mutate-people})
                      state  (atom (om/normalize People init-data true))]
                  (parser {:state state} tx)
                  (let [ui (parser {:state state} (om/get-query People))]
                    (friends-consistent? (:people ui))))))