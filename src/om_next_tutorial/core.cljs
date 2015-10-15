(ns om-next-tutorial.core
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(def app-state (atom {:count 0}))

(defn read [{:keys [state] :as env} key params]
  (let [st @state]
    (if-let [[_ value] (find st key)]
      {:value value}
      {:value :not-found})))

(defn mutate [{:keys [state] :as env} key params]
  (if (= 'increment key)
    {:value  [:count]
     :action #(swap! state update-in [:count] inc)}
    {:value :not-found}))

(defui Counter
       static om/IQuery
       (query [this]
              [:count])
       Object
       (render [this]
               (let [{:keys [count]} (om/props this)]
                 (dom/div nil
                          (dom/span nil (str "Count: " count))
                          (dom/button
                            #js {:onClick
                                 (fn [_] (om/transact! this '[(increment)]))}
                            "Click me!")))))

(def reconciler
  (om/reconciler
    {:state  app-state
     :parser (om/parser {:read read :mutate mutate})}))

(om/add-root! reconciler
              Counter (gdom/getElement "app-counter"))

(defui HelloWorld
       Object
       (render [this]
               (dom/div nil (get (om/props this) :title))))

(def hello (om/factory HelloWorld))

(js/React.render
  (apply dom/div nil
         (map #(hello {:title (str "Hello " %)})
              (range 6)))
  (gdom/getElement "app"))