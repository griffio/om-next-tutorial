(ns om-next-tutorial.core
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

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