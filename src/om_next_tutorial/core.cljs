(ns om-next-tutorial.core
(:require [goog.dom :as gdom]
          [om.next :as om :refer-macros [defui]]
          [om.dom :as dom]))

(enable-console-print!)

(defui HelloWorld
  Object
  (render [this]
          (dom/div nil "Hello world!")))

(def hello (om/factory HelloWorld))

(js/React.render (hello) (gdom/getElement "app"))
