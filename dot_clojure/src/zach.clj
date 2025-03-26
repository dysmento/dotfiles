(ns zach
  (:require [portal.api :as p]))

(defn tap-portal
  []
  (add-tap p/submit)
  (p/open))
