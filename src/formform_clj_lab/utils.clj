(ns formform-clj-lab.utils
  (:require [clojure.string :as string]))


(defn seq-to-vec
  "Quick & dirty conversion from sequence to vector by string replacement"
  [s]
  (-> (str s)
      (-> (string/replace #"\(" "[") (string/replace #"\)" "]"))
      read-string))



(comment 
  (seq-to-vec '((()())()))
  ,)

