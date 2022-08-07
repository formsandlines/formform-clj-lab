(ns formform-clj-lab.utils
  (:require [clojure.string :as string]
            [clojure.walk :as w]))


(defn seq-to-vec
  "Quick & dirty conversion from sequence to vector by string replacement"
  [s]
  (-> (str s)
      (-> (string/replace #"\(" "[") (string/replace #"\)" "]"))
      read-string))

(defn sets [coll]
  ((fn f [x]
     (if (seqable? x)
       (into #{} (map f x))
       x)) coll))

(defn maps [coll]
  (let [f (fn f [x]
            (if (seqable? x)
              (if (nil? x)
                nil
                [(into {} (map f x)) nil])
              [x nil]))]
    (into {} (map f coll))))

(defn maps->forms [m]
  (w/prewalk #(if (seqable? %)
                (if (seq %)
                  (keys %)
                  '())
                %) m))


