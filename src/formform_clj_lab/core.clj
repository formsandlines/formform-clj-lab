(ns formform-clj-lab.core)

;; Just some calling and crossing using lazy sequences in Clojure
;; See other source files for some more interesting experiments


(defn- call* [f] (repeat f))
(defn call
  ([]    '())
  ([f]   (take 2 (call* f)))
  ([f n] (take n (call* f))))

(defn cross* [f] (iterate #(list %) f))
(defn cross
  ([] '())
  ([f]   (second (cross* f)))
  ([f n] (last (take n (cross* f)))))


(comment
  
  (= (cross) (call)) ;=> true
  (= (cross '()) (call '())) ;=> false

  (call '())
  (call '() 3)

  (cross '())
  (cross '() 3)

  (call (call (call '())))

  (cross (cross (cross '())))

  ,)
