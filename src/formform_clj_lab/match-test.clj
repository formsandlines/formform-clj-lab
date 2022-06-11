(ns formform-clj-lab.match-test
  (:require [clojure.core.match :refer [match matchv]]))

;; Testing clojure.core.match (examples mostly from docs)

;; example:
(doseq [n (range 1 101)]
  (println
    (match [(mod n 3) (mod n 5)]
      [0 0] "FizzBuzz"
      [0 _] "Fizz"
      [_ 0] "Buzz"
      :else n)))


;; Matching literals

;; Wildcards in patterns
(let [x true
      y true
      z true]
  (match [x y z]
         [_ false true] 1
         [false true _] 2
         [_ _ false] 3
         [_ _ true]  4))

;; Bindings for wildcard values
(let [x 1 y 2 z 4]
  (match [x y z]
         [1 2 b] [:a0 b]
         [a 2 4] [:a1 a]))


;; Local scope and symbols

;; Match literal symbol
(match [['my-sym]]
       [['my-sym]] :success)

;; Match the value of an existing local binding
(let [a (+ 1 2)]
  (match [[3]]
         [[a]] a))  ;; 3 matches against the value of `a`, local binding is preserved

;; Create a "named" wildcard pattern that creates a binding of the given name to the right of the pattern row
(match [['my-sym]]
       [[a]] a) ;; a is a wildcard, here bound to 'my-sym on the right of the pattern row


;; Vector matching

(let [v [1 2 3]]
  (match [v]
         [[1 1 1]] :a0
         [[1 2 1]] :a1
         [[1 2 _]] :a2))

;; Rest syntax
(let [v [3 2 3]]
  (match [v]
         [[1 1 3]] :a0
         [[2 & r]] :a1
         [[3 & r]] :a2))

;; Extend match to support primitive arrays (?)
; (defn balance [^objects node]
;   (matchv ::objects [node]
;     [(:or [:black [:red [:red a x b] y c] z d]
;           [:black [:red a x [:red b y c]] z d]
;           [:black a x [:red [:red b y c] z d]]
;           [:black a x [:red b y [:red c z d]]])] (R (B a x b) y (B c z d))))


;; Seq matching

(let [x [1 2 nil nil nil]]
  (match [x]
         [([1] :seq)]   :a0
         [([1 2] :seq)] :a1
         [([1 2 nil nil nil] :seq)] :a2))

;; Rest syntax
(let [x '(1 2 3 4)]
  (match [x]
         [([1] :seq)] :a0
         [([_ 2 & ([a & b] :seq)] :seq)] [:a1 a b]))


;; Map matching

(let [x {:a 1 :b 1}]
  (match [x]
         [{:a _ :b 2}] :a0
         [{:a 1 :b _}] :a1
         [{:c 3 :d _ :e 4}] :a2))

;; Constrain map matching so that only maps with exact key set will match
(let [x {:a 1 :b 2}]
  (match [x]
         [({:a _ :b 2} :only [:a :b])] :a0
         [{:a 1 :c c}] :a1
         [{:c 3 :d d :e 4}] :a2))

;; Assert that a key must be present by using a wildcard value
(let [x {:a 1 :b 1}]
  (match [x]
         [{:c _}] :a0
         :else :no-match))


;; Or-patterns

(let [x [1 2 3]]
  (match [x]
         [[1 (:or 3 4) 3]] :a0
         [[1 (:or 2 3) 3]] :a1))
;=> :a1
    
(let [x {:a 3}]
  (match [x]
         [{:a (:or 1 2)}] :a0
         [{:a (:or 3 4)}] :a1))
;=> :a1


;; Guards

;; Guards are simple boolean tests
(defn div3? [x] (zero? (rem x 3)))
(let [y [2 3 4 5]]
  (match [y]
         [[_ (a :guard even?) _ _]] :a0
         [[_ (b :guard [odd? div3?]) _ _]] :a1))
;=> :a1

;; Guard predicate invoked on the whole map:
(let [y {:a 5 :b 9 :c 0}]
  (match [y]
         [{:a _ :b 2}] :a0
         [({:a 5 :b _} :guard [(comp odd? :b) (comp div3? :b)])] :a1
         [({:a 5 :b _} :guard #(= 0 (:c %)))] :a2))
;=> :a1


;; Capture part of a match with a binding
(let [v [[1 2]]]
  (match [v]
         [[[3 1]]] :a0
         [[([1 a] :as b)]] [:a1 a b]))
;=> [:a1 2 [1 2]]


;; Nested maps
(match [{:a {:b :c}}]
       [{:a {:b nested-arg}}] nested-arg)
;=> :c


;; Function application results
(let [n 0]
  (match [n]
         [(1 :<< inc)] :one
         [(2 :<< dec)] :two
         :else :no-match))
;=> :one


