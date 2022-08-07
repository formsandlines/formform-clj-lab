(ns formform-clj-lab.exp5
  (:require [clojure.walk :as walk]
            [clojure.core.match :refer [match]]
            [clojure.math.combinatorics :as combo]))

;; 19.06.2022

;; Attempt to use core.match as a pattern-matching facility for
;; algebraic replacement rules

;; It does not seem to work with sets, so sequences have to be
;; permuted in order to match commutative terms

;; I built a normalize function to reduce the number of combinations
;; but there would still be plenty of possibilities that are
;; hard to match with this approach


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some helper functions

(def fcomp
  (fn [a b]
    ; (println ">> a:" a " , b: " b)
    (cond
      (= a b) 0
      ; (and (sequential? a)
      ;      (sequential? b)) (compare (count b) (count a))
      (sequential? a) -1
      (sequential? b) 1
      :else (compare a b))))

(defn normalize
  "Converts a sequential FORM to a set of sets for pattern matching."
  ([xs]
   (normalize xs fcomp))
  ([xs cp]
   (walk/postwalk #(if (sequential? %)
                     (->> %
                          (apply sorted-set-by cp)
                          vec)
                     %)
                  xs)))

(defn seq-first-permute
  "Takes a seq-first normalized FORM and returns a seq of permutations
  where each sequence in turn is swapped with the head of the FORM."
  [xs]
  (loop [[x & r] xs
         perms '()]
    (if (sequential? x)
      (recur r (cons (->> xs
                          (remove #(= % x))
                          (cons x)
                          (into []))
                     perms))
      perms)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pattern matching

(defn alg-j1->
  "Position: take out"
  [ctx]
  (let [ctx-normal (normalize ctx)
        ;; ! optimize to immediately return correct match
        ;; ? is this exhaustive
        res (->> (for [perm (seq-first-permute ctx-normal)]
                   (match perm
                          [[[& p] & q]] (if (= p q) [ nil ] nil)
                          :else nil))
                 (filter some?))]
    {:result (first res) :origin ctx-normal}))

(defn alg-j1<-
  "Position: put in"
  [ctx]
  (if (or (empty? ctx) (= ctx [ nil ]))
    {:result [ '[[p] p] ] :origin ctx}
    {:result nil :origin ctx}))

(defn alg-c1->
  "Reflect out"
  [ctx]
  (match ctx
         [ [[& fs]] ] {:result (vec fs) :origin ctx}
         :else {:result nil :origin ctx}))

(defn alg-c1<-
  "Reflect in"
  [ctx]
  (match ctx
         [ & fs ] {:result [ [(vec fs)] ] :origin ctx}
         :else {:result nil :origin ctx}))


(comment

  (normalize '[ c [b a] ]) ;=> [[a b] c]
  (normalize '[ [[a c] b] b [c a] ]) ;=> [[a c] [[a c] b] b]
  (seq-first-permute (normalize '[ [y x b] c [b a] ]))
  ;=> ([[b x y] [a b] c] [[a b] [b x y] c])
  (seq-first-permute [[1 2] [3 4 5] [6] 7 8 9])
  ;=> ([[6] [1 2] [3 4 5] 7 8 9] 
  ;    [[3 4 5] [1 2] [6] 7 8 9] 
  ;    [[1 2] [3 4 5] [6] 7 8 9])

  (alg-j1-> '[ [p [p]] ]) ;=> {:result [nil], :origin [[[p] p]]}
  (alg-j1-> '[ [[p] q] ]) ;=> {:result nil, :origin [[[p] q]]}
  (alg-j1-> '[ [b [a b] a] ]) ;=> {:result [nil], :origin [[[a b] a b]]}
  (alg-j1-> '[ [b c [a b] a] ]) ;=> {:result nil, :origin [[[a b] a b c]]}
  ;; context not permitted yet:
  (alg-j1-> '[ [b [a b] a] x ]) ;=> {:result nil, :origin [[[a b] a b] x]}
  ;; normalization will not always match patterns:
  (alg-j1-> '[ [[[a c] b] b [c a]] ]) 
  ;=> {:result nil, :origin [[[a c] [[a c] b] b]]}

  (alg-j1<- [  ]) ;=> {:result [[[p] p]], :origin []}
  (alg-j1<- [ nil ]) ;=> {:result [[[p] p]], :origin [nil]}


  (alg-c1-> '[ [[a]] ]) ;=> {:result [a], :origin [[[a]]]}
  (alg-c1-> '[ [[a [] b]] ]) ;=> {:result [a [] b], :origin [[[a [] b]]]}
  (alg-c1-> '[ [[a [b]]] ]) ;=> {:result [a [b]], :origin [[[a [b]]]]}
  (alg-c1-> '[ [a] ]) ;=> {:result nil, :origin [[a]]}

  (alg-c1<- '[ a ]) ;=> {:result [[[a]]], :origin [a]}
  (alg-c1<- '[ a ]) ;=> {:result [[[a]]], :origin [a]}
  (alg-c1<- '[ a [b] ]) ;=> {:result [[[a [b]]]], :origin [a [b]]}

)
(comment
  ;; some experiments

  (let [x #{:a :b}]
    (match [x]
      [#{:b :a}] :a0
      :else nil))


  (def eq [#{:a :z}
           :a])

  (let [x #{:a :z}
        ptn (first eq)
        mat (second eq)]
    (match [x]
      [ptn] mat
      :else nil))


  (let [x 1 y 2
        ptn [1 2]]
    (match [x y]
      ptn "yes"
      :else nil))
  
  (walk/macroexpand-all '(let [ctx '[[a] a]]
                           (match [ctx]
                             [[[& p] & q]] nil
                             :else ctx)))

  (let [ctx '[[a] a]]
    (match [ctx]
      [[[& p] & q]] true
      :else false))

  (combo/permutations [1 2 3 4])
)

