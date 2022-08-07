(ns formform-clj-lab.exp6-1
  (:require
    [clojure.core.logic :as l :refer [run run* defne conde]]))

;; 14.07.2022

;; Relational programming and unification can be fruitful to represent
;; algebraic replacement rules

;; Clojures core.logic is an original implementation of miniKanren,
;; which is a nice functional approach to logic programming in the
;; relational paradigm

;; However, it seems to lack facilities to deal with commutative
;; structures in unification / pattern matching

;; Properties of the FORM structure:
;; - It is commutative, but order preservation is preferred:
;;   (a b) == (b a), through lack of order assumption in graphs
;; - It is idempotent, but redundance must be representable:
;;   (a a) == (a), through law of calling
;; - It is associative, but only with (redundant) double marks:
;;   (p (q r)) =/= ((p q) r), but
;;   (p ((q r))) == (((p q)) r), through law of crossing


;; attempts to represent some replacement rules:

#_:clj-kondo/ignore
(defne pa-j1 "position" [eqL eqR]
  ([[ [[p] p] ] [ nil ]])
  ([[ nil ] [ [[p] p] ]]))

#_:clj-kondo/ignore
(defne pa-j2 "transposition" [eqL eqR]
  ([[ [[p r] [q r]] ] [ [[p] [q]] r ]])
  ([[ [[p] [q]] r ] [ [[p r] [q r]] ]]))

#_:clj-kondo/ignore
(defne pa-c1 "reflexion" [eqL eqR]
  ([[ [[v]] ] [ v ]]))

#_:clj-kondo/ignore
(defne pa-c2 "generation" [eqL eqR]
  ([[ [a b] b ] [ [a] b ]]))

#_:clj-kondo/ignore
(defne pa-c5 "iteration" [eqL eqR]
  ([[ [v v] ] [ v ]]))

(comment

  (run* [q]
    (pa-j1 [ '((p) p) ] q)) ;=> ([ nil ])

  (run* [q]
    (pa-j1 [ '((p) x) ] q)) ;=> ()

  (run* [q]
    (pa-j1 q [ nil ])) ;=> ([ [[_0] _0] ])

  (run* [q]
    (pa-j1 q [ 'x ])) ;=> ()


  (run* [q]
    (pa-j2 [ '((p r) (q r)) ] q)) ;=> ([ [[p] [q]] r ])

  (run* [q]
    (pa-j2 [ '((p r) (q x)) ] q)) ;=> ()

  (run* [q]
    (pa-j2 q [ '((p) (q)) 'r ])) ;=> ([ [[p r] [q r]] ])

  (run* [q]
    (pa-j2 q [ '((p) (q)) '() ])) ;=> ([ [[p ()] [q ()]] ])


  (run* [q]
    (pa-c1 [ '((x)) ] q)) ;=> ([ x ])

  (run* [q]
    (pa-c1 q [ 'x ])) ;=> ([ [[x]] ])


  (run* [q]
    (pa-c2 [ '(a b) 'b ] q)) ;=> ([ [a] b ])


  (run* [q]
    (pa-c5 [ '(x x) ] q)) ;=> ([ x ])

  (run* [q]
    (pa-c5 q [ 'x ])) ;=> ([ [x x] ])

  )
(comment
  ;; recursive matches

  ;; this is not really useful, but…:
  #_:clj-kondo/ignore
  (defne solve [fml out]
    ([_ [fml]])
    ([_ [res . d]]
     (conde
       [(pa-j1 fml res)]
       [(pa-j2 fml res)])
     (solve res d)))

  ;; failing matches will only return the input:
  (run 2 [q]
    (solve [ '((a)) ] q)) ;=> ([ [((a))] ])

  ;; succeeding matches oscillate between input and output:
  (run 4 [q]
    (solve [ '((a) a) ] q))
  ;=> ([[((a) a)]]
  ;    ([nil] [nil])
  ;    ([nil] [[[_0] _0]] [[[_0] _0]])
  ;    ([nil] [[[_0] _0]] [nil] [nil]))

  (run 4 [q]
    (solve [ '((a x) (b x)) ] q))
  ;=> ([[((a x) (b x))]] 
  ;    ([[[a] [b]] x] [[[a] [b]] x]) 
  ;    ([[[a] [b]] x] [[[a x] [b x]]] [[[a x] [b x]]]) 
  ;    ([[[a] [b]] x] [[[a x] [b x]]] [[[a] [b]] x] [[[a] [b]] x]))

  )
