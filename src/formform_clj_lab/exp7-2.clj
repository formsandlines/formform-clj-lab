(ns formform-clj-lab.exp7-2
  (:require 
    [meander.strategy.epsilon :as r]
    [meander.epsilon :as m]))

;; 24.07.2022

;; Tried using plain vector patterns with permutations in m/or
;; alternatives, but the combinatorial space explodes here and
;; there seems to be no generator function for permutations in patterns

;; The expressiveness and flexibility is just too limited, even when
;; forms are sorted in a normalized way before matching

;; I thought maybe order for variable names can be determined lexically
;; - equal variables and forms will be together

;; But what is the “normal” order of a FORM that matches `((?p ?r) (?q ?r))`?
;; - how do I know the match for `?r` in `(?p ?r)` is the same as
;;   the match in `(?q ?r)` before matching it?
;; - I will not be able to determine the order in the input FORM
;;   unless I compare every element (and subsequence) in it
;; - e.g. if I have a FORM `((y a) (a x))`, lexical order would be
;;   `((a x) (a y))`, but this is not the matching order in the pattern
;;   and I could as well have something like `((a b) (b c))`

;; So it seems like there is no way around using sets or maps
;; if I want to do anything non-trivial here


(def pa-j1-> "Position: take out"
  (r/rewrite
    [!pre ... (m/or [?p [?p]] [[?p] ?p]) . !post ...]
    [!pre ... . !post ...]))

(def pa-j1<- "Position: put in"
  (r/rewrite
    [!acc ...]
    [[['p] 'p] . !acc ...]))


(def pa-j2-> "Transposition: collect"
  (r/rewrite
    ;; ? how to scan for & join the same multiple elems in both colls?
    [!pre ... [(m/or [?p ?r] [?r ?p])
               (m/or [?q ?r] [?r ?q])] . !post ...]
    [!pre ... [[?p] [?q]] ?r . !post ...]))

(def pa-j2<- "Transposition: distribute"
  (r/rewrite
    [(m/and !r !r2) ... [[!p ...] [!q ...]] . (m/and !r !r2) ...]
    [ [[!p ... . !r ...] [!q ... . !r2 ...]] ]))


(comment
  
  (pa-j1-> '[a [a [a]] b c]) ;=> [a b c]
  (pa-j1<- '[a]) ;=> [[[p] p] a]

  (pa-j2-> '[p [[z x] [y z]] z]) ;=> [p [[x] [y]] z z]
  (pa-j2<- '[z [[x] [y]] w v]) ;=> [[[x z w v] [y z w v]]]
  )

