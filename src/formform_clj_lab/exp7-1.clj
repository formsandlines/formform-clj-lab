(ns formform-clj-lab.exp7-1
  (:require 
    [meander.strategy.epsilon :as r]
    [meander.epsilon :as m]
    [formform-clj-lab.utils :refer [sets]]))

;; 21.07.2022

;; My first attempt at representing algebraic transformations with Meander
;; using set patterns

;; Unfortunately, set patterns have a “fake” rest syntax, so something
;; like `#{?x ^& ?x}` will conflict with the compiler and I am not sure
;; how far I can go with this kind of restriction…

;; Even something as innocent as `#{#{^& ?p} ^& ?p}` fails to compile:
;; “Unable to resolve classname: &”, unless I rename one instance of `?p`
(comment
  ;; this fails to compile:
  (def pa-j1-> "position"
    (r/rewrite
      #{ #{#{^& ?p} ^& ?p} ^& (..+ !ctx) }
      #{ ^& (!ctx ...) }))
  )

;; There are also no unique rest operators possible, like `&0` and `&1`,
;; which can be used in other patterns to divide collections
(comment
  ;; this matches only `e`, but not `d`:
  (m/find #{'a 'b 'c 'd 'e}
    #{ 'a ^& #{'b 'c} ^& ?y }
    ?y)

  ;; this fails to match:
  (m/find #{'a 'b 'c 'd 'e}
    #{ 'a ^&0 #{'b 'c} ^&1 ?y }
    ?y)
  )


(m/defsyntax ..+ [!rest]
  `(m/seqable ~!rest ...))


(def pa-j1 "position"
  (r/rewrite
    #{ #{#{?p ^& '#{}} ?p ^& '#{}}
      ^& (..+ !ctx) }
    #{^& (!ctx ...) }
    
    #{^& (..+ !ctx) }
    #{ #{#{'p} 'p}
      ^& (!ctx ...) }))

(def pa-j2 "transposition"
  (r/rewrite
    #{ #{#{?p ?r ^& '#{}} #{?q ?r ^& '#{}} ^& '#{}}
      ^& (..+ !ctx) }
    #{ #{#{?p} #{?q}} ?r
      ^& (!ctx ...) }

    #{ #{#{?p ^& '#{}} #{?q ^& '#{}}} ?r
      ^& (..+ !ctx) }
    #{ #{#{?p ?r} #{?q ?r}}
      ^& (!ctx ...) } ))

(def pa-c1 "reflexion"
  (r/rewrite
    #{ #{#{^& (..+ !as)} ^& '#{}}
      ^& (..+ !ctx) }
    #{^& (!as ... !ctx ...) }))

(comment
  (pa-j1 (sets [ '(a (a)) ])) ;=> #{}
  (pa-j1 (sets [  ])) ;=> #{#{p #{p}}}
  (pa-j1 (sets [ '(a b (a)) ])) ;=> #{}
  ;; context is always preserved:
  (pa-j1 (sets [ '(a (a)) '(x) 'y ])) ;=> #{y #{x}}
  ;; pa-j1 does not fail, it just adds ((p)p):
  (pa-j1 (sets [ '((a) b) ])) ;=> #{#{#{a} b} #{p #{p}}}
  (pa-j1 (sets [ '(b (b ())) ])) ;=> #{#{p #{p}} #{#{#{} b} b}}
  (pa-j1 (sets [ '(b (b) ()) ])) ;=> #{#{#{} #{b} b} #{p #{p}}}

  (pa-j2 (sets [ '((a c) (b c)) ])) ;=> #{#{#{a} #{b}} c}
  (pa-j2 (sets [ '((a) (b)) 'c ])) ;=> #{#{#{c b} #{a c}}}
  (pa-j2 (sets [ '((a c) (b d)) ])) ;=> #meander.epsilon/fail[]
  ;; no additional elements allowed:
  (pa-j2 (sets [ '((a c) (b)) 'c ])) ;=> #meander.epsilon/fail[]

  (pa-j2 (sets [ '((a) (c)) 'c ])) ; ! Execution error, Duplicate key: c
  ; would be #{#{#{a c} #{c c}}}, which has duplicate key in #{c c}

  (pa-c1 (sets [ '((a)) ])) ;=> #{a}
  ;; works on the multiple elements:
  (pa-c1 (sets [ '((a b c)) ])) ;=> #{a c b}
  (pa-c1 (sets [ '((a (b c))) 'x 'y ])) ;=> #{x #{c b} a y}

  )








