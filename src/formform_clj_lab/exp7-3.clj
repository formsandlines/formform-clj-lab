(ns formform-clj-lab.exp7-3
  (:require 
    [clojure.zip :as z]
    [meander.strategy.epsilon :as r]
    [meander.epsilon :as m]
    [formform-clj-lab.utils :refer [maps maps->forms]]))

;; 31.07.2022

;; I discovered that by matching on maps instead, I can get all the
;; benefits of a real and distinguishable rest syntax, which solves all
;; the previous issues that I had with set patterns

;; There is an immense flexibility with unordered collections that
;; just cannot be matched by any sequential pattern and Meander provides 
;; a lot of powerful solutions to almost any kind of matching problem


;; I have been able to represent all equations from the “primary algebra”
;; in GSBs “Laws of Form“ as Meander patterns and even reconstructed
;; all of the proofs by deriving each step from my defined patterns only
;; using Clojures zip library to precisely navigate the FORMs

;; It is still unclear how to represent the generalized patterns of
;; some rules in theorems 10 through 12 of LoF

;; C5 (iteration) may be the only case where a map pattern does not 
;; really work or make much sense, since it matches multiple instances
;; of elements
;; I am not yet sure how to deal with it, since the `->` case will
;; be handled by the map conversion anyway (no duplicate keys allowed)
;; and the `<-` case can only be represented with a sequential collection
;; which would break composability of rewrites (if this is even useful)


;; I had some issues with patterns of many variables where Meander likes
;; to assign multiple important terms to only one variable instead of
;; distributing them more evenly first

;; Of course this is because I mostly use submaps in patterns and how
;; would Meander know what I want to match here

;; Although the replacements are still correct, they often become useless
;; because of this...
;; The only way I found around this problem is to make sure no empty
;; maps are assigned to a variable by using `m/pred`, but I still
;; need to match cases where variables match nothing, so I somehow
;; have to tell Meander to try all non-empty cases first


(m/defsyntax ..+ [!rest]
  `(m/seqable ~!rest ...))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some convenience functions for rule application:

(defn subst
  "Substitutes `x` by applying a `rule`, which is a function that returns
  an r/rewrite applied to `args` before substitution"
  [x rule & args]
  (let [form (if (seqable? x) (maps x) x)]
    (maps->forms ((apply rule args) form))))

(defn subst-range 
  "Like `subst` but applies the `rule` from a range from (inclusive) 
  `start` to (exclusive) `end` in `ctx`"
  [ctx [start end] rule & args]
  (let [[before ss]    (split-at start ctx)
        [subctx after] (split-at end ss)]
    (concat before (maps->forms ((apply rule args) (maps subctx))) after)))

(defn subst-in 
  "Like `subst` but applies the `rule` to a subcontext matched by a
  set `match-set` which contains all elements the rule should apply to."
  [ctx match-set rule & args]
  (let [rest-ctx (remove match-set ctx)]
    (concat (maps->forms ((apply rule args) (maps match-set))) rest-ctx)))

(defn subst-repeat
  "Repeatedly substitutes `x` with `rule`."
  [x rule & args]
  ((r/top-down
     (r/repeat #(apply subst % rule args)))
   x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some convenience functions for zipper movement:

(defn find-coll-in-loc 
  "Repeats z/right until `loc` is a z/branch?."
  [loc]
  (if (or (nil? loc) (z/branch? loc))
    loc
    (recur (z/right loc))))

(defn find-in-loc-by 
  "Repeats z/right until `pred` is true for the current node at `loc`."
  [loc pred]
  (if (or (nil? loc) (and (z/branch? loc) (pred (z/node loc))))
    loc
    (recur (z/right loc) pred)))

(defn zip-top 
  "Repeats z/up from `loc` until root loc is reached."
  [loc]
  (last (take-while some? (iterate z/up loc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; equivalence rules after LoF primary algebra

(defn pa-j1-> "position: -> take out"
  []
  (r/rewrite
    { {?p _ & ?p} _ & (..+ !ctx) }
    { & (!ctx ...) }))

(defn pa-j1<- "position: <- put in"
  ([] (pa-j1<- 'p))
  ([p & ps]
   (let [p (maps (cons p ps))]
     (r/rewrite
       {& (..+ !ctx)}
       {{{& ~p} nil & ~p} nil & (!ctx ...)}))))


(defn pa-j2-> "transposition: -> collect"
  []
  (r/rewrite
    { {{&0 ?r &1 !p} _ {&0 ?r &1 !q} _ & '{}} _ & (..+ !ctx) }
    { {!p nil !q nil} nil &0 ?r &1 (!ctx ...) }))

(defn pa-j2<- "transposition: <- distribute"
  []
  (r/rewrite
    { {{& (..+ !p)} _ {& (..+ !q)} _ & '{}} _ & ?r }
    { {{& (!p ... & ?r)} nil
       {& (!q ... & ?r)} nil} nil }))

(comment
  ;; it is unclear yet how to represent generalizations of these patterns:
  ;; (see also C8 and C9)

  ;; ((a r) (b r) ...) = ((a) (b) ...) r
  ; (defn pa-j2-ext-> "extended (theorem 10) transposition: -> collect"
  ;   []
  ;   (r/rewrite
  ;     { {{&0 ?r &1 !p} _ {&0 ?r &1 !q} _ & '{}} _ & (..+ !ctx) }
  ;     { {!p nil !q nil} nil &0 ?r &1 (!ctx ...) }))

  ;; ((a) (b) ...) r = ((a r) (b r) ...)
  ; (defn pa-j2-ext<- "extended (theorem 10) transposition: <- distribute"
  ;   []
  ;   (r/rewrite
  ;     { {{& (..+ !p)} _ {& (..+ !q)} _ & '{}} _ & ?r }
  ;     { {{& (!p ... & ?r)} nil
  ;        {& (!q ... & ?r)} nil} nil }))
  )


(defn pa-c1-> "reflexion: -> reflect out"
  []
  (r/rewrite
    { {{& (..+ !a)} _ & '{}} _ & (..+ !ctx) }
    { & (!a ... !ctx ...) }))

(defn pa-c1<- "reflexion: <- reflect in"
  ([]
   (r/rewrite
     (m/pred seqable? { & (..+ !a) })
     { {{& (!a ...)} nil} nil }

     ?x
     {{?x nil} nil}))
  ([x & xs]
   (let [x (maps (cons x xs))]
     (r/rewrite
       { &0 ~x &1 (..+ !ctx) }
       { {{& ~x} nil} nil & (!ctx ...) }))))


(defn pa-c2-> "generation: -> degenerate"
  []
  (r/rewrite
    { {&0 ?b &1 (..+ !a)} nil &0 ?b &1 (..+ !ctx)}
    { {& (!a ...)} nil &0 ?b &1 (!ctx ...) }))

(defn pa-c2<- "generation: <- regenerate"
  []
  ;; ! could be ambiguous e.g. in [ (a) (b) ]
  ;; -> order matters?
  (r/rewrite
    { {& (..+ !a)} nil & ?b }
    { {&0 ?b &1 (!a ...)} nil & ?b }))


(defn pa-c3-> "integration: -> reduce"
  []
  (r/rewrite
    { {& '{}} nil }
    { '{} nil }))

(defn pa-c3<- "integration: <- augment"
  ([] (pa-c3<- 'a))
  ([a & as]
   (let [a (maps (cons a as))]
     (r/rewrite
       {{& '{}} nil & '{}}
       {'{} nil & ~a}))))


(defn pa-c5-> "iteration: -> iterate"
  []
  (r/rewrite ; ? could be rewritten without Meander
    { & ?a }
    { & ?a }))

(defn pa-c5<- "iteration: <- reiterate"
  []
  (r/rewrite ; ? how to handle composition of rewrites strategies now?
    { & ?a }
    ( &0 ?a &1 ?a )))


(defn pa-c4-> "occultation: -> conceal"
  []
  (r/rewrite
    { {{& ?a} nil & ?b} nil &0 ?a &1 (..+ !ctx) }
    { &0 ?a &1 (!ctx ...) }))

(defn pa-c4<- "occultation: <- reveal"
  ([] (pa-c4<- 'b))
  ([b & bs]
   (let [b (maps (cons b bs))]
     (r/rewrite
       { & ?a }
       { {{& ?a} nil & ~b} nil & ?a }))))


(defn pa-c6-> "extension: -> contract"
  []
  (r/rewrite
    { {{& ?a} nil {& ?b} nil & '{}} nil
      {{& ?a} nil & ?b} nil & (..+ !ctx) }
    { &0 ?a &1 (!ctx ...) }))

(defn pa-c6<- "extension: <- expand"
  ([] (pa-c6<- 'b))
  ([b & bs]
   (let [b (maps (cons b bs))]
     (r/rewrite
       {& ?a}
       {{{& ?a} nil {& ~b} nil} nil
        {{& ?a} nil & ~b} nil}))))


(defn pa-c7-> "echelon: -> break"
  []
  (r/rewrite
    { {{{& ?a} nil & ?b} nil & ?c} nil & (..+ !ctx) }
    { {&0 ?a &1 ?c} nil {{& ?b} nil & ?c} nil & (!ctx ...) }))

(defn pa-c7<- "echelon: <- make"
  []
  (r/rewrite
    { {&0 ?a &1 ?c} nil {{& ?b} nil & ?c} nil & (..+ !ctx) }
    { {{{& ?a} nil & ?b} nil & ?c} nil & (!ctx ...) }))


(defn pa-c8-> "Modified transposition: -> collect"
  []
  (r/rewrite
    ;; m/pred to avoid ?r = nil after unification of ?a, ?b and ?c:
    { {{&0 (m/pred seq ?r) &1 ?b} nil {&0 (m/pred seq ?r) &1 ?c} nil 
       {& ?a} nil & '{}} nil & (..+ !ctx) }
    { {{& ?a} nil {& ?b} nil {& ?c} nil} nil {{& ?a} nil {& ?r} nil} nil
     & (!ctx ...) }

    ;; still we have to separately allow for ?r = nil:
    { {{& ?a} nil {& ?b} nil {& ?c} nil & '{}} nil & (..+ !ctx) }
    { {{& ?a} nil {& ?b} nil {& ?c} nil} nil {{& ?a} nil '{} nil} nil
     & (!ctx ...) }))

(defn pa-c8<- "Modified transposition: <- distribute"
  []
  (r/rewrite
    { {{& ?a} nil {& ?b} nil {& ?c} nil & '{}} nil 
      {{& ?a} nil {& ?r} nil & '{}} nil & (..+ !ctx) }
    { {{& ?a} nil {&0 ?b &1 ?r} nil {&0 ?c &1 ?r} nil} nil
     & (!ctx ...) }))


(defn pa-c9-> "Crosstransposition: -> collect"
  []
  (r/rewrite
    { {{{& ?b} nil {& (m/pred seq ?r)} nil & '{}} nil
       {{& ?a} nil {& (m/pred seq ?r)} nil & '{}} nil
       {{& ?x} nil & (m/pred seq ?r)} nil
       {{& ?y} nil & (m/pred seq ?r)} nil 
       & '{}} nil & (..+ !ctx) }
    { {{& ?r} nil &0 ?a &1 ?b} nil {&0 ?r &1 ?x &2 ?y} nil }))

(defn pa-c9<- "Crosstransposition: <- distribute"
  []
  (r/rewrite
    { {{& (m/pred seq ?r)} nil &0 (m/pred seq ?a) &1 (m/pred seq ?b)} nil 
      {&0 ?r &1 ?x &2 ?y} nil & (..+ !ctx) }
    { {{{& ?b} nil {& ?r} nil} nil
       {{& ?a} nil {& ?r} nil} nil
       {{& ?x} nil & ?r} nil
       {{& ?y} nil & ?r} nil} nil }))



(comment
  ;; some tests with these patterns:

  (subst [ '(a (a)) ] pa-j1->) ;=> ()
  (subst [ '(a b c (a b c)) ] pa-j1->) ;=> ()
  (subst [ '(a b (a)) ] pa-j1->) ;=> #meander.epsilon/fail[]
  (subst [ '((a) b) ] pa-j1->) ;=> #meander.epsilon/fail[]
  (subst [ '(b (b ())) ] pa-j1->) ;=> #meander.epsilon/fail[]
  (subst [ '(b (b) ()) ] pa-j1->) ;=> #meander.epsilon/fail[]
  ;; context is always preserved:
  (subst [ '(a (a)) '(x) 'y ] pa-j1->) ;=> ((x) y)

  (subst [  ] pa-j1<-) ;=> ((p (p)))
  (subst [ 'x ] pa-j1<-) ;=> (x (p (p)))
  (subst [ 'x 'y ] pa-j1<- 'a) ;=> (x y (a (a)))
  (subst [ ] pa-j1<- 'a 'b) ;=> ((a b (a b)))
  (subst [ ] pa-j1<- '(x (y))) ;=> (((x (y)) ((x (y)))))


  (subst [ '((a c) (b c)) ] pa-j2->) ;=> (c ((a) (b)))
  (subst [ '((a c) (b d)) ] pa-j2->) ;=> (((a c) (b d)))
  (subst [ '((b a c) (a c b)) ] pa-j2->) ;=> #meander.epsilon/fail[]
  ;; ! should not fail if p and q are empty!
  (subst [ '((b a c) (a d b)) ] pa-j2->) ;=> (b a ((c) (d)))

  (subst [ '((a) (b)) 'c ] pa-j2<-) ;=> (((a c) (b c)))
  (subst [ '((a c) (b)) 'c ] pa-j2<-) ;=> (((a c) (b c)))
  (subst [ '((a) (c)) 'c ] pa-j2<-) ;=> (((a c) (c)))


  (subst [ '((a)) ] pa-c1->) ;=> (a)
  ;; works on the multiple elements:
  (subst [ '((a b c)) ] pa-c1->) ;=> (a b c)
  (subst [ '((a (b c))) 'x 'y ] pa-c1->) ;=> (a (b c) x y)

  (subst [ 'a ] pa-c1<-) ;=> (((a)))
  (subst 'a pa-c1<-) ;=> ((a))
  (subst [ 'a '(b c) ] pa-c1<-) ;=> (((a (b c))))
  (subst [ 'a 'b 'c ] pa-c1<- 'b) ;=> (a c ((b)))
  (subst [ 'a 'b 'c ] pa-c1<- 'b 'a) ;=> (c ((b a)))


  (subst [ '(a b) 'b ] pa-c2->) ;=> (b (a))
  (subst [ 'c '(a c y b x) 'a 'b ] pa-c2->) ;=> (a c b (y x))

  (subst [ '(a) 'b ] pa-c2<-) ;=> (b (b a))
  (subst [ 'c '(y x) 'a 'b ] pa-c2<-) ;=> (c a b (c a b y x))
  ;; regenerate is ambiguous, so order determines the generation:
  (subst [ '(a) '(b) ] pa-c2<-) ;=> ((b) ((b) a))
  (subst [ '(b) '(a) ] pa-c2<-) ;=> ((a) ((a) b))


  (subst [ '() 'a ] pa-c3->) ;=> (())
  (subst [ '(a) 'b ] pa-c3->) ;=> #meander.epsilon/fail[]
  (subst [ 'a '() '() 'b ] pa-c3->) ;=> (())

  (subst [ '() ] pa-c3<-) ;=> (a ())
  (subst [ '() 'a ] pa-c3<-) ;=> #meander.epsilon/fail[]
  (subst [ '() ] pa-c3<- 'x 'y 'z) ;=> (x y z ())
  (subst [ '() ] pa-c3<- '(x (y))) ;=> ((x (y)) ())


  (subst [ '((a) b) 'a ] pa-c4->) ;=> (a)
  (subst [ 'y '('y (x y) 'z) 'x ] pa-c4->) ;=> (x y)
  (subst [ 'x '('y (x y) 'z) 'x ] pa-c4->) ;=> #meander.epsilon/fail[]
  (subst [ '((())) '() ] pa-c4->) ;=> (())

  (subst [ 'a ] pa-c4<-) ;=> (a (b (a)))
  (subst [ 'b ] pa-c4<- 'a) ;=> (b (a (b)))
  (subst [ 'x '() ] pa-c4<- '()) ;=> (x () (() (x ())))
  (subst [ nil ] pa-c4<-) ;=> ((b ()))
  (subst [ ] pa-c4<- nil) ;=> ((()))


  (subst [ 'a 'a ] pa-c5->) ;=> (a)
  (subst [ 'a 'c 'b 'c 'a 'a ] pa-c5->) ;=> (a c b)
  (subst [ '(a b) 'a '(b a a) ] pa-c5->) ;=> ((a b) a)

  (subst [ 'a ] pa-c5<-) ;=> (a a)
  (subst [ '() ] pa-c5<-) ;=> (() ())
  (subst [ 'a '(x (y)) '() ] pa-c5<-) ;=> (a (x (y)) () a (x (y)) ())


  (subst [ '((a) (b)) '((a) b) ] pa-c6->) ;=> (a)
  (subst [ '((a) (b)) '((a) (b)) ] pa-c6->) ;=> #meander.epsilon/fail[]
  (subst [ '((a b) (b)) '((a) b) ] pa-c6->) ;=> #meander.epsilon/fail[]
  (subst [ '((a) (b) c) '((a) b) ] pa-c6->) ;=> #meander.epsilon/fail[]
  (subst [ '((a) (b)) '((a) b) 'c ] pa-c6->) ;=> (a c)
  (subst [ '((x y) (() (() ()))) '((x y) () (() ())) ] pa-c6->) ;=> (x y)
  (subst [ '((x) ()) '((x) ()) ] pa-c6->) ;=> #meander.epsilon/fail[] 

  (subst [ 'a ] pa-c6<-) ;=> (((a) (b)) (b (a)))
  (subst [ 'x 'y ] pa-c6<- 'z) ;=> (((x y) (z)) (z (x y)))
  (subst [ '() ] pa-c6<- '() '(())) ;=> (((()) (() (()))) (() (())))


  (subst [ '(((a) b) c) ] pa-c7->) ;=> ((a c) (c (b)))
  (subst [ '(((a))) ] pa-c7->) ;=> ((a) (()))
  (subst [ '((())) ] pa-c7->) ;=> (() (()))

  (subst [ '(a c) '((b) c) ] pa-c7<-) ;=> ((c (b (a))))
  (subst [ '(a) '(()) ] pa-c7<-) ;=> ((((a))))
  (subst [ '() '(()) ] pa-c7<-) ;=> (((())))


  (subst [ '((a) (b r) (c r)) ] pa-c8->) ;=> (((a) (b) (c)) ((a) (r)))
  (subst [ '((a) (b) (c)) ] pa-c8->) ;=> (((a) (b) (c)) ((a) ()))

  (subst [ '((a) (b) (c)) '((a) (r)) ] pa-c8<-) ;=> (((a) (b r) (c r)))

  
  (subst [ '(((b) (r)) ((a) (r)) ((x) r) ((y) r)) ] pa-c9->)
  ;=> ((a b (r)) (r x y))
  
  (subst [ '((r) a b) '(r x y) ] pa-c9<-)
  ;=> ((((a) (r)) ((b) (r)) (r (y)) (r (x))))

  )
(comment
  ;; more precise substitutions can be performed with utility functions:

  (subst-in [ 'a 'b 'c ] #{'b 'c} pa-c1<-) ;=> (((c b)) a)
  ;; equivalently:
  (subst [ 'a 'b 'c ] pa-c1<- 'b 'c) ;=> (a ((b c)))
  (subst-in [ '(a (a)) '(x y (x y)) ] #{'(x y (x y))} pa-j1->) ;=> ((a (a)))

  (subst-range [ '(a (a)) '(x y (x y)) ] [1 2] pa-j1->) ;=> ((a (a)))
  (subst-range [ '(a (a)) '(x y (x y)) ] [0 1] pa-j1->) ;=> ((x y (x y)))

  )
(comment
  ;; to apply a rule to an inner part of the FORM
  ;; we can use a traversing strategy (like r/bottom-up)

  (subst [ '(a (b (c) c) (x (x y) y)) 'b ] pa-j1->)
  ;=> #meander.epsilon/fail[]

  ((r/bottom-up
     (r/attempt #(subst % pa-j1->)))
   [ '(a (b (c) c) (x (x y) y)) 'b ])
  ;=> [(a (b (c) c)) b]

  ((r/top-down
     (r/attempt #(subst % pa-c1->)))
   [ '(a ((b (c ((d)) e)) f)) ])
  ;=> [(a ((b (d c e)) f))]

  ;; same result in bottom-up substitution:
  ((r/bottom-up
     (r/attempt #(subst % pa-c1->)))
   [ '(a ((b (c ((d)) e)) f)) ])
  ;=> [(a ((b (d c e)) f))]

  ;; we can even follow the exact substitution steps with r/trace:
  ((r/bottom-up
     (r/trace (r/attempt #(subst % pa-c1->))))
   [ '(a (((b) x))) ])
  ;=> [((b) x a)]

  ;; r/some can be helpful to search for matches in rule application:
  ((r/some
     #(subst % pa-c1->))
   [ '(a ((b c) d)) '(e ((f g))) ])
  ;=> [(a ((b c) d)) (f g e)]


  ((r/all
     #(subst % pa-c1<-))
   [ 'a '(b (c)) '((d)) ])
  ;=> [((a)) (((b (c)))) ((((d))))]

  ((r/all
     #(subst % pa-c1<-))
   [ '(a) ])
  ;=> [(((a)))]

  (subst 'a pa-c1<-) ;=> ((a))
  (maps->forms ((pa-c1<-) 'a)) ;=> ((a))
  (subst [ 'a ] pa-c1<-) ;=> (((a)))
  (subst [ 'a 'b ] pa-c1<-) ;=> (((a b)))


  ;; r/repeat and r/n-times are great if rules can be applied more often
  ((r/repeat
     (r/trace #(subst % pa-c1->)))
   [ '((((a ((x)))) b)) ])
  ;=> (x a b)

  ((r/n-times 2
     #(subst % pa-c1->))
   [ '((((a ((x)))) b)) ])
  ;=> (a ((x)) b)

  ;; through piping, multiple rules can be applied successively

  ((r/pipe
     (r/trace #(subst % pa-c1->))
     (r/trace #(subst % pa-c2->))
     (r/trace #(subst % pa-c3->))
     (r/pred #(= % '(()))))
   [ '(((a a) a b)) ])
  ;=> (())

  ;; r/choice will try to match one of the specified rewrite rules:
  ((r/choice
     (r/trace #(subst % pa-c3->))
     (r/trace #(subst % pa-c1->))
     (r/trace #(subst % pa-c2->)))
   [ '(((a a) a b)) ])
  ;=> ((a) a b)

  ;; however, this will not work with proofs that require a more
  ;; constructive approach, where the pattern has to be built first
  ;; by selectively applying rules rather than brute-forcing a match

  ;; there certainly may be strategic/heuristic approaches which automatic
  ;; theorem solver or proof assistents apply, but they are far
  ;; beyond the abilities of simple term rewriting

  ;; I can, however, provide selective rule application to the user
  ;; maybe even with a visual interface to select parts of a FORM
  )

(comment
  ;; using Clojures zip library, we can walk the FORM and specify
  ;; where exactly a replacement rule shall be applied


  ;; it is important to always substitute on a context,
  ;; NOT on the FORM itself, since my rewrite rules take context
  ;; into account and return in a new context

  ;; therefore, to substitute more selectively in a context
  ;; `subst-in` can be given a set of all elements in the context
  ;; that should be considered for the rewrite rule

  ;; this is useful, since for example `pa-c1<-` can wrap
  ;; any element, but we sometimes want only a specific selection
  ;; and otherwise it would just wrap the entire context
  ;; (I added term selection to the rule itself so this is now obsolete)

  ;; of course, every FORM is a context, but the outermost
  ;; context is the unmarked FORM that does not evaluate
  ;; so we just ignore it, but it has to be merged in composition

  ;; Demonstration of “Consequence 1. Reflexion”:
  (let [zz (z/seq-zip '( ((a)) ))
        s1 (z/edit zz #(subst % pa-j1<- '(a)))
        s2 (z/edit s1 #(subst % pa-j2<-))
        s3 (z/edit (z/down s2) #(subst % pa-j1->))
        s4 (z/edit s3 #(subst % pa-j1<- 'a))
        s5 (z/edit (z/up s4) #(subst % pa-j2->))
        s6 (z/edit s5 #(subst % pa-j1->))
        ]
    {:res (z/root s6)
     :steps (map z/root [zz s1 s2 s3 s4 s5 s6])})
  ;=> {:res (a),
  ;    :steps
  ;    ((((a)))
  ;     (((a)) ((a) ((a))))
  ;     (((a ((a))) ((a) ((a)))))
  ;     (((a ((a)))))
  ;     (((a ((a))) (a (a))))
  ;     (a ((((a))) ((a))))
  ;     (a))}

  ;; Demonstration of “Consequence 2. Generation”:
  (let [zz (z/seq-zip '( (a b) b ))
        s1 (z/edit (z/down zz) #(subst % pa-c1<- 'a))
        s2 (z/edit s1 #(subst % pa-c1<- 'b))
        s3 (z/edit (z/up s2) #(subst % pa-j2<-))
        s4 (z/edit (z/down s3) #(subst % pa-j1->))
        s5 (z/edit (z/up s4) #(subst % pa-c1->))]
    {:res (z/root s5)
     :steps (map z/root [zz s1 s2 s3 s4 s5])})
  ;=> {:res ((a) b),
  ;    :steps
  ;    (((a b) b)
  ;     ((((a)) b) b)
  ;     ((((b)) ((a))) b)
  ;     ((((b) b) ((a) b)))
  ;     ((((a) b)))
  ;     ((a) b))}

  ;; Demonstration of “Consequence 3. Integration”:
  (let [zz (z/seq-zip '( () a ))
        s1 (z/edit zz #(subst % pa-c2<-))
        s2 (z/edit s1 #(subst % pa-c1<-))
        s3 (z/edit (z/down s2) #(subst % pa-j1->))]
    {:res (z/root s3)
     :steps (map z/root [zz s1 s2 s3])})
  ;=> {:res (()), 
  ;    :steps 
  ;    ((() a) 
  ;     (a (a)) 
  ;     (((a (a)))) 
  ;     (()))}

  ;; Demonstration of “Consequence 4. Occultation”:
  (let [zz (z/seq-zip '( ((a) b) a ))
        s1 (z/edit zz #(subst % pa-c2<-))
        ;; needs search for the FORM because order of elements is shuffled
        s2 (z/edit (find-coll-in-loc (z/down s1)) #(subst % pa-c2<-))
        s3 (z/edit (z/up s2) #(subst % pa-j1->))]
    {:res (z/root s3)
     :steps (map z/root [zz s1 s2 s3])})
  ;=> {:res (a), 
  ;    :steps 
  ;    ((((a) b) a) 
  ;     (a (a (a) b)) 
  ;     (a (a b (a b))) 
  ;     (a))}

  ;; Demonstration of “Consequence 5. Iteration”:
  (let [zz (z/seq-zip '( a a ))
        s1 (z/edit zz #(subst-range % [0 1] pa-c1<-))
        s2 (z/edit s1 #(subst % pa-c4->))]
    {:res (z/root s2)
     :steps (map z/root [zz s1 s2])})
  ;=> {:res (a),
  ;    :steps 
  ;    ((a a) 
  ;     (((a)) a) 
  ;     (a))}

  ;; Demonstration of “Consequence 6. Extension”:
  (let [zz (z/seq-zip '( ((a) (b)) ((a) b) ))
        s1 (z/edit zz #(subst % pa-c1<-))
        s2 (z/edit (z/down s1) #(subst % pa-j2->))
        s3 (z/edit s2 #(subst % pa-j1->))
        s4 (z/edit (z/up s3) #(subst % pa-c1->))]
    {:res (z/root s4)
     :steps (map z/root [zz s1 s2 s3 s4])})
  ;=> {:res (a),
  ;    :steps
  ;    ((((a) (b)) ((a) b))
  ;     (((((a) (b)) ((a) b))))
  ;     (((a) (((b)) (b))))
  ;     (((a)))
  ;     (a))}

  ;; Demonstration of “Consequence 7. Echelon”:
  (let [zz (z/seq-zip '( (((a) b) c) ))
        s1 (z/edit (find-coll-in-loc (z/down (z/down zz)))
             #(subst % pa-c1<- 'b))
        s2 (z/edit (z/up s1) #(subst % pa-j2<-))
        s3 (z/edit (z/up s2) #(subst % pa-c1->))]
    {:res (z/root s3)
     :steps (map z/root [zz s1 s2 s3])})
  ;=> {:res (((b) c) (a c)),
  ;    :steps
  ;    (((((a) b) c)) 
  ;     (((((b)) (a)) c)) 
  ;     (((((b) c) (a c)))) 
  ;     (((b) c) (a c)))}

  ;; Demonstration of “Consequence 8. Modified transposition”:
  (let [zz (z/seq-zip '( ((a) (b r) (c r)) ))
        s1 (z/edit (z/down zz) #(subst % pa-c1<- '(b r) '(c r)))
        ;; since both elements in the context are FORMs, we need
        ;; more sophisticated pattern matching to select the right one
        s2 (z/edit (find-in-loc-by (z/down s1)
                     (r/match ((& _)) true _ false))
             #(subst % pa-j2->))
        s3 (z/edit (z/up (z/up s2)) #(subst % pa-c7->))]
    {:res (z/root s3)
     :steps (map z/root [zz s1 s2 s3])})
  ;=> {:res (((b) (c) (a)) ((a) (r))),
  ;    :steps
  ;    ((((a) (b r) (c r)))
  ;     (((((b r) (c r))) (a)))
  ;     (((r ((b) (c))) (a)))
  ;     (((b) (c) (a)) ((a) (r))))}

  ;; Demonstration of “Consequence 9. Crosstransposition”:
  (let [zz (z/seq-zip '( (((b) (r)) ((a) (r)) ((x) r) ((y) r)) ))
        s1-1 (z/edit (z/down zz) #(subst % pa-c1<- '((x) r) '((y) r)))
        s1-2 (z/edit (find-in-loc-by (z/down s1-1)
                       (r/match ((& _)) true _ false))
               #(subst % pa-j2->))
        ;; since there are now multiple nested C1 applications,
        ;; we can avoid moving around by using a Meander strategy 
        s1-3 (z/edit s1-2 #(subst-repeat % pa-c1->))
        s2-1 (z/edit (zip-top s1-3) #(subst % pa-c8->))
        s2-2 (z/edit s2-1 #(subst-repeat % pa-c1->))
        s3-1 (z/edit (find-in-loc-by (z/down s2-2)
                       #(contains? (set %) 'r))
               #(subst % pa-c2->))
        s3-2 (z/edit s3-1 #(subst % pa-c1->))
        ;; ! how to ensure correct order for regenerate rule?
        s4   (z/edit (z/up s3-2) #(subst % pa-c2<-))
        s5-1 (z/edit (find-in-loc-by (z/down s4)
                       #(contains? (set %) 'a))
               (fn [x] ((r/bottom-up
                          (r/attempt #(subst % pa-c1<- 'r)))
                        x)))
        s5-2 (z/edit s5-1 #(subst % pa-c6->))]
    {:res (z/root s5-2)
     :steps (map z/root [zz s1-3 s2-2 s3-2 s4 s5-2])})
  ;=> {:res ((y x r) ((r) a b)),
  ;    :steps
  ;    (((((b) (r)) ((a) (r)) ((x) r) ((y) r)))
  ;     ((((b) (r)) ((a) (r)) (r (y x))))
  ;     ((a b (r (y x))) (r (r (y x))))
  ;     ((a b (r (y x))) (y x r))
  ;     ((y x r) ((y x r) a b (r (y x))))
  ;     ((y x r) ((r) a b)))}

  )

#_(comment
  ;; trying to tackle the empty assignment problem,
  ;; but no success so far

  (m/rewrites
    (maps '((b r) (a) (c r)))
    { {& ?x} nil {&0 ?r &1 ?y} nil {&0 ?r &1 ?z} nil }
    (m/app (partial map maps->forms) [?x ?y ?z ?r]))

  ;; yields all these combinations...:
  #_(((b r) (a)   (c r) ())
     ((b r) (c r) (a)   ())
     ((a)   (b)   (c)   (r))
     ((a)   (b r) (c r) ())
     ((a)   (c)   (b)   (r))
     ((a)   (c r) (b r) ())
     ((c r) (b r) (a)   ())
     ((c r) (a)   (b r) ()))

  ;; this does not work:
  (m/rewrites
    (maps '((b r) (a) (c r)))
    (m/with [%ptn { {& ?x} nil {&0 ?r &1 ?y} nil {&0 ?r &1 ?z} nil }
             %var (m/or (m/pred seq? ?r ?x ?y ?z)
                    (m/and (m/pred seq? ?r ?x ?y) (m/pred nil? ?z))
                    (m/and (m/pred seq? ?r ?x) (m/pred nil? ?y ?z))
                    (m/and (m/pred seq? ?r) (m/pred nil? ?x ?y ?z))
                    (m/and (m/pred seq? ?x ?y ?z) (m/pred nil? ?r))
                    (m/and (m/pred seq? ?y ?z) (m/pred nil? ?r ?x))
                    (m/and (m/pred seq? ?z) (m/pred nil? ?r ?x ?y))
                    )
             %res (m/and %var %ptn)]
      %res)
    (m/app (partial map maps->forms) [?x ?y ?z ?r]))
  )

