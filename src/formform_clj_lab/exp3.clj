(ns formform-clj-lab.exp3)

;; 06.06.2022

;; A FORM can be just a seq with an outer wrapper as the /unmarked mark/
;; - the wrapper is necessary to handle relations as FORMs
;; - this makes composition much cleaner compared to the double mark
;; - think of [] == (()) (I’ve been using brackets for groups long ago)

;; This is as simple as it gets in terms of representation with Clojure data
;; - evaluation is pretty simple and parsing should be trivial
;; - since '() != nil, this can be our first distinction (not in other Lisps!)

;; Quotation makes it easy to pass around and compose FORMs as data
;; - wrappers can be easily spliced using ~@f

;; Destructuring and pattern matching can be very fruitful here
;; - even more so with https://github.com/clojure/core.match

;; There is of course no need to match Clojure syntax with FORM notation
;; - self-equivalent re-entry FORMs cannot be represented 1:1, since they need special annotations and constraints
;; - it might prove to be inefficient to use lists instead of vectors for FORMs at some point
;; - macros may help to simplify more complicated stuff, but would also introduce more layers between the code and FORM


(declare reduce-form)

(defn- reduce-expr [x env]
  (cond
    (nil?  x) x
    (coll? x) (let [vs (reduce-form x env)]
                (case vs
                  ( nil (()) ((nil)) ) nil
                  vs))
    :else (if-let [interpr (env x)]
            (let [vs (reduce-form interpr env)]
              (if (<= (count vs) 1)
                (first vs)
                (list vs)))
            x)))

(defn- reduce-form [ctx env]
  (->> ctx
       (map #(reduce-expr % env))
       set
       (remove nil?)))

(defn f-eval
  ([form] (f-eval form {}))
  ([form env]
   (let [f (reduce-form form env)
         v (when (<= (count f) 1)
             (case (first f)
               []  'm
               nil 'n
               nil))]
     (if (some? v)
       v
       {:form f :env env}))))



(def m `[ () ])
(def n `[ nil ]) ; use '_ as an alternative?
(defn v [sym] [ sym ]) ; variables as symbols must also be wrapped


;; some common FORM patterns/functions:

(defn f-inv [a] `[ (~@a) ])

(defn f-or  [a b] `[ ~@a ~@b ])
(defn f-and [a b] `[ ((~@a)(~@b)) ])
(defn f-equiv [a b] `[ (~@(f-or a b))~@(f-and a b) ])

(defn f-dec [a b] `[ ((~@a) ~@b) ])


;; re-entry experiments:

;; could include a local env as metadata for function definitions
;; - ? merge env metadata with user env in evaluation
(def re-meta (with-meta `[ ((f a) b) ] {:env {`f `[ ((f a) b) ]}}))

;; could use a map to map symbol -> FORM more directly
(def re-map `[ {:f [ ((:f a) b) ]} ])
;; or as specification for self-equivalent re-entry FORMs
(def seq-re-map
  `[ {:re/seq ( a, b )
      :re/parity any :re/unmarked false :re/interpr rec-ident} ])

;; SeqREs could also become records
(defrecord SeqRE [parity unmarked? interpr fseq])
;; which would simplify notation and constrain definition:
(def seq-re-rec `[ ~(->SeqRE 'any true 'rec-ident `( a, (b) )) ])

;; re-entry as named function?

(defn re-dec [a b]
  (fn f [] `[ ((~@f a) b) ]))

;; with lazy evaluation?
(defn re-entry [f x]
  (lazy-seq (cons x (re-entry f (f x)))))
;; can be used as an iterative FORM builder:
(take 3 (re-entry (fn [x] `[ (~@x) ]) `[ () ] ))
(take 3 (re-entry (fn [x] `[ () ~@x ]) `[ () ] ))
(take 3 (re-entry (fn [x] `[ ((~@x ~'a) ~'b) ]) `[ nil ] ))


(comment

  (reduce-form `[ ((a)) ] {`a `[ () (b) ], `b m})

  (f-eval `[ (() ()) () (()) ])
  (f-eval `[ (()) () (()) (()) ])
  (f-eval (f-inv (f-inv m)))
  (f-eval (f-or (f-or m n) n))
  (f-eval (f-equiv m n))

  (f-eval `[ a ] {`a n})

  (f-eval `[ a (b) ] {`a n, `b m})
  (f-eval (f-dec (v `a) (v `b)) {`a `[ (b) ] , `b n})


  (f-eval (f-dec (f-and m n) n))

  (f-eval *1)

  (f-inv m)
  (f-or m (f-or (f-and n n) m))
  (f-equiv n n)


  ,)
