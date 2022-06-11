(ns formform-clj-lab.exp2)

;; ~01/2022 (in Scheme)

;; The form as its own evaluation function for a given environment/interpr.
;; - it can be composed with other forms
;; - it can be partially or not at all interpreted and reduced

;; It is a very interesting implementation for form evaluation
;; however, this makes form-as-data very complicated:
;; - structural manipulation of fn-forms is hard
;; - cannot use pattern matching except for evaluated expressions
;; - evaluated expressions cannot be composed with forms

(defn some-has? [item s]
  ((set s) item))

(defn every-has? [item s]
  (every? #(= % item) s))

(defn ctx [& forms]
  (fn [env]
    (let [vs (map #(% env) forms)]
      (cond
        (or (empty? vs)
            (every-has? '() vs)) '()
        (some-has? 'mark vs)     'mark  ; law of calling
        :else vs))))

(defn mark [& forms]
  (fn [env]
    (let [v ((apply ctx forms) env)
          _ (println v)]
      (case v
        []   'mark   ; [] = '() in case tests
        mark '()
        (if (and (coll? v) (== 1 (count v))  ; law of crossing
                 (coll? (first v)) (== 1 (count (first v))))
          (let [[[v']] v]
            v')
          v)))))

(defn v [sym]
  (fn [env]
    (let [v (get env sym)]
      (if (some? v)
        (v env)
        sym))))

(def n (ctx))
(def m (mark))



(defn f-dec [va vb]
  ((mark (mark (v 'a)) (v 'b)) {'a va, 'b vb}))

(defn f-re []
  ((v 'f) {'f (mark (v 'f))}))


(comment
  
  ((mark (mark (v 'a) (v 'b))) {})
  (f-dec m n)
  (f-re) ;=> StackOverflowError

  ((mark (v 'a) (mark (v 'b) (mark))) {})

  (let [f1 ((v 'a) {})
        f2 ((mark (mark (v 'a))) {})]
    (= f1 f2))

  (m {})
  (n {})
  ((mark) {})
  ((ctx) {})
  
  ((v 'x) {})
  ((v 'x) {'x m})
  ((v 'x) {'x n})
  
  ((mark m) {})
  ((mark n) {})
  ((ctx m) {})
  
  ,)

