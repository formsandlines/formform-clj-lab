(ns formform-clj-lab.exp1)

;; ~11/2021 (in Scheme)

;; The logical value as a positional selector function
;; - it can be composed to select logical values via logical values
;; - it can be given symbols/numbers/etc. to interpret its value

;; It is a way to implement a logical calculus using just lambda functions
;; - this way, it can replace/implement conditional forms
;; - I don’t think it has much practical value over conventional approaches

;; Combined, these selectors work like formDNA


(def n (fn [x y z w] x))
(def u (fn [x y z w] y))
(def i (fn [x y z w] z))
(def m (fn [x y z w] w))

(defn inv [fx] (fx m i u n))

(defn rel
  ([fx fy]
   (fx
     (fy n u i m) ; n y
     (fy u u m m) ; u y
     (fy i m i m) ; i y
     m))          ; m y
  ([fx fy & more]
   (case (count more)
     0 (rel fx fy)
     1 (rel fx (rel fy (first more)))
     (rel fx (rel fy (apply rel more))))))

(defn feval
  ([f] (feval f ['n 'u 'i 'm]))
  ([f [n u i m]] (f n u i m)))


(comment
  ((inv n) 'n 'u 'i 'm)
  ((inv n) 0 1 2 3)
  ((inv u) 0 1 2 3)
  ((inv i) 0 1 2 3)
  ((inv m) 0 1 2 3)

  (rel u i)
  (feval (rel n i n i))

  (rel (inv u) (rel (inv m) i))
  (feval *1)
  (feval (inv u) [0 1 2 3])

  ,)

(comment
  ((inv n) 'n 'm 'u 'i)
  ((inv n) 0 2 3 1)
  ((inv m) 0 2 3 1)
  ((inv u) 0 2 3 1)
  ((inv i) 0 2 3 1)

  ,)




(comment
  ;; boolean logic with fns

  (def t (fn [x y] x))
  (def f (fn [x y] y))

  (def lneg (fn [bx] (bx f t)))

  (def land (fn [bx by] (bx (by t f) f)))
  (def lor (fn [bx by] (bx t (by t f))))

  (def xor (fn [bx by] (lneg (lor bx by))))


  ((lneg t) 't 'f)
  ((lneg t) 1 0)
  ((land (lor f t) t) 't 'f)

  ,)
