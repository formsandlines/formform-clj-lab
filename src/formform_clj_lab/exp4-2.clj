(ns formform-clj-lab.exp4-2
  (:require [clojure.walk :as walk]))

;; 16.06.2022

;; Tried to introduce undetermined values to exp4-1 in a more explicit,
;; self-evaluating way like I did with `m`.

;; It is just an experiment with no real practical value, since
;; I wanted to represent the uFORM definition `f = ((f))` as the
;; concrete meaning of the symbol `u`.
;; But the output of this “value” is just too verbose to work with.

;; Tried different approaches for lazy/incremental evaluation, but unsure 
;; what is most aligned with the calculus and I don’t even know yet where 
;; I would use this. The goal here is to work out a more natural 
;; representation of self-equivalent re-entry FORMs as described in 
;; uFORM iFORM and maybe even a more efficient algorithm based on that.


;; VALUE

(def n `nil)
(def m `(·))
;; doesn’t work well with eval and too cumbersome to deal with:
;; -> just use 'u / 'i ?
(def u ((fn f [] `(<· [(~f) n] n))))
(def i `(· ~u))

(def fvals [n u i m])

(def n? nil?)
(def m? (partial = m))
(def u? (partial = u))
(def i? (partial = i))


;; FORM

(defn ·
  "(to) focus/mark: (…)"
  ([]  m)
  ([f] (condp = f
         m n
         n m
         u i
         i u
         `(· ~f)))
  ([f & r]
   (let [condense (comp (partial remove nil?) set)
         cnt (condense (cons f r))]
     (cond
       (< (count cnt) 2) (apply · cnt)
       (some #{m} cnt) n
       (and (some #{u} cnt)
            (some #{i} cnt)) n
       :else `(· ~@cnt)))))


;; Syntactic sugar

(defmacro ··
  "group macro: ((…))"
  [& cnt]
  `(· (· ~@cnt)))

(defmacro <·
  "nest-to-left macro: (((…)a)b)
  use brackets [x y …] to group expressions on the same level"
  [& exprs]
  (let [v (vec exprs)
        x (first v)]
    (loop [s (if (vector? x) `(· ~@x) `(· ~x))
           r (rest v)]
      (if (empty? r)
        s
        (let [x (first r)]
          (recur (if (vector? x) `(· ~s ~@x) `(· ~s ~x))
                 (rest r)))))))

(defmacro ·>
  "nest-to-right macro: (a(b(…)))
  use brackets [x y …] to group expressions on the same level"
  [x & r]
  (let [s (vec r)]
    (if (empty? s)
      (if (vector? x) `(· ~@x) `(· ~x))
      (if (vector? x) `(· ~@x (·> ~@r)) `(· ~x (·> ~@r))))))



(comment
  ;; from exp4-1:

  ;; operation <=> (symbolic) expression
  (eval `(·)) ;=> (·)
  (= (eval `(·)) (eval (eval `(·)))) ;=> true

  ;; nothing evaluates to nothing
  (eval `nil) ;=> nil
  (= (eval `nil) (eval (eval `nil))) ;=> true

  ;; Axiom 1. The law of calling
  (= (· (·) (·)), (· (·)) ) ;=> true
  (= (· nil nil), (· nil) ) ;=> true

  ;; Axiom 2. The law of crossing
  (= (· (·)), nil ) ;=> true

  ;; the function itself is not the mark, it must be performed
  (= `· `(·)) ;=> false
  (= (eval `·) (eval `(·))) ;=> false

  ,)

(comment

  ;; variables are just bindings:
  (let [a m
        b n
        form `(· (· ~a)(· ~b))]
    (eval form)) ;=> nil

  ;; value tables are trivial:
  ;; ! fix: INCORRECTLY EVALUATED for u/i with eval
  ;; ! 
  (into
    {}
    (for [a fvals
          b fvals
          :let [form `(·(· ~a) ~b)]]
      [[a b] (eval form)]))
  ;; INCORRECT:
  (eval `(·(· ~u) ~n))
  ;; CORRECT:
  (·(· u) n)

  ;; FORMs can be defined as more general symbolic expressions:
  (defn ·rel [a b] `(·· ~a ~b))
  (defn ·and [a b] `(· (· ~a)(· ~b)))

  ;; and evaluated using Clojures eval function:
  (eval (·rel m n)) ;=> (·)
  (eval (·and m n)) ;=> nil

  ;; or composed with other FORMs:
  (defn ·nand [a b] `(· ~(·and a b)))

  (let [x (·nand m n)]
    (println x) ;; (· (· (· (·)) (· nil)))
    (eval x)) ;;=> (·)
  
  ,)

(comment

  ;; quoting can be useful to evaluate recursive expressions safely:
  (let [re-form ((fn f [] `(· (~f))))]
    (take 3 (iterate eval re-form)))
  ;=> ((· (#function[eval6961/f--6962]))
  ;    (· (· (#function[eval6961/f--6962])))
  ;    (· (· (· (#function[eval6961/f--6962])))))

  ;; nesting macros can be used to build self-equivalent re-entry FORMs:
  (let [re-form ((fn f [] `(<· [(~f) n] n)))]
    (take 3 (iterate eval re-form)))
  ;=> ((<· [(#function[eval6866/f--6867]) n] n)
  ;    (· (· (<· [(#function[eval6866/f--6867]) n] n) nil) nil)
  ;    (· (· (· (· (<· [(#function[eval6866/f--6867]) n] n) nil) nil) nil) nil))


  ;; a different approach using symbol substitution by walking the form:
  (let [re-form '{f (· f)}]
    (take 3 (iterate #(walk/postwalk-replace re-form %) 'f)))
  ;=> (f (· f) (· (· f)))

  (let [re-form '{f (<· [f a] b)}]
    (take 4 (iterate #(walk/postwalk-replace re-form %) 'f)))
  ;=> (f
  ;    (<· [f a] b)
  ;    (<· [(<· [f a] b) a] b)
  ;    (<· [(<· [(<· [f a] b) a] b) a] b))


  ;; here is a more indirect functional approach building a lazy seq of FORMs:
  (defn re-lazy [x]
    (lazy-seq (cons x (re-lazy `(· ~x)))))

  (last (take 5 (re-lazy n)))
  ;=> (· (· (· (· nil))))


  ,)

