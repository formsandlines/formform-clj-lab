(ns formform-clj-lab.exp4-1
  (:require [clojure.walk :as walk]))

;; 16.06.2022

;; This combines my symbolic approach (exp3) with my functional one (exp2).

;;  `·`  is a mark or focus point; an indication of value.
;;       It must have a context to refer to (think of a physical mark).
;; `( )` alone has no meaning here, it does not signify a difference.
;;       It would ideally be the mark too, but for practical reasons,
;;       we make this additional distinction.
;; `(·)` is the FORM:
;;       its content differs from its context by indication of the mark `·`.

;; This additional distinction might not be necessary in Clojure (see exp3),
;; but has the advantage that we can use the mark as an indication
;; as well as a distinction in the operative sense (by function application).

;; This way, (·) evaluates itself and needs no eval function walking over
;; its expression tree.
;; - FORMs can be quoted to be passed as data and unquoted to be evaluated
;; - expressions that cannot be simplified are returned in quotation
;; - the value distinction is between `(·)` and `nil`, similar to exp3

;; Parsing is not quite so trivial as in exp3, because of the `·`,
;; but it should still be very simple.


;; Unlike exp2, FORMs don’t return functions for envs, so there are no
;; convoluted function expressions, making FORM-as-data easier.
;; - variables can be simple Clojure bindings, no need for custom envs
;; - evaluated expressions can actually be composed with other FORMs

;; Macros can help simplify common forms like nested sequences,
;; without having to implement special parsers.

;; `<>` may be a way to group forms without doing the operation twice,
;; - it introduces a further distinction that may not be necessary
;; - for `((…))`, I created the `··` macro


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
    (eval form))

  ;; value tables are trivial:
  ;; ! fix: INCORRECTLY EVALUATED for u/i with eval
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

  ;; nested FORM sequences in both directions are easy to write with macros:
  (walk/macroexpand-all '(<· m n n)) ;;=> (· (· (· m) n) n)
  (walk/macroexpand-all '(·> m n n)) ;;=> (· m (· n (· n)))
  (walk/macroexpand-all '(<· m)) ;;=> (· m)
  ;; can also splice in an unmarked context:
  (walk/macroexpand-all '(·> n [n m] n)) ;;=> (· n (· n m (· n)))

  ;; however, macros will not be replaced with their expansions
  (= `(·> ~m ~n), `(· ~m (· ~n))) ;=> false
  `(<· ~m ~n ~n) ;=> (<· (·) nil nil)
  ;; because they are just part of the Clojure syntax
  (= (·> m n), (· m (· n))) ;=> true
  (eval `(· (·> m 
                (<· n m n)
                [n (·· m)])
           n)) ;=> (·)
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

