(ns formform-clj-lab.exp4-3
  (:require [clojure.walk :as walk]))

;; 16.06.2022

;; In this variation of exp4-2, I switched to a symbolic representation
;; for `u` and included the definition as metadata (I know this is kind
;; of silly, but I just like having it around)

;; I don’t really know where to go from here, especially since this
;; approach is not really efficient for evaluation (see comment below).

(comment
  ;; Issue with this approach:

  ;; evaluation of forms in Clojure is leftmost-innermost:
  (defn foo [n & _] (println n))
  (foo 0 (foo 10 (foo 20)) (foo 11 (foo 21)))
  ;; see also: https://clojure.org/reference/evaluation

  ;; this is a problem for ·, because it would be much more efficient
  ;; to evaluate the outer FORMs first in case there is a (·)
  ;; and also because a lot of simplification can be done beforehand

  ;; but this is not possible as innermost expressions have to be
  ;; evaluated first.

  ;; what if · is a macro that manipulates form-as-data algebraically
  ;; before passing them to evaluation?

  ,)


;; VALUE

(def n `nil)
(def m `(·))
(def ^{'f '(· (· f))} u :mn) ; original def. remains as metadata
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

; (defn ·
;   "(to) focus/mark: (…)"
;   ([]  m)
;   ([f] (condp = f
;          m n
;          n m
;          u i
;          i u
;          `(· ~f)))
;   ([f & r]
;    (let [condense (comp (partial remove nil?) set)
;          cnt (condense (cons f r))]
;      (cond
;        (< (count cnt) 2) (apply · cnt)
;        (some #{m} cnt) n
;        (and (some #{u} cnt)
;             (some #{i} cnt)) n
;        :else `(· ~@cnt)))))


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

  (eval u) ;=> :mn
  (eval i) ;=> (· :mn)
  (eval `(· u)) ;=> (· :mn)
  (eval `(· i)) ;=> (· (· :mn)) ; should be :mn ?

  ((meta (var u)) 'f) ;=> (· (· f))

  (eval '(· (· i))) ;=> (· (· (· :mn)))
  (eval 'i) ;=> (· :mn)

  (· i) ;=> (· (· :mn))

  )
(comment
  ;; not sure where I wanted to go with this:

  (defn form [& cnt] `(· ~@cnt))

  (defn expr [& forms] `(·· ~@forms))

  (= (expr (form m n) (form n)) 
     `(·· (· (·) nil) (· nil)) )

  (= (expr (form (form n) (form m))) 
     `(·· (· (· nil) (· (·))))) 
 )

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
  ;; from exp4-1:

  ;; variables are just bindings:
  (let [a m
        b n
        form `(· (· ~a)(· ~b))]
    (eval form))

  ;; value tables are trivial:
  ;; ! does not simplify completely with `u`/`i`
  (into
    {}
    (for [a fvals
          b fvals
          :let [form `(·(· ~a) ~b)]]
      [[a b] (eval form)]))

  (= (·(· u) n)
     (eval `(·(· ~u) ~n))) ;=> true
  

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

