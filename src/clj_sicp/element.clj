(ns clj-sicp.element
  (:require [clojure.spec.alpha :as s]
                                        ;[clojure.spec.test.alpha :as stest]
            [clojure.spec.gen.alpha :as gen]))

;;; Compound Procedures
(defn square [x] (* x x))
(square 21)
(defn sum-of-square [x y] (+ (square x) (square y)))
(sum-of-square 2 3)
(defn f [a] (sum-of-square (+ a 1) (* a 2)))
(clojure.pprint/pprint
 (macroexpand
  (f 5)))


;;; Conditional Expressions predicates


(defn abs [x]
  (cond (> x 0) x)
  (= x 0) 0
  (< x 0) (- x))

(abs -10)

(defn abs- [x]
  (cond (< x 0) (- x)
        :else x))

(defn abs-if [x]
  (if (< x 0) (- x) x))

(abs-if 10)

(abs- 100)

(abs -10)

(and (> 10 5) (< 5 1))


;;; Exercise 1.2


(/ (+ 5 4 (- 2 (- 3 (+ 6 1/3))))
   (* 3 (* (- 6 2) (- 2 7))))

;;; Exercise 1.3
(defn sumsquare [a b] (+ (* a a) (* b b)))

(defn proc [x y z]
  (cond (and (>= x z) (>= y z)) (sumsquare x y)
        (and (>= y x) (>= z x)) (sumsquare y z)
        (and (>= z y) (>= x y)) (sumsquare z x)))

(proc 1 2 3)
(proc 5 4 3)
(proc 1 3 2)

;;; Exercise 1.4
(defn a-plus-abs-b [a b]
  ((if (> b 0) + -) a b))

(a-plus-abs-b 2 3)
(a-plus-abs-b 10 -1)

;;;  1.1.7 Example: Square Roots by Newton's Method
(defn abs [x]
  (max x (- x)))
(abs -10)

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(good-enough? 1 3)

(defn average [x y]
  (/ (+ x y) 2))

(average 2 4)

(defn improve [guess x]
  (average guess (/ x guess)))

(defn improve3 [guess x] 
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x) guess
      (sqrt-iter (improve3 guess x) x)))

(defn sqrt-iter3 [guess x]
  (= (improve3 guess x) guess))

(sqrt-iter 2 4)

(defn sqrt [x]
  (sqrt-iter3 1.1 x))

(sqrt 4.0)
(sqrt 36.0)

(defn mult [x y ctr]
  #dbg (println x)
  (if (= ctr y) x
      (+ x (mult x y (inc ctr)))
     ))


(mult 2 5 1)
