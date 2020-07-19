(ns clj-sicp.intro-data-abstraction
  (:require [clj-sicp.procedure_and_process :refer [gcd]]))

;; Intro
(defn make-rat [n d]
  (let [g (gcd n d)]
    (cons (/ n g) (cons (/ d g) (list)))))

(defn numer [rat]
  (first rat))

(defn denom [rat]
  (second rat))

(defn print-rat [rat]
  (println (str (numer rat) "/" (denom rat))))

;; a1/b1 + a2/b2 = (a1*b2 + b1*a2) / b1 * b2
(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))

;; a1/b1 * a2/b2 = (a1 * a2) / (b1 * b2)
(defn mult-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

;; a1/b1 / a2/b2 = a1*b2 / b1*a2
(defn div-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn equal-rat [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; Exercise 2.2
(defn make-segment [p1 p2]
  (cons p1 (cons p2 (list))))

(defn start-segment [s]
  (first s))
(defn end-segment [s]
  (second s))

(defn avg [x y]
  (/ (+ x y) 2.0))

(defn midpoint-segment [s]
  (let [s1 (first s)
        s2 (second s)]
    (make-point (avg (x-point s1) (x-point s2))
                (avg (y-point s1) (y-point s2)))))

(def p1 (make-point 2 4))
(def p2 (make-point 3 6))

(midpoint-segment (make-segment p1 p2))

(defn make-point [x y]
  (cons x (cons y (list))))

(defn x-point [p] (first p))
(defn y-point [p] (second p))

(defn print-point [p]
  (println (str "(" (x-point p) "," (y-point p) ")")))

;;Exercise 2.4
(defn cons' [x y]
  (fn [m]
    (m x y)))

(defn car' [z]
  (z (fn [p q] p)))

(defn cdr' [z]
  (z (fn [p q] q)))

;; ((cons' 1 2) (fn [p q] p))
(car' (cons' 1 2))
(cdr' (cons' 1 2))


;; Exercise 2.5
(defn weird-cons [x y]
  (* (Math/pow 2 x) (Math/pow 3 y)))

(def c (weird-cons 3 5))
(def a (Math/pow 2 3))
(def b (Math/pow 3 5))
(* a b)
(/ (Math/log a) (Math/log 2))

(defn weird-car [num]
  (let [c (Math/log)]))

(defn weird-cdr [num]
  (rem num 3))

(weird-car c)
