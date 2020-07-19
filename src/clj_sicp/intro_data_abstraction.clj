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


