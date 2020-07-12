(ns core.abstraction-hop)

(defn cube [x] (* x x))

;; Procedure as arguments

(defn sum-integers
  "computes the sum of the integers from a through b:"
  [a b]
  (if (> a b)
    0
    (+ a (sum-integers (inc a) b))))

(sum-integers 1 5)

(defn sum-cubes
  "computes the sum of the cubes of the integers in the given range:"
  [a b]
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (inc a) b))))

(sum-cubes 1 5)

(defn pi-sum
  "computes the sum of a sequence of terms in the series
  1/1*3 +1/5*7 +1/9*11 etc etc
  "
  [a b]
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2)))
       (pi-sum (+ a 4) b))))

(pi-sum 2 5)

(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(= (sum-integers 1 5) (sum identity 1 inc 5))
(= (sum-cubes 1 5) (sum cube 1 inc  5))

(defn pi-term [x]
  (/ 1.0 (* x (+ x 2))))
(defn pi-next [x]
  (+ x 4))

(= (pi-sum 1 3) (sum pi-term 1.0 pi-next 3.0))


;; Using these procedures, we can compute an approximation to π :


(* 8 (sum pi-term 1 pi-next 10000))

;; Find Integral
(defn integral [f a b dx]
  (* (sum f (+ a (/ dx 2.0)) (partial + dx) b) dx))

(integral cube 0 1 0.01)

;; Exercise 1.29 Simpson's rule
;; h = (b-2) / n where n is even integer, yk = f(a + kh)

(defn simpsons-integral [f a b n]
  (letfn [(h [] (/ (- b a) n))
          (yk [k] (f (+ a (* (h) k))))
          (simpsons [k] (* (cond (or (= k 0) (= k n)) 1
                                 (= even? k) 4
                                 (= odd? k) 2)) (yk k))]
    (* (/ (h) 3)
       (sum simpsons 0 inc n))))

(simpsons-integral cube 0 1.0 100)
(simpsons-integral cube 0 1.0 1000)

;; Exercise 1.30 Recursion to Iteration
(defn sum [term a next b]
  (letfn [(iter [a result]
            (if (> a b) result
                (recur (next a) (+ result (term a)))))]
    (iter a 0)))

(sum identity 1 inc 10)
