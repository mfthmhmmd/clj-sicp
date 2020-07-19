(ns clj-sicp.procedure_and_process
  (:gen-class))

(defn fact-iter [result n counter]
  (if (= counter n) result
      (fact-iter (+ result n) n (inc counter))))

(fact-iter 0 100 1)

(defn factorial [n]
  (let [result 0 n n counter 1]
    (fact-iter result n counter)))


(defn plus [a b]
  (if (= a 0) b
      (plus (dec a) (inc b))))

(defn plus [a b]
  (if (= a 0) b
      (inc (plus (dec a) b))))

(inc (+ 1 3))
(inc (+ 0 3))

(plus 2 3)

(defn ackermann [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (ackermann (dec x) (dec y))))

(ackermann 1 10)

(ackermann 2 4)
(ackermann 3 3)

;;; Exercise 1.10
(defn f [n] (ackermann 0 n))
(defn g [n] (ackermann 1 n))
(defn h [n] (ackermann 2 n))
(defn k [n] (* 5 n n))
(f (k 3))
(k 5)

;;; 1.2.2 Tree Recursion
(defn fibonacci [n]
  (cond (= n 1) 1
        (= n 0) 0
        :else
        (+ n  (fibonacci (- n 1)) (fibonacci (- n 2)))))

(fibonacci 4)

(defn fibonacci-iter [a b counter]
  (if (= counter 0) b
      (recur (+ a b) a (dec counter))))

(defn fibonacci [n]
  (let [a 1 b 0 counter n]
    (fibonacci-iter a b counter)))

(fibonacci 10)

;;; Exercise 1.11
;;; Recursive
(defn f [n]
  ;;#dbg
  (if (< n 3) n
       ;;#dbg
      (+
       (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(f 12)

(defn f [n]
  (loop [a 2 b 1 c 0 counter (- n 2)]
    (cond (< n 3)        n
          (<= counter 0) a
          :else          (recur (+ a (* 2 b) (* 3 c))
                                a
                                b
                                (dec counter)))))

(f 5)

(defn hanoi [n from to spare]
  (if (= n 0) "Done"
      (do
        (hanoi (dec n) from spare to)
        (println from "to" to)
        (hanoi (dec n) spare to from))))

(hanoi 5 "A" "C" "B")

(defn expt [b n]
  (if (= n 0) 1
      (* b (expt b (dec n)))))

(expt 3 3)

(defn square [x] (* x x))

(defn fast-expt [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt b (/ n 2)))
        :else (* b (fast-expt b (dec n)))))

(fast-expt 4 4)

(defn mult [a b]
  (if (= b 0) 0
      (+ a (mult a (dec b)))))

(defn dobl [n]
  (mult n n))

(defn halv [n]
  (if (even? n) (/ n 2)))

(halv
 (dobl
  (mult 2 4)))

(defn sum [a b]
  (if (> a b) 0
      (+ a (sum (inc a) b))))

(sum 1 5)
(reduce + (range 1 6))

(defn filt [fun alist]
  (let [f (first alist) r (rest alist)]
    (cond (empty? alist) nil
          :else     (if (fun f) (cons f (filt fun r))
                        (filt fun (rest r))))))


(defn gcd [a b]
  (if (= b 0) a
      (gcd b (mod a b))))

(mod 2 4)
(mod 8 4)
(gcd 4 8)
(gcd 206 48)
