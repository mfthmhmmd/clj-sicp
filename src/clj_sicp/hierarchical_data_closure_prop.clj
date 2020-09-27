(ns clj-sicp.hierarchical-data-closure-prop)

(defn parity [n]
  (loop [n n r 0]
    (if (= n 0) r
        (recur (dec n) (- 1 r)))))


;; Hierarchical Structure


(def x (cons (list 1 2) (list 3 4)))

(defn length [l]
  (if (empty? l) 0
      (+ 1 (length (rest l)))))

(defn count-leaves [l]
  (cond
    (not (seq? l)) 1
    (empty? l)     0
    :else          (+ (count-leaves (first l))
                      (count-leaves (rest l)))))
x
(count-leaves (list (list 1 2) (list 3 4) (list (list 5 6) 4)))

(defn list-ref [items n]
  (if (= n 0) (first items)
      (recur (rest items) (dec n))))

(list-ref '(1 2 3) 4)

(defn alength [alist]
  (if (empty? alist) 0
      (+ 1 (alength (rest alist)))))

(alength '(2 3 4 5))

(defn append [l1 l2]
  (if (empty? l1) l2
      (cons (first l1) (append (rest l1) l2))))

(append '(2 3) '(4 5))

;; Exercise 2.17
(defn last-pair [l]
  (if (empty? (rest l)) (first l) (last-pair (rest l))))

(last-pair (list 23 72 149 34))

;; Exercise 2.18
(defn reverse' [l]
  (loop [l l r (list)]
    (if (empty? l) r
        (recur (rest l) (cons (first l) r)))))

(reverse' (range 1000))


;; Exercise 2.20


(defn same-parity [x & args]
  (loop [parity (parity x)
         s      args
         result []]
    (cond
      (empty? s)                    (concat [x] result)
      (= parity (parity (first s))) (recur parity (rest s) (conj result (first s)))
      :else                         (recur parity (rest s) result))))

(same-parity 2 3 4 5)
