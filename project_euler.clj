(ns project.euler)


;p1
;multiples of 3 or 5

(defn multiples?
  [n a]
  (zero? (mod n a)))

(defn p1
  [n]
  (cond
    (= n 0) 0
    (multiples? n 3) (+ n (p1 (- n 1)))
    (multiples? n 5) (+ n (p1 (- n 1)))
    :else (p1 (- n 1))))

(println (p1 999))

(defn p1-v2
  [n]
  (apply + (filter (fn [n] (or (multiples? n 3) (multiples? n 5)))
                   (range 1 n))))

(println (p1-v2 1000))


;p2
;even fibonacci numbers

(defn fibonacci
  [s1 s2 limit]
  (loop [a s1
         b s2
         fibo (vector s1 s2)]
    (let [nb (+ a b)]
      (if (>= nb limit)
          fibo
          (recur b nb (conj fibo nb))))))

(defn p2
  [s1 s2 limit]
  (apply + (filter even? (fibonacci s1 s2 limit))))

(println (p2 1 2 4000000))


;p3
;largest prime factor

(defn prime?
  [x]
  (cond
    (< x 2) false
    (= x 2) true
    (even? x) false
    :else
      (loop [n 3]
        (cond
          (<= x (* n n)) true
          (zero? (mod x n)) false
          :else (recur (+ n 2))))))
 
(defn next-prime
  [x]
  (let [n (+ x 1)]
    (if (prime? n) n (recur n))))

(defn prime-factors
  [x]
  (loop [n x
         p 2
         result []]
    (cond
      (prime? n) (conj result n)
      (zero? (mod n p)) (recur (/ n p) p (conj result p))
      :else (recur n (next-prime p) result))))

(defn p3
  [x]
  (last (prime-factors x)))
  
(println (p3 600851475143))
