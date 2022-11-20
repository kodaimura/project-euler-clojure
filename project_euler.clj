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
         result (vector s1 s2)]
    (let [nb (+ a b)]
      (if (>= nb limit)
          result
          (recur b nb (conj result nb))))))

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
          (< x (* n n)) true
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


;p4
;largest palindrome product

(defn palindrome-number?
  [x]
  (let [s (str x)]
    (= s (apply str (reverse s)))))

(defn max-number-of-n-digit
  [n]
  (Integer/parseInt (apply str (replicate n "9"))))
  
(defn min-number-of-n-digit
  [n]
  (Integer/parseInt (apply str (cons "1" (replicate (- n 1) "0")))))

(defn p4
  [digit]
  (let [start (min-number-of-n-digit digit)
        end (max-number-of-n-digit digit)]
    (loop [a start
           b start
           result 0]
      (cond
        (and (= a end) (= b (+ end 1))) result
        (= b (+ end 1)) 
          (recur (+ a 1) start result)
        (and (palindrome-number? (* a b)) (< result (* a b))) 
          (recur a (+ b 1) (* a b))
        :else 
          (recur a (+ b 1) result)))))

(println (p4 3))


;p5
;smallest multiple

(defn divisible-by-from-n-to-m?
  [x n m]
  (every? (fn [i] (zero? (mod x i)))
          (range n (+ m 1))))

(defn p5
  [n m]
  (loop [i 1]
    (if (divisible-by-from-n-to-m? i n m)
        i
        (recur (+ i 1)))))

;(println (p5 1 20))


;p6
;sum square difference

(defn square [x] (* x x))

(defn sum-of-squares
  [n]
  (apply + (map square (range 1 (+ n 1)))))

(defn square-of-sum
  [n]
  (square (apply + (range 1 (+ n 1)))))

(defn p6
  [n]
  (- (square-of-sum n) (sum-of-squares n)))

(println (p6 100))


;p7
;10001st prime

(defn nth-prime
  [n]
  (loop [i 0
         p 0]
    (if (= i n) p (recur (+ i 1) (next-prime p)))))

(println (nth-prime 10001))


;p8
;largest product in a series

(def p8numstr
 "96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450")

(defn parse-int
  [s]
  (try (Integer/parseInt s)
    (catch Exception e nil)))

(defn multiply-range
  [nums n m]
  (loop [i n
         result 1]
    (if (= i (+ m 1))
        result
        (recur (+ i 1) (* result (nth nums i))))))

(defn p8 
  [ls x]
  (def end (- (count ls) x))
  (loop [i 0
         result 0]
    (if (= i end) 
        result
        (let [mr (multiply-range ls i (- (+ i x) 1))]
          (if (< result mr)
              (recur (+ i 1) mr)
              (recur (+ i 1) result))))))

(def p8nums (filter (fn [x] (not (nil? x))) (map parse-int (map str (seq p8numstr)))))

(println (p8 p8nums 13))
