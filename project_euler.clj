(ns project.euler
  (:require
    [clojure.string :as string]
    [clojure.math :as math]))

(defn parse-int
  [s]
  (try (Integer/parseInt s)
    (catch Exception e nil)))

(defn map-parse-int
  [ls]
  (map parse-int ls))
  ;(map #(Integer/parseInt %) ls))

(defn divide-into-lines
  [block]
  (string/split block #"\n"))

;複数行の数字の並びを一次元リストに変換する
(defn parse-enumerate-numbers-to-list
  [block]
  (filter (fn [x] (not (nil? x)))
          (map parse-int 
               (map str (seq block)))))

;複数行の数字の並びを二次元リストに変換する
(defn parse-enumerate-numbers-to-matrix
  [block]
  (map map-parse-int
       (map (fn [line] (string/split line #""))
            (divide-into-lines block))))

;複数行の数字の並び(数字間スペースあり)を二次元リストに変換する
(defn parse-enumerate-numbers-with-space-to-matrix
  [block]
  (map map-parse-int
       (map (fn [line] (string/split line #" "))
            (divide-into-lines block))))

;複数行の数字の並びを一行を一つの数値として一次元リストに変換する
(defn parse-enumerate-numbers-to-lines
  [block]
  (map bigdec (divide-into-lines block)))


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

(def p8arg*
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

(defn multiply-range
  [nums n m]
  (loop [i n
         ret 1]
    (if (= i (+ m 1))
        ret
        (recur (+ i 1) (* ret (nth nums i))))))

(defn p8 
  [ls x]
  (def len (count ls))
  (loop [i 0
         ret 0]
    (if (= i (- len x)) 
        ret
        (recur (+ i 1) (max ret (multiply-range ls i (- (+ i x) 1)))))))

(println (p8 (parse-enumerate-numbers-to-list p8arg*) 13))


;p9
;special pythagorean triplet

(defn p9
  [x]
  (loop [a 1 
         b 2 
         c (- x 3)]
    (cond 
      (= (* c c) (+ (* b b) (* a a))) (* a b c)
      (<= c b) (recur (+ a 1) (+ a 2) (- x a a 3)) 
      :else (recur a (+ b 1) (- c 1)))))

(println (p9 1000))


;p10
;summation of primes

(defn sum-of-primes-below
  [n]
  (loop [i 0 
         result 0]
    (cond
      (< n i) result
      (prime? i) (recur (+ i 1) (+ result i))
      :else (recur (+ i 1) result))))

(println (sum-of-primes-below 2000000))


;p11
;largest product in a grid

(def p11arg*
 "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48")

(defn rotate90
  [matrix]
  (loop [ls matrix 
         ret []]
    (if (empty? (first ls))
        ret
        (recur (map rest ls) (conj ret (reverse (map first ls)))))))

(defn rotate45
  [matrix]
  (def cols (count (first matrix)))
  (def matrix-aux
    (loop [i 0 
           ls matrix 
           ret []]
      (if (empty? ls) 
          ret 
          (recur (+ i 1) 
                 (rest ls) 
                 (conj ret 
                      (concat (repeat i 1)
                              (first ls)
                              (repeat (- cols 1 i) 1)))))))
  (rotate90 matrix-aux))

(defn rotate135
  [matrix]
  (rotate45 (rotate90 matrix)))

(def test-matrix [[1 2 3] [4 5 6] [7 8 9]])
(println (rotate90 test-matrix))
(println (rotate45 test-matrix))
(println (rotate135 test-matrix))

(defn max-product-series
  [ls n]
  (def len (count ls))
  (if (< len n)
      (apply * ls)
      (loop [i 0 ret 0]
        (if (> (+ i n) len) 
            ret
            (recur (+ i 1) (max ret (multiply-range ls i (- (+ i n) 1))))))))

(defn p11
  [matrix n]
  (apply max (concat (map (fn [ls] (max-product-series ls n)) matrix)
                     (map (fn [ls] (max-product-series ls n)) (rotate45 matrix))
                     (map (fn [ls] (max-product-series ls n)) (rotate90 matrix))
                     (map (fn [ls] (max-product-series ls n)) (rotate135 matrix)))))

(println (p11 (parse-enumerate-numbers-with-space-to-matrix p11arg*) 4))


;p12
;highly divisible triangular number

(defn count-divisors
  [n]
  (loop [i 1 ret 0]
    (cond
      (< n (* i i)) ret
      (= n (* i i)) (+ ret 1)
      (zero? (mod n i)) (recur (+ i 1) (+ ret 2))
      :else (recur (+ i 1) ret))))

(defn p12
  [n]
  (loop [i 2 x 1]
    (if (<= n (count-divisors x))
        x
        (recur (+ i 1) (+ x i)))))

(println (p12 5))
(println (p12 500))


;p13
;large sum

(def p13arg* 
 "37107287533902102798797998220837590246510135740250
46376937677490009712648124896970078050417018260538
74324986199524741059474233309513058123726617309629
91942213363574161572522430563301811072406154908250
23067588207539346171171980310421047513778063246676
89261670696623633820136378418383684178734361726757
28112879812849979408065481931592621691275889832738
44274228917432520321923589422876796487670272189318
47451445736001306439091167216856844588711603153276
70386486105843025439939619828917593665686757934951
62176457141856560629502157223196586755079324193331
64906352462741904929101432445813822663347944758178
92575867718337217661963751590579239728245598838407
58203565325359399008402633568948830189458628227828
80181199384826282014278194139940567587151170094390
35398664372827112653829987240784473053190104293586
86515506006295864861532075273371959191420517255829
71693888707715466499115593487603532921714970056938
54370070576826684624621495650076471787294438377604
53282654108756828443191190634694037855217779295145
36123272525000296071075082563815656710885258350721
45876576172410976447339110607218265236877223636045
17423706905851860660448207621209813287860733969412
81142660418086830619328460811191061556940512689692
51934325451728388641918047049293215058642563049483
62467221648435076201727918039944693004732956340691
15732444386908125794514089057706229429197107928209
55037687525678773091862540744969844508330393682126
18336384825330154686196124348767681297534375946515
80386287592878490201521685554828717201219257766954
78182833757993103614740356856449095527097864797581
16726320100436897842553539920931837441497806860984
48403098129077791799088218795327364475675590848030
87086987551392711854517078544161852424320693150332
59959406895756536782107074926966537676326235447210
69793950679652694742597709739166693763042633987085
41052684708299085211399427365734116182760315001271
65378607361501080857009149939512557028198746004375
35829035317434717326932123578154982629742552737307
94953759765105305946966067683156574377167401875275
88902802571733229619176668713819931811048770190271
25267680276078003013678680992525463401061632866526
36270218540497705585629946580636237993140746255962
24074486908231174977792365466257246923322810917141
91430288197103288597806669760892938638285025333403
34413065578016127815921815005561868836468420090470
23053081172816430487623791969842487255036638784583
11487696932154902810424020138335124462181441773470
63783299490636259666498587618221225225512486764533
67720186971698544312419572409913959008952310058822
95548255300263520781532296796249481641953868218774
76085327132285723110424803456124867697064507995236
37774242535411291684276865538926205024910326572967
23701913275725675285653248258265463092207058596522
29798860272258331913126375147341994889534765745501
18495701454879288984856827726077713721403798879715
38298203783031473527721580348144513491373226651381
34829543829199918180278916522431027392251122869539
40957953066405232632538044100059654939159879593635
29746152185502371307642255121183693803580388584903
41698116222072977186158236678424689157993532961922
62467957194401269043877107275048102390895523597457
23189706772547915061505504953922979530901129967519
86188088225875314529584099251203829009407770775672
11306739708304724483816533873502340845647058077308
82959174767140363198008187129011875491310547126581
97623331044818386269515456334926366572897563400500
42846280183517070527831839425882145521227251250327
55121603546981200581762165212827652751691296897789
32238195734329339946437501907836945765883352399886
75506164965184775180738168837861091527357929701337
62177842752192623401942399639168044983993173312731
32924185707147349566916674687634660915035914677504
99518671430235219628894890102423325116913619626622
73267460800591547471830798392868535206946944540724
76841822524674417161514036427982273348055556214818
97142617910342598647204516893989422179826088076852
87783646182799346313767754307809363333018982642090
10848802521674670883215120185883543223812876952786
71329612474782464538636993009049310363619763878039
62184073572399794223406235393808339651327408011116
66627891981488087797941876876144230030984490851411
60661826293682836764744779239180335110989069790714
85786944089552990653640447425576083659976645795096
66024396409905389607120198219976047599490197230297
64913982680032973156037120041377903785566085089252
16730939319872750275468906903707539413042652315011
94809377245048795150954100921645863754710598436791
78639167021187492431995700641917969777599028300699
15368713711936614952811305876380278410754449733078
40789923115535562561142322423255033685442488917353
44889911501440648020369068063960672322193204149535
41503128880339536053299340368006977710650566631954
81234880673210146739058568557934581403627822703280
82616570773948327592232845941706525094512325230608
22918802058777319719839450180888072429661980811197
77158542502016545090413245809786882778948721859617
72107838435069186155435662884062257473692284509516
20849603980134001723930671666823555245252804609722
53503534226472524250874054075591789781264330331690")

(defn p13
  [ls]
  (subs (str (apply + ls)) 0 10))

(println (p13 (parse-enumerate-numbers-to-lines p13arg*)))


;p14
;longest collatz sequence

(defn count-collatz-sequence
  [start]
  (loop [n start 
         count 1]
    (cond
      (= n 1) count
      (even? n) (recur (/ n 2) (+ count 1))
      :else (recur (+ (* 3 n) 1) (+ count 1))))) 

(defn p14
  [limit]
  (loop [i 1 
         max-count 0
         ret 0]
    (if (= i limit)
        ret
        (let [count (count-collatz-sequence i)]
          (if (< max-count count)
              (recur (+ i 1) count i)
              (recur (+ i 1) max-count ret))))))

(println (p14 1000000))


;p15
;lattice paths

;long overflow
;(defn multiplication
;  [n m]
;  (loop [i n ret 1]
;    (if (> i m) ret (recur (+ i 1) (* ret i)))))

;long overflow
;(defn multiplication
;  [n m]
;  (apply * (range n (+ m 1))))

(defn multiplication
  [n m]
  (loop [i n ret 1M]  ;(bigdec 1) でもok
    (if (> i m) ret (recur (+ i 1) (* ret i)))))

(defn multiplication-v2
  [n m]
  (apply *' (range n (+ m 1))))


(defn combination
  [m n]
  (/ (multiplication (+ (- m n) 1) m)
     (multiplication 1 n)))

(println (combination 40 20))


;p16
;power digit sum

(defn number-to-digits
  [x]
  (map #(Integer/parseInt %) (string/split (str x) #"")))

(defn sum-of-digit
  [x]
  (apply + (number-to-digits x)))

(defn pow
  [x n]
  (loop [i 1 ret (bigdec x)]
    (if (= i n) ret (recur (+ i 1) (* ret x)))))

(defn p16
  [x n]
  (sum-of-digit (bigint (pow x n))))

(println (p16 2 15))
(println (p16 2 1000))


;p17
;number letter counts

(def num-en-map
  {1 "one", 2 "two", 3 "three", 4 "four", 5 "five", 6 "six",
  7 "seven", 8 "eight", 9 "nine", 10 "ten", 11 "eleven",
  12 "twelve", 13 "thirteen", 14 "fourteen", 15 "fifteen",
  16 "sixteen", 17 "seventeen", 18 "eighteen", 19 "nineteen",
  20 "twenty", 30 "thirty", 40 "forty", 50 "fifty", 60 "sixty",
  70 "seventy", 80 "eighty", 90 "ninety", 1000 "onethousand"})

;1000より大きい数は想定しない
(defn number-to-english
  [x]
  (loop [n x ret "" aux ""]
    (cond
      (= n 0) ret
      (num-en-map n) (str ret aux (num-en-map n)) 
      (<= 100 n) (recur (mod n 100) (str ret (num-en-map (int (/ n 100))) "hundred") "and")
      (<= 20 n) (recur (mod n 10) (str ret aux (num-en-map (* (int (/ n 10)) 10))) "")
      :else (println "error"))))

(println (number-to-english 23))
(println (number-to-english 342))
(println (number-to-english 111))

(defn p17
  [n m]
  (loop [i n ret 0]
    (if (> i m)
        ret
        (recur (+ i 1) (+ ret (count (number-to-english i)))))))

(println (p17 1 5))
(println (p17 1 1000))


;p18
;maximum path sum 1

(def  p18arg*
"75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23")

(defn p18
  [triangle]
  (defn aux
    [depth i]
    (let [x (nth (nth triangle depth) i)]
    (if (= depth (- (count triangle) 1))
        x
        (max (+ x (aux (+ depth 1) i)) 
             (+ x (aux (+ depth 1) (+ i 1)))))))
   (aux 0 0))

(println (p18 (parse-enumerate-numbers-with-space-to-matrix p18arg*)))


;p19
;counting sundays

(defn leapyear?
  [y]
  (cond
    (zero? (mod y 400)) true
    (zero? (mod y 100)) false
    (zero? (mod y 4)) true
    :else false))

(defn get-month-days
  [y m]
  (cond
    (or (= m 4) (= m 6) (= m 9) (= m 11)) 30
    (= m 2) (if (leapyear? y) 29 28)
    :else 31))

;月の最終日の曜日取得(Sun: 1 Mon: 2 ... Sat: 7)
(defn get-day-of-week-first-day-of-next-month
  [day-of-week-first-day-of-month y m]
  (mod (+ day-of-week-first-day-of-month 
          (get-month-days y m) 1)
        7))

;期間内の月の初日が日曜日(Sun: 1)となる回数をカウント
(defn p19
  [day-of-week-start-day ys ms ye me]
  (loop [dow day-of-week-start-day
         y ys
         m ms
         ret 0]
    (if (or (> y ye) (and (= y ye) (> m me)))
        ret 
        (recur (get-day-of-week-first-day-of-next-month dow y m)
               (if (= m 12) (+ y 1) y)
               (if (= m 12) 1 (+ m 1))
               (if (= dow 1) (+ ret 1) ret)))))

(println (- (p19 2 1900 1 2000 12) (p19 2 1900 1 1900 12)))


;p20
;factorial digit sum

(defn factorial
  [n]
  (if (= n 1) 1M (* n (factorial (- n 1)))))

(defn p20
  [n]
  (apply + (number-to-digits (factorial n))))

(println (p20 100))


;p21
;amicable numbers

(defn divisors-except-self
  [n]
  (loop [i 2
         ret [1]]
    (cond
      (< n (* i i)) ret
      (= n (* i i)) (conj ret i)
      (zero? (mod n i)) (recur (+ i 1) (concat ret (list i (/ n i))))
      :else (recur (+ i 1) ret))))

(defn amicable-number?
  [n]
  (let [b (apply + (divisors-except-self n))
        a (apply + (divisors-except-self b))] 
    (and (= a n) (not (= a b)))))

(defn p21
  [n]
  (loop [i 1 ret 0]
    (cond
      (= i n) ret
      (amicable-number? i) (recur (+ i 1) (+ ret i))
      :else (recur (+ i 1) ret))))

(println (p21 10000))


;p22
;names scores

(def p22arg*
  (slurp "p022_names.txt"))

(defn name-score
  [n name]
  (* n (apply + (map (fn [c] (- (int c) 64)) 
                     (seq name)))))

(defn p22
  [names]
  (def len (count names))
  (loop [i 0 ret 0]
    (if (= i len)
        ret
        (recur (+ i 1) 
               (+ ret (name-score (+ i 1) (string/replace (nth names i) #"\"" "")))))))

(println (p22 (sort (string/split p22arg* #","))))


;p23
;non-abundant sums

(defn abundant-number?
  [n]
  (< n (apply + (divisors-except-self n))))

(defn abundant-numbers
  [limit]
  (loop [i 1
         ret []]
    (cond 
      (< limit i) ret
      (abundant-number? i) (recur (+ i 1) (conj ret i))
      :else (recur (+ i 1) ret))))

(defn abundant-sums-exists?
  [n abundants]
  (loop [ls abundants]
    (cond 
      (< n (* 2 (first ls))) false
      (abundant-number? (- n (first ls))) true
      :else (recur (rest ls)))))

(defn p23 
  [limit]
  (def abundants (abundant-numbers limit))
  (loop [i 1
         ret 0]
    (cond
      (> i limit) ret
      (abundant-sums-exists? i abundants) (recur (+ i 1) ret)
      :else (recur (+ i 1) (+ ret i)))))

;(println (p23 28123))


;p24
;lexicographic permutations

(defn drop-index
  [ls index]
  (concat (take index ls) (drop (+ index 1) ls)))

(defn drop-item
  [ls item]
  (drop-index ls (.indexOf ls item)))

(defn permutations
  [ls]
  (loop [l ls ret []]
    (cond 
      (empty? ls) [[]]
      (empty? l) ret
      :else (recur (rest l) 
                   (concat ret 
                          (map (fn [x] (cons (first l) x)) 
                               (permutations (drop-item ls (first l)))))))))

(defn p24
  [ls i]
  (nth (sort (map (fn [l] (apply str l)) 
                  (permutations ls)))
       (- i 1)))

(println (p24 [0 1 2 3 4 5 6 7 8 9] 1000000))


;p25
;1000-digit fibonacci number

(defn digit
  [n]
  (count (str n)))

(defn p25 []
  (loop [a 1M
         b 1M
         i 2]
    (if (= (digit b) 1000)
        i
        (recur b (+ a b) (+ i 1)))))

(println (p25))
