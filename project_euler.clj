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
(defn parse-enumerate-numbers->list
  [block]
  (filter (fn [x] (not (nil? x)))
          (map parse-int 
               (map str (seq block)))))

;複数行の数字の並びを二次元リストに変換する
(defn parse-enumerate-numbers->matrix
  [block]
  (map map-parse-int
       (map (fn [line] (string/split line #""))
            (divide-into-lines block))))

;複数行の数字の並び(数字間スペースあり)を二次元リストに変換する
(defn parse-enumerate-numbers-with-space->matrix
  [block]
  (map map-parse-int
       (map (fn [line] (string/split line #" "))
            (divide-into-lines block))))

;複数行の数字の並びを一行を一つの数値として一次元リストに変換する
(defn parse-enumerate-numbers->lines
  [block]
  (map bigdec (divide-into-lines block)))

;一行をスペース区切りでリストにし二次元リストに変換する
(defn parse-enumerate-with-space->matrix
  [block]
  (map (fn [line] (string/split line #" "))
       (divide-into-lines block)))

;; p1
;; Multiples of 3 or 5
;; 1000未満の3または5の倍数の和

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

(println "p1" (p1 999))

(defn p1-v2
  [n]
  (apply + (filter (fn [n] (or (multiples? n 3) (multiples? n 5)))
                   (range 1 n))))

(println "p1-v2" (p1-v2 1000))


;; p2
;; Even Fibonacci numbers
;; 400万を超えないフィボナッチ数列の偶数項の和

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

(println "p2" (p2 1 2 4000000))


;; p3
;; Largest prime factor
;; 最大の素数の約数

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
  (if (< x 2) 
      []
      (loop [n x
             p 2
             result []]
        (cond
          (prime? n) (conj result n)
          (zero? (mod n p)) (recur (/ n p) p (conj result p))
          :else (recur n (next-prime p) result)))))

(defn p3
  [x]
  (last (prime-factors x)))
  
(println "p3" (p3 600851475143))


;; p4
;; Largest palindrome product
;; 2つの3桁の数の積から作られる最大の回文数

(defn palindrome?
  [s]
  (= s (apply str (reverse s))))

(defn palindrome-number?
  [x]
  (palindrome? (str x)))

(defn max-number-of-digit
  [n]
  (Integer/parseInt (apply str (replicate n "9"))))
  
(defn min-number-of-digit
  [n]
  (Integer/parseInt (apply str (cons "1" (replicate (- n 1) "0")))))

(defn p4
  [digit]
  (let [start (min-number-of-digit digit)
        end (max-number-of-digit digit)]
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

(println "p4" (p4 3))


;; p5
;; Smallest Multiple
;; 1から20までのすべての数で割り切れる最小の正数

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

;(println "p5" (p5 1 20))


;; p6
;; Sum square difference
;; 1から100までの整数の2乗和と和の2乗の差

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

(println "p6" (p6 100))


;; p7
;; 10001st Prime
;; 10001番目の素数

(defn nth-prime
  [n]
  (loop [i 0
         p 0]
    (if (= i n) p (recur (+ i 1) (next-prime p)))))

(println "p7" (nth-prime 10001))


;; p8
;; Largest product in a series
;; 隣接する13桁の数字の積で最大の値

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

(println "p8" (p8 (parse-enumerate-numbers->list p8arg*) 13))


;; p9
;; Special Pythagorean triplet
;; a < b < c | a^2 + b^2 = c^2 | a + b + c = 1000 を満たす abcの値

(defn p9
  [x]
  (loop [a 1 
         b 2 
         c (- x 3)]
    (cond 
      (= (* c c) (+ (* b b) (* a a))) (* a b c)
      (<= c b) (recur (+ a 1) (+ a 2) (- x a a 3)) 
      :else (recur a (+ b 1) (- c 1)))))

(println "p9" (p9 1000))


;; p10
;; Summation of primes
;; 2000000未満の素数の和

(defn sum-of-primes-below
  [n]
  (loop [i 0 
         result 0]
    (cond
      (< n i) result
      (prime? i) (recur (+ i 1) (+ result i))
      :else (recur (+ i 1) result))))

;(println "p10" (sum-of-primes-below 2000000))


;; p11
;; Largest product in a grid
;; 縦、横、斜めの連続する4数の積で最大の値

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

(println "p11" (p11 (parse-enumerate-numbers-with-space->matrix p11arg*) 4))


;; p12
;; Highly divisible triangular number
;; 三角数 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
;; 約数の数が500を超える最初の三角数

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
(println "p12" (p12 500))


;; p13
;; Large sum
;; 下記の全ての50桁の数の和の先頭10桁

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

(println "p13" (p13 (parse-enumerate-numbers->lines p13arg*)))


;; p14
;; Longest collatz sequence
;; 偶数: n/2   奇数: 3n+1
;; 上記のルールで1になるまで繰り返した時に、
;; 100万未満の開始数で、最も長いチェーンを生成する数字

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

;(println "p14" (p14 1000000))


;; p15
;; Lattice paths
;; 20×20 のグリッドで左上から右下に行くルートがいくつあるか。

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

(println "p15" (combination 40 20))


;; p16
;; Power digit sum
;; 2^1000 の全ての桁の数字の和

(defn integer->list
  [x]
  (map #(Integer/parseInt %) (string/split (str x) #"")))

(defn sum-of-digit
  [x]
  (apply + (integer->list x)))

(defn pow
  [x n]
  (loop [i 1 ret (bigdec x)]
    (if (= i n) ret (recur (+ i 1) (* ret x)))))

(defn p16
  [x n]
  (sum-of-digit (bigint (pow x n))))

(println (p16 2 15))
(println "p16" (p16 2 1000))


;; p17
;; Number letter counts
;; 1から1000までの数字を英語にしたときの合計の文字数

(def num-en-map
  {1 "one", 2 "two", 3 "three", 4 "four", 5 "five", 6 "six",
  7 "seven", 8 "eight", 9 "nine", 10 "ten", 11 "eleven",
  12 "twelve", 13 "thirteen", 14 "fourteen", 15 "fifteen",
  16 "sixteen", 17 "seventeen", 18 "eighteen", 19 "nineteen",
  20 "twenty", 30 "thirty", 40 "forty", 50 "fifty", 60 "sixty",
  70 "seventy", 80 "eighty", 90 "ninety", 1000 "onethousand"})

;1000より大きい数は想定しない
(defn number->english
  [x]
  (loop [n x ret "" aux ""]
    (cond
      (= n 0) ret
      (num-en-map n) (str ret aux (num-en-map n)) 
      (<= 100 n) (recur (mod n 100) (str ret (num-en-map (int (/ n 100))) "hundred") "and")
      (<= 20 n) (recur (mod n 10) (str ret aux (num-en-map (* (int (/ n 10)) 10))) "")
      :else (println "error"))))

(println (number->english 23))
(println (number->english 342))
(println (number->english 111))

(defn p17
  [n m]
  (loop [i n ret 0]
    (if (> i m)
        ret
        (recur (+ i 1) (+ ret (count (number->english i)))))))

(println (p17 1 5))
(println "p17" (p17 1 1000))


;; p18
;; Maximum path sum 1
;; 上から下までの数字の合計が最大になるルート

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

(println "p18" (p18 (parse-enumerate-numbers-with-space->matrix p18arg*)))


;; p19
;; Counting Sundays
;; 1900/1/1 Monday
;; 4の倍数閏年
;; 100の倍数閏年ではない
;; 400の倍数閏年
;; 1901/1/1から2000/12/31の間で、月の初日が日曜日となる日数

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

(println "p19" (- (p19 2 1900 1 2000 12) (p19 2 1900 1 1900 12)))


;; p20
;; Factorial digit sum
;; 100!の全ての桁の数字の合計

;(defn factorial
;  [n]
;  (if (= n 0) 1M (* n (factorial (- n 1)))))

(defn factorial
  [n]
  (if (= n 0) 1M (apply * (range 1M (+ n 1)))))

(defn p20
  [n]
  (apply + (integer->list (factorial n))))

(println "p20" (p20 100))


;; p21
;; Amicable numbers
;; 220 の約数1, 2, 4, 5, 10, 11, 20, 22, 44, 55, 110 -> 合計284
;; 284 の約数1, 2, 4, 71, 142 -> 合計220
;; 上のような d(a)=b d(b)=a a!=b を満たす10000以下の数字の合計 

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

(println "p21" (p21 10000))


;; p22
;; Names scores
;; COLINE -> 3+15+12+9+14 = 53   辞書順で938番目
;; COLINE のスコアが53*938 = 49714 と計算される時の
;; ファイル内の全ての文字列の合計スコア

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

(println "p22" (p22 (sort (string/split p22arg* #","))))


;; p23
;; Non-abundant sums

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

;(println "p23" (p23 28123))


;; p24
;; Lexicographic permutations

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

;(println "p24" (p24 [0 1 2 3 4 5 6 7 8 9] 1000000))


;; p25
;; 1000-digit fibonacci number

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

(println "p25" (p25))


;; p26
;; Reciprocal cycles
;; d < 1000 において 1/d の循環小数の桁が最も大きいd

;(recurring-cycle 1 7) ->[1 4 2 8 5 7]
(defn recurring-cycle
  [n d]
  (loop [n n 
         d d 
         mods [] 
         quos []]
    (let [mo (mod n d)]
      (if (.contains mods mo)
          (drop (+ 1 (.indexOf mods mo)) (conj quos (quot n d)))
          (recur (* 10 mo) d (conj mods mo) (conj quos (quot n d)))))))

(println (recurring-cycle 1 7))

(defn p26
  [limit]
  (loop [i 1 ret 0]
    (if (< limit i)
        ret
        (recur (+ i 1) (max ret (count (recurring-cycle 1 i)))))))

(println "p26" (p26 1000))


;; p27
;; Quadratic primes
;; n^2+an+b において最も多くの素数を生成するab
;; ただし |a| < 1000 かつ |b| <= 1000

(defn p27 []
  (loop [a -999 
         b -1000
         n 0
         max-n 0
         ret 0]
    (cond
      (and (= a 999) (= b 1000)) ret
      (= b 1000) (recur (+ a 1) -1000 0 max-n ret)
      (prime? (+ (* n n) (* n a) b)) (recur a b (+ n 1) max-n ret)
      (< max-n n) (recur a (+ b 1) 0 n (* a b))
      :else (recur a (+ b 1) 0 max-n ret))))

(println "p27" (p27))


;; p28
;; Number spiral diagonals
;; 真ん中を1として渦巻状に整数を並べた1001×1001の正方形の対角線を結ぶ数字の和
;; 7 8 9
;; 6 1 2
;; 5 4 3

(defn p28 []
  (loop [n 1 
         layer 1
         c 1
         ret 1]
    (let [nn (+ n (* layer 2))]
      (cond
        (and (= (+ 1 (* layer 2)) 1001) (= c 4)) (+ ret nn)
        (= c 4) (recur nn (+ layer 1) 1 (+ ret nn))
        :else (recur nn layer (+ c 1) (+ ret nn))))))

(println "p28" (p28))


;; p29
;; Distinct powers
;; 2 <= a <= 100 および 2 <= b <= 100 に対してa^b で生成される値で
;; 異なる項はいくつあるか

(defn p29
  []
  (loop [a 2 b 2 ls []]
    (cond
      (and (= a 100) (= b 100)) (count (set (cons (pow a b) ls)))
      (= b 100) (recur (+ a 1) 2 (cons (pow a b) ls))
      :else (recur a (+ b 1) (cons (pow a b) ls)))))

(println "p29" (p29))


;; p30
;; Digit fifth powers
;; 1634 = 1^4 + 6^4 + 3^4 + 4^4
;; 8208 = 8^4 + 2^4 + 0^4 + 8^4
;; 9474 = 9^4 + 4^4 + 7^4 + 4^4 
;; 5桁で成り立つもの全ての数の和

(defn sum-of-powers-of-digits
  [x n]
  (apply + (map (fn [i] (int (pow i n)))
                (integer->list x))))

(defn get-p30-limit
  [digit]
  (loop [i 1 ret 0]
    (let [x (* i (int (pow 9 digit)))] 
      (cond
        (and (not (= (count (str x)) i)) (not (= ret 0))) ret
        (= (count (str x)) i) (recur (+ i 1) x)
        :else (recur (+ i 1) ret)))))

(println (get-p30-limit 4))
(println (get-p30-limit 5))

(defn p30
  [digit]
  (def limit (get-p30-limit digit))
  (loop [i 2 ret 0]
    (cond
      (= i limit) ret
      (= i (sum-of-powers-of-digits i digit)) (recur (+ i 1) (+ ret i))
      :else (recur (+ i 1) ret))))

(println (p30 4))
(println "p30" (p30 5))


;; p31
;; Coin sums
;; 1,2,5,10,20,50,100 の組み合わせで200になる通り 

(defn p31
  [ls x]
  (defn aux
    [ls sum]
    (cond
      (= sum x) 1
      (< x sum) 0
      (empty? ls) 0
      (and (= 1 (count ls)) (zero? (mod (- x sum) (first ls)))) 1
      :else (+ (aux ls (+ sum (first ls)))
               (aux (rest ls) sum))))
  (aux (sort > ls) 0))

(println "p31" (p31 [1 2 5 10 20 50 100 200] 200))


;; p32
;; Pandigital products
;; a*b=c a,b,cの桁に1~9の数字を全て使って表せる積の合計
;; 例) 39*186=7254

(defn p32 []
  (loop [a 1
         b 1
         ls []]
    (cond
      (= a 100) (apply + (set ls)) 
      (= b 3000) (recur (+ a 1) (+ a 1) ls)
      (= (apply str (sort (concat (integer->list a)
                                  (integer->list b)
                                  (integer->list (* a b)))))
         "123456789") (recur a (+ b 1) (cons (* a b) ls))
      :else (recur a (+ b 1) ls))))

(println "p32" (p32))


;; p33
;; Digit cancelling fractions
;; 49/98 -> 4/8 (約分と9を分子・分母からとったものが同じ)
;; 上のような分数の積の分母

;["abc" "bcd" "cde"] -> ["ab" "bd" "de"]
;["49" "98"] -> ["4" "9"]
(defn drop-common-string
  [ls]
  (loop [lls (map (fn [s] (string/split s #"")) ls)
         fls (first lls)]
    (cond
      (empty? fls) 
        (map (fn [l] (apply str l)) lls)
      (every? (fn [l] (.contains l (first fls))) lls) 
        (recur (map (fn [l] (drop-item l (first fls))) lls) (rest fls))
      :else (recur lls (rest fls)))))

(println (drop-common-string ["abc" "bcd" "cde"]))
(println (drop-common-string ["49" "98"]))

(defn curious-fraction?
  [numer denom]
  (if (and (zero? (mod numer 10)) (zero? (mod denom 10)))
      false
      (let [x (drop-common-string (list (str numer) (str denom)))
            n (first x)
            d (second x)]
        (cond 
          (or (= "" n) (= "" d)) false
          (= "0" d) false
          (= n (str numer)) false
          :else (= (/ numer denom)
                   (/ (Integer/parseInt n) (Integer/parseInt d)))))))

(println (curious-fraction? 49 98))
(println (curious-fraction? 30 50))
(println (curious-fraction? 13 27))

(defn p33 []
  (loop
    [numer 10
     denom 10
     ret 1]
    (cond
      (= denom 100) (denominator ret)
      (= numer denom) (recur 10 (+ denom 1) ret)
      (curious-fraction? numer denom)
        (recur (+ numer 1) denom (* ret (/ numer denom)))
      :else (recur (+ numer 1) denom ret))))

(println "p33" (p33))


;; p34
;; Digit factorials
;; 1! + 4! + 5! = 1 + 24 + 120 = 145 のような数字の合計
;; 1! = 1 2! = 2 は含めない
;; 999999(6桁) -> (* (factorial 9) 6) 2177280(7桁)
;; 9999999(7桁) -> (* (factorial 9) 7) 2540160(7桁)
;; 99999999(8桁) -> (* (factorial 9) 8) 2903040(7桁)
;; 8桁以上は考えない

(defn p34 []
  (loop [n 3M
         ret 0]
    (cond
      (= n 2540160M) ret
      (= n (apply + (map factorial (integer->list n)))) 
        (recur (+ n 1) (+ ret n))
      :else (recur (+ n 1) ret))))

;(println "p34" (p34))


;; p35
;; Circular primes
;; 197はcircular prime (197, 971, 719 全て素数)
;; 1000000以下でcircular primeは何個あるか

(defn rotations-of-digits
  [n]
  (loop [x (str n)
         i (count (str n))
         ret []]
    (if (= i 0) 
        ret
        (let [x2 (str (subs x 1) (subs x 0 1))]
          (recur x2 (- i 1) (cons (Integer/parseInt x2) ret))))))

(println (rotations-of-digits 197))
(println (rotations-of-digits 7))

(defn circular-prime?
  [n]
  (every? prime? (rotations-of-digits n)))

(defn p35
  [limit]
  (loop [i 1
         ret 0]
    (cond
      (> i limit) ret
      (circular-prime? i) (recur (next-prime i) (+ ret 1))
      :else (recur (next-prime i) ret))))

(println "p35" (p35 1000000))


;; p36
;; Double-base palindromes
;; 585 = 1001001001(binary)
;; 10進数2進数両方で回文数となる1000000以下の数字の合計

(defn decimal->binary
  [x]
  (loop [n x ret ""]
    (if (= n 0) 
        ret
        (recur (quot n 2) (str (mod n 2) ret)))))

(println (decimal->binary 10))
(println (decimal->binary 585))

(defn p36
  [limit]
  (loop [i 1 ret 0]
    (cond
      (= i limit) ret
      (and (palindrome-number? i) (palindrome? (decimal->binary i)))
        (recur (+ i 1) (+ ret i))
      :else (recur (+ i 1) ret))))

(println "p36" (p36 1000000))


;; p37
;; Truncatable primes
;; 3797 : 3797(素数) 797(素数) 97(素数) 7(素数)
;; 379(素数) 37(素数) 3(素数)となるような 11個の素数の合計

(defn continuously-remove-digits
  [n]
  (let [s (str n)
        len (count s)]
    (concat (map (fn [i] (Integer/parseInt (subs s 0 i))) 
                 (range 1 len))
            (map (fn [i] (Integer/parseInt (subs s i)))
                 (range 1 len))
            (list n))))

(defn truncatable-prime?
  [n]
  (if (> 10 n)
      false
      (every? prime? (continuously-remove-digits n))))

(defn p37 []
  (loop [i 10 c 0 ret 0]
    (cond
      (= c 11) ret
      (truncatable-prime? i) (recur (next-prime i) (+ c 1) (+ ret i))
      :else (recur (next-prime i) c ret))))

(println "p37" (p37))


;; p38
;; Pandigital multiples
;; 192 × 1 = 192
;; 192 × 2 = 384
;; 192 × 3 = 576
;; 192 (1,2,3) -> 192384576 (:1to9 Pandigital)
;; x (1,2,..,n) で最大の1to9 Pandigital

(defn pandigital-number?
  [n]
  (= (sort (integer->list n))
     (range 1 10)))

;(defn pandigital-number?
;  [x m n]
;  (= (sort > (integer->list x))
;     (range n (- m 1) -1)))

(defn list->integer
  [ls]
  (bigint (apply str ls)))

(defn p38 []
  (loop [i 1 
         n 2 
         ret 0]
    (let [x (list->integer (map (fn [m] (* m i)) (range 1 (+ n 1))))]
      (cond
        (< 990000000 x) (if (= n 2) ret (recur (+ i 1) 2 ret))
        (pandigital-number? x) (recur i (+ n 1) (max ret x))
        :else (recur i (+ n 2) ret)))))

(println "p38" (p38))


;; p39
;; Integer right triangles
;; {a, b, c} について p=a+b+c (p<=1000)  
;; a,b,c それぞれを辺の長さとした三角形が直角三角形となる場合を調べたとき
;; a,b,c の組み合わせが最も多くなるpの値

(defn right-angle-triangle?
  [a b c]
  (let [ls (sort (list a b c))]
    (= (+ (square (first ls)) (square (second ls))) 
       (square (last ls)))))

(defn combination-of-be-triangle
  [p]
  (loop [a 1 b 1 ret 0]
    (let [c (- p a b)]
      (cond
        (and (= a 1) (<= c b)) ret
        (or (= a b) (<= c b)) (recur 1 (+ b 1) ret)
        (right-angle-triangle? a b c) (recur (+ a 1) b (+ ret 1))
        :else (recur (+ a 1) b ret)))))

(defn p39
  [limit]
  (loop [p 3 maximum 0 ret 0]
    (let [cobt (combination-of-be-triangle p)]
      (cond
        (< limit p) ret
        (< maximum cobt) (recur (+ p 1) cobt p)
        :else (recur (+ p 1) maximum ret)))))

;(println "p39" (p39 1000))


;; p40
;; Champernowne's constant
;; 0.123456789101112131415161718192021...
;; d1 を少数1桁目の数字とする時
;; d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

(defn nth-digits
  [x n]
  (Integer/parseInt (str (nth (integer->list x) n))))

(defn p40
  [ls]
  (loop [ls (sort ls)
         i 1 
         c 0 
         ret 1]
    (let [d (digit i)
          x (first ls)]
      (cond
        (empty? ls) ret
        (<= x (+ c d)) (recur (rest ls) 
                              (+ i 1) 
                              (+ c d) 
                              (* ret (nth-digits i (- x c 1))))
        :else (recur ls (+ i 1) (+ c d) ret)))))

(println "p40" (p40 [1 10 100 1000 10000 100000 1000000]))


;; p41
;; Pandigital prime
;; パンデジタルの中で最大の素数

(defn p41 []
  (loop [i 9]
    (let [ls (filter prime? 
                     (map list->integer 
                          (permutations (range 1 (+ i 1)))))]
      (if (empty? ls) (recur (- i 1)) (apply max ls)))))

;(println "p41" (p41))


;; p42
;; Coded triangle numbers
;; Tn = 1/2 * n(n+1) 
;; SKY = 19+11+25= 55 = T10
;; 単語の値がTnとなる単語はいくつあるか

(def p42arg*
  (slurp "p042_words.txt"))

(defn word-value
  [s]
  (apply + (map (fn [c] (- (int c) 64))
                (seq (string/upper-case s)))))

;(defn triangle-number?
;  [n]
;  (loop [i 1]
;    (let [x (/ (* i (+ i 1)) 2)]
;      (cond
;        (< n x) false
;        (= n x) true
;        :else (recur (+ i 1))))))

;reference to p44
(defn triangle-number?
  [n]
  (zero? (mod (- (Math/sqrt (+ 1 (* 8 n))) 1) 2)))

(defn triangle-word?
  [s]
  (triangle-number? (word-value s)))

(defn p42 []
  (count (filter triangle-word? 
                 (map (fn [s] (string/replace s #"\"" ""))
                      (string/split p42arg* #",")))))

(println "p42" (p42))


;; p43
;; Sub-string divisibility

(defn p43-aux
  [ls]
  (and
    (zero? (mod (list->integer (list (nth ls 1) (nth ls 2) (nth ls 3))) 2))
    (zero? (mod (list->integer (list (nth ls 2) (nth ls 3) (nth ls 4))) 3))
    (zero? (mod (list->integer (list (nth ls 3) (nth ls 4) (nth ls 5))) 5))
    (zero? (mod (list->integer (list (nth ls 4) (nth ls 5) (nth ls 6))) 7))
    (zero? (mod (list->integer (list (nth ls 5) (nth ls 6) (nth ls 7))) 11))
    (zero? (mod (list->integer (list (nth ls 6) (nth ls 7) (nth ls 8))) 13))
    (zero? (mod (list->integer (list (nth ls 7) (nth ls 8) (nth ls 9))) 17))))

(defn p43 []
  (apply + (map list->integer 
                (filter p43-aux (permutations [0 1 2 3 4 5 6 7 8 9])))))

;(println "p43" (p43))


;; p44
;; Pentagon numbers
;; Pn=n(3n-1)/2
;; Pi=Pj+Pk, Pl=|Pk-Pj| となるものな中で
;; |Pk-Pl|の最小値

(defn pentagonal-number
  [n]
  (/ (* n (- (* 3 n) 1)) 2))

;(defn pentagonal-number?
;  [n]
;  (loop [i 1]
;    (let [x (pentagonal-number i)]
;      (cond
;        (< n x) false
;        (= n x) (do (println n) true)
;        :else (recur (+ i 1))))))

;二次方程式の解の公式を使う
;P=n(3n-1)/2
;3n^2-n-2P=0 が自然数の解を持てば良い
;-> (1+√1+24P)/6 が自然数となる
(defn pentagonal-number?
  [n]
  (zero? (mod (+ 1 (Math/sqrt (+ 1 (* 24 n)))) 6)))

(defn p44 []
  (loop [j 1 k 2]
    (cond 
      (= j 0) (recur k (+ k 1))
      (and (pentagonal-number? (- (pentagonal-number k) (pentagonal-number j)))
           (pentagonal-number? (+ (pentagonal-number j) (pentagonal-number k))))
        (- (pentagonal-number k) (pentagonal-number j))
      :else (recur (- j 1) k))))

(println "p44" (p44))


;; p45
;; Triangular, pentagonal, and hexagonal
;; Triangle: Tn=n(n+1)/2
;; Pentagonal: Pn=n(3n−1)/2
;; Hexagonal: Hn=n(2n−1)
;; T285 = P165 = H143 = 40755
;; Find the next triangle number that is also pentagonal and hexagonal

(defn hexagonal-number?
  [n]
  (zero? (mod (+ 1 (Math/sqrt (+ 1 (* 8 n)))) 4)))

(defn triangle-number
  [n]
  (/ (* n (+ n 1)) 2))

(defn p45 []
  (loop [i 286]
    (let [x (triangle-number i)]
      (if (and (pentagonal-number? x) (hexagonal-number? x))
          x
          (recur (+ i 1))))))

(println "p45" (p45))


;; p46
;; Goldbach's other conjecture
;; 素数+2*n^2の形で表せない奇数合成数の最小値
;; 合成数: 1とその数自身以外の約数を持つ自然数

(defn composite-number?
  [n]
  (and (not (= n 1)) (not (prime? n))))

(defn next-odd-composite-number
  [n]
  (loop [n (if (odd? n) (+ n 2) (+ n 1))]
    (if (composite-number? n)
        n
        (recur (+ n 2)))))

(defn goldbach?
  [n]
  (loop [p 2 i 1]
    (let [x (+ p (* 2 (square i)))]
      (cond
        (< n p) false
        (= n x) true
        (< n x) (recur (next-prime p) 1)
        :else (recur p (+ i 1))))))

(defn p46 []
  (loop [i 9]
    (if (goldbach? i)
        (recur (next-odd-composite-number i))
        i)))

(println "p46" (p46))


;; p47
;; Distinct primes factors
;; それぞれ4つの異なる素因数を持つ連続する4つの数の最初の数

(defn p47
  [n]
  (loop [i 1 c 0]
    (cond
      (= c n) (- i n)
      (= n (count (set (prime-factors i)))) (recur (+ i 1) (+ c 1))
      :else (recur (+ i 1) 0))))

(println "p47" (p47 4))


;; p48
;; Self powers
;; 1^1 + 2^2 + 3^3 + ... + 1000^1000 の末尾10桁

(defn p48 
  [n]
  (loop [i 1 ret 0N]
    (if (= i (+ n 1))
        ret
        (let [x (+ ret (pow i i))
              d (digit x)]
          (recur (+ i 1) (bigint (subs (str x) (max 0 (- d 10)) d)))))))

(println "p48" (p48 1000))


;; p49
;; Prime permutations
;; 3330で増加する連続する3つの数で、
;; 3数全て素数かつ4桁の数がそれぞれ互いの順列

(defn prime-permutations?
  [ls]
  (and (every? prime? ls)
       (= 1 (count (set (map 
                         (fn [x] (apply str (sort (str x)))) 
                          ls))))))

(defn p49 []
  (loop [a 1000]
    (let [b (+ a 3330)
          c (+ b 3330)]
      (cond
        (= a 1487) (recur (+ a 1))
        (prime-permutations? (list a b c)) (str a b c)
        :else (recur (+ a 1))))))

(println "p49" (p49))


;; p50
;; Consecutive prime sum
;; 最も連続した素数の和として書ける100万未満の素数

(defn prime-list
  [limit]
  (loop [i 2 ret []]
    (if (< limit i)
        ret
        (recur (next-prime i) (conj ret i)))))

(defn p50 
  [limit]
  (def pls (prime-list (/ limit 2)))
  (def len (count pls))
  (loop [i 0 x 0 chain 0 max-chain 0 ret 0]
    (cond
      (and (= chain 0) (= i (- len 1))) ret
      (or (< limit x) (= i (- len 1)))
        (recur (+ (- i chain) 1) 0 0 max-chain ret)
      (and (prime? x) (< max-chain chain))
        (recur (+ i 1) (+ x (nth pls i)) (+ chain 1) chain x)
      :else 
        (recur (+ i 1) (+ x (nth pls i)) (+ chain 1) max-chain ret))))

(println "p50" (p50 1000000))


;; p51
;; Prime digit replacements
;; 56**3 の*の部分を 0 ~ 9で置換
;; 56003, 56113, 56333, 56443, 56663, 56773, 56993 の7つの素数
;; 8つの素数があるパターン

;(defn insert-at
;  [ls i x]
;  (concat (take i ls) (list x) (drop i ls)))

;(make-replacements "56xx3" "x" ["0" "1" "2"]) -> ["56003" "56113" "56223"] 
(defn make-replacements
  [s match ls]
  (map (fn [x] (string/replace s match x)) ls))

;"56xx3" -> [56003 56113 ... 56993]
(defn p51-digit-replacements
  [s]
  (map-parse-int 
   (make-replacements s "x" ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"])))

;(defn str> [a b] (= 1 (String/.compareTo a b)))

;563 -> ["5xx63" "5x6x3" "5x63x" "56xx3" "56x3x" "563xx"] 
(defn p51-make-replace-targets
  [n]
  (filter (fn [s] (and (not= (subs s 0 1) "x") 
                       (= (str n) (string/replace s #"x" ""))))
          (map (fn [l] (apply str l))
               (into [] 
                (set 
                 (permutations 
                  (concat (map str (integer->list n))
                          ["x" "x"])))))))

(println (make-replacements "56xx3" "x" ["0" "1" "2"]))
(println (p51-digit-replacements "56xx3"))
(println (p51-make-replace-targets 563))

(defn p51-aux
  [n x]
  (loop [ls (p51-make-replace-targets n)]
    (if (empty? ls) 
        nil
        (let [ps (filter prime? (p51-digit-replacements (first ls)))] 
          (if (= x (count ps))
              (apply min ps)
              (recur (rest ls)))))))
  
(defn p51
  [x]
  (loop [i 1]
    (let [result (p51-aux i x)]
      (if (nil? result) (recur (+ i 1)) result))))

(println "p51" (p51 7))
;(println "p51" (p51 8))


;; p52
;; Permuted multiples
;; x, 2x, 3x, 4x, 5x, 6x 全てで同じ数字を持つ最小のx

(defn permuted-numbers?
  [ls]
  (apply = (map set (map integer->list ls))))

(println (permuted-numbers? [123 321 3132]))
(println (permuted-numbers? [1234 432 1423]))
(println (permuted-numbers? '(125874 251748)))

(defn p52
  [n]
  (loop [i 1]
    (if (permuted-numbers? (map (fn [x] (* i x)) (range 1 (+ n 1))))
        i
        (recur (+ i 1)))))

(println "p52" (p52 6))


;; p53
;; Combinatoric selections
;; 1 <= n <= 100 の間に nCrが1000000を超えるのは何通り

(defn p53
  [x]
  (loop [n 1 r 1 ret 0]
    (cond
      (= n 101) ret
      (= r n) (recur (+ n 1) 1 ret)
      (< x (combination n r)) (recur n (+ r 1) (+ ret 1))
      :else (recur n (+ r 1) ret))))

(println "p53" (p53 1000000))


;; p54
;; Poker hands
;; High Card: Highest value card.
;; One Pair: Two cards of the same value.
;; Two Pairs: Two different pairs.
;; Three of a Kind: Three cards of the same value.
;; Straight: All cards are consecutive values.
;; Flush: All cards of the same suit.
;; Full House: Three of a kind and a pair.
;; Four of a Kind: Four cards of the same value.
;; Straight Flush: All cards are consecutive values of same suit.
;; Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

(def p54arg (slurp "p054_poker.txt"))
(def pokerhands (parse-enumerate-with-space->matrix p54arg))

(defn p1hand [ls] (take 5 ls))
(defn p2hand [ls] (drop 5 ls))

(def poker-card-value
  {"1" 1 "2" 2 "3" 3 "4" 4 "5" 5
   "6" 6 "7" 7 "8" 8 "9" 9 "T" 10
   "J" 11 "Q" 12 "K" 13 "A" 14})

(defn card-value [card] (poker-card-value (subs card 0 1)))
(defn card-suit [card] (subs card 1 2))

(defn rember
  [ls x]
  (loop [ls ls ret []]
    (cond
      (empty? ls) ret
      (= (first ls) x) (recur (rest ls) ret)
      :else (recur (rest ls) (conj ret (first ls))))))

(defn count-x
  [ls x]
  (count (filter (fn [n] (= n x)) ls)))

(defn same-all?
  [ls]
  (= 1 (count (set ls))))

(defn consecutive?
  [ls]
  (= (sort ls) (range (apply min ls) (+ 1 (apply max ls)))))

(defn eph-sort-cond
  [a b]
  (if (= (second a) (second b))
      (> (first a) (first b))
      (> (second a) (second b))))

;["2H" "5C" "3D" "2D" "3S"] -> ((3 2) (2 2) (5 1))
(defn encode-poker-hand
  [ls]
  (loop [lv (map card-value ls) ret []]
    (if (empty? lv) 
        (sort eph-sort-cond ret)
        (let [x (first lv)]
          (recur (rember lv x) (cons (list x (count-x lv x)) ret))))))

(println (encode-poker-hand ["2H" "5C" "3D" "2D" "3S"]))
(println (encode-poker-hand ["QH" "5C" "AD" "KD" "JS"]))
(println (encode-poker-hand ["2H" "3C" "3D" "2D" "3S"]))

(defn poker-one-pair?
  [enc]
  (and (= (second (first enc)) 2) 
       (= (count enc) 4)))

(defn poker-two-pairs?
  [enc]
  (and (= (second (first enc)) 2)
       (= (count enc) 3)))
  
(defn poker-three-cards?
  [enc]
  (and (= (second (first enc)) 3)
       (= (count enc) 3)))

(defn poker-straight?
  [enc]
  (and (= (count enc) 5)
       (consecutive? (sort (map first enc)))))

(defn poker-full-house?
  [enc]
  (and (= (second (first enc)) 3)
       (= (count enc) 2)))

(defn poker-four-card?
  [enc]
  (= (second (first enc)) 4))

(defn poker-flush?
  [ls]
  (same-all? (map card-suit ls)))

(defn eval-poker-value
  [ls]
  (loop [enc (encode-poker-hand ls)
         k 100000000
         ret 0]
    (if (empty? enc)
        ret
        (let [e (first enc)]
          (recur (rest enc) (/ k 100)
                 (+ ret (* (first e) k (second e))))))))

(defn eval-poker-hand-aux
  [ls]
  (def f (if (poker-flush? ls) 80000000000 0))
  (def enc (encode-poker-hand ls))
  (cond
    (poker-four-card? enc) 130000000000
    (poker-full-house? enc) 120000000000
    (poker-straight? enc) (+ f 60000000000)
    (poker-three-cards? enc) 50000000000
    (poker-two-pairs? enc) (+ f 30000000000)
    (poker-one-pair? enc) (+ f 20000000000)
    :else f))

(defn eval-poker-hand
  [ls]
  (+ (eval-poker-hand-aux ls)
     (eval-poker-value ls)))

(defn p54 
  [hands]
  (loop [ls hands ret 0]
    (cond
      (empty? ls) ret
      (< (eval-poker-hand (p2hand (first ls))) 
         (eval-poker-hand (p1hand (first ls))))
        (recur (rest ls) (+ ret 1))
      :else (recur (rest ls) ret))))

(println "p54" (p54 pokerhands))


;; p55
;; Lychrel numbers
;; 反転させた数を足すことを何度(50未満)繰り返しても答えが回文数にならない数
;; 10000未満のLychrel数の個数

(defn lychrel-number?
  [n]
  (loop [n n c 0]
    (let [x (+ n (bigint (apply str (reverse (str n)))))]
      (cond
        (= c 50) true
        (palindrome-number? x) false
        :else (recur x (+ c 1))))))

(defn p55
  [limit]
  (loop [i 1 ret 0]
    (cond
      (= i limit) ret
      (lychrel-number? i) (recur (+ i 1) (+ ret 1))
      :else (recur (+ i 1) ret))))

(println "p55" (p55 10000))


;; p56
;; Powerful digit sum

(defn sum-digit
  [n]
  (apply + (integer->list n)))

(defn p56 []
  (loop [a 1 b 1 ret 0]
    (cond
      (and (= a 100) (= b 100)) ret
      (= b 100) (recur (+ a 1) 1 ret)
      :else (recur a (+ b 1) (max ret (sum-digit (pow a b)))))))

(println "p56" (p56))


;; p57
;; Square root convergents
;; 分子の桁数が分母の桁数より大きい分数

(defn p57
  [n]
  (loop [i 0 x (/ 1 2) ret 0]
    (if (= i n) 
        ret
        (let [nx (/ 1 (+ 2 x))
              m (+ 1 nx)]
          (if (< (digit (denominator m)) (digit (numerator m)))
              (recur (+ i 1) nx (+ ret 1))
              (recur (+ i 1) nx ret))))))

(println "p57" (p57 1000))


;; p58
;; Spiral primes

(defn p58
  [percent]
  (loop [i 1 xc 1 pc 0 
         layer 1 c 1]
    (let [x (+ i (* 2 layer))]
      (cond 
        (not= c 4) (recur x (+ xc 1) (if (prime? x) (+ pc 1) pc)
                          layer (+ c 1))
        (< (* 100 (/ pc xc)) percent) (- (* 2 layer) 1)
        :else (recur x (+ xc 1) (if (prime? x) (+ pc 1) pc)
                     (+ layer 1) 1)))))

(println "p58" (p58 10))


;; p59
;; XOR decryption


;; p60
;; Prime pair sets
;; 任意の 2 つの素数を連結して別の素数を生成する 5つの素数

(defn combinations
  [ls n]
  (cond
    (<= (count ls) n) (list ls)
    (= n 1) (map list ls)
    :else (concat (map (fn [l] (cons (first ls) l)) (combinations (rest ls) (- n 1))) 
                  (combinations (rest ls) n))))

(println (combinations [1 2 3 4] 2))
  
(defn prime-pair-sets?
  [ls]
  (if (empty? ls)
      false
      (let [l (combinations ls 2)]
        (every? prime? (map (fn [x] (Integer/parseInt (apply str x))) 
                       (concat l (map reverse l)))))))

(println (prime-pair-sets? [3 7 109 673]))
(println (prime-pair-sets? [3 7 13 109]))

(defn p60 
  [n]
  (loop [i 3 ls (map (fn [x] (list)) (range n))]
    (if (not (empty? (first ls))) 
        (apply + (first (first ls)))
        (let [l (concat (rest ls) (list (list (list))))
              l* (map (fn [x] (map (fn [y] (cons i y)) x)) l)
              ls* (map (fn [x] (filter prime-pair-sets? x)) l*)]
          (recur (next-prime i) (map concat ls ls*))))))

(println "p60" (p60 4))
;(println "p60" (p60 5))


;; p61
;; Cyclical figurate numbers

(defn triangle
  [n]
  (/ (* n (+ n 1)) 2))

(defn square
  [n]
  (* n n))

(defn pentagonal
  [n]
  (/ (* n (- (* 3 n) 1)) 2))

(defn hexagonal
  [n]
  (* n (- (* 2 n) 1)))

(defn heptagonal
  [n]
  (/ (* n (- (* 5 n) 3)) 2))

(defn octagonal
  [n]
  (* n (- (* 3 n) 2)))

(defn xgonal-4digits
  [f]
  (loop [i 1 ret []]
    (let [n (f i)]
      (cond
        (< 9999 n) ret
        (< 999 n) (recur (+ i 1) (conj ret n))
        :else (recur (+ i 1) ret)))))

;1234 -> ["12" "34"]
(defn sprit-4digit->2digit
  [n]
  (let [s (str n)]
    (list (subs s 0 2) (subs s 2 4))))

;[["12" "34"] ["12" "56"] ["34" "56"]]
;-> {"12" ["34" "56"], "34" ["56"]}
(defn make-hashmap61
  [ls]
  (loop [ls ls ret {}]
    (if (empty? ls) 
        ret
        (let [x (first ls)]
          (if (contains? ret (first x))
              (recur (rest ls) (assoc ret (first x) (cons (second x) (get ret (first x)))))
              (recur (rest ls) (assoc ret (first x) (list (second x)))))))))

(defn p61-aux
  [ls]
  (println ls))

(defn p61 []
  (let [tri (xgonal-4digits triangle)
        squ (xgonal-4digits square)
        pen (xgonal-4digits pentagonal)
        hex (xgonal-4digits hexagonal)
        hep (xgonal-4digits heptagonal)
        oct (xgonal-4digits octagonal)

        tri2 (map sprit-4digit->2digit tri)
        squ2 (map sprit-4digit->2digit squ)
        pen2 (map sprit-4digit->2digit pen)
        hex2 (map sprit-4digit->2digit hex)
        hep2 (map sprit-4digit->2digit hep)
        oct2 (map sprit-4digit->2digit oct)

        trih (make-hashmap61 tri2)
        squh (make-hashmap61 squ2)
        penh (make-hashmap61 pen2)
        hexh (make-hashmap61 hex2)
        heph (make-hashmap61 hep2)
        octh (make-hashmap61 oct2)]
    (p61-aux (list trih squh penh hexh heph octh))))

;(println (p61))


;; p62
;; Cubic permutations
;; 41063625は 345^3 で　桁を入れ替えた　56623104は 384^3 さらに　66430125は 405^3
;; このように立方数になる桁の置換が5つある最小の立方数は?

(defn cube [n] (* n n n))

(defn cube?
  [n]
  (loop [i 1]
    (cond
      (< n (cube i)) false
      (== n (cube i)) true
      :else (recur (+ i 1)))))

;123 -> [123 132 213 231 312 321]
(defn digit-permutations
  [n]
  (map bigint (map (fn [ls] (apply str ls))
                   (permutations (integer->list n)))))

(defn p62-v0 
  [n]
  (loop [i 1] 
    (let [ls (set (filter (fn [x] (and (cube? x) (<= (cube i) x)))
                          (digit-permutations (cube i))))]
      (if (= (count ls) n)
          ls
          (recur (+ i 1))))))

(println (digit-permutations 123))
(println (set (filter cube? (digit-permutations 41063625))))

;;p62別解 (v0は遅すぎる)

;41063625 -> "01234566"
;56623104 -> "01234566"
(defn make-key62
  [n]
  (apply str (sort (integer->list n))))

(defn p62
  [n]
  (loop [i 1 hm {}]
    (let [c (cube i)
          k (make-key62 c)
          l (get hm k)]
      (cond 
        (nil? l) (recur (+ i 1) (assoc hm k (list c)))
        (= (count l) (- n 1)) (apply min (cons c l))
        :else (recur (+ i 1) (assoc hm k (cons c l)))))))

(println (p62 3))
(println "p62" (p62 5))


;; p63
;; Powerful digit counts
;; 自然数xをn乗して得られるn桁の整数は何個あるか?
;; 10のn乗はn+1桁となるためxは10まで

(defn p63 []
  (loop [x 1 n 1 ret 0]
    (let [d (digit (pow x n))]
      (cond 
        (= x 10) ret
        (> n d) (recur (+ x 1) 1 ret)
        (= n d) (recur x (+ n 1) (+ ret 1))
        :else (recur x (+ n 1) ret)))))

(println "p63" (p63))


;; p65
;; Convergents of e

(defn convergents
  [x ls]
  (defn aux 
    [ls]
    (if (nil? (second ls))
        (first ls) 
        (+ (first ls) (/ 1 (aux (rest ls))))))
  (+ x (/ 1 (aux ls))))

(defn seq65-for-e
  [n]
  (take n (flatten 
           (map (fn [i] [1 (* i 2) 1]) 
                (range 1 (+ 2 (quot n 3)))))))

(defn p65
  [n]
  (apply + (integer->list 
            (numerator (convergents 2 (seq65-for-e (- n 1)))))))

(println (convergents 1 [2 2 2]))
(println (convergents 2 [1 2 1 1 4 1]))
(println (seq65-for-e 10))
(println (p65 10))
(println "p65" (p65 100))


;; p67
;; Maximum path sum II
;; p18の改良版で解く
;; 1
;; 1 2
;; 3 2 4
;; 1 2 3 4
;; ->
;; 1
;; 1 2
;; 5 5 8
;; ->
;; 1
;; 6 10
;; ->
;; 11

(def p67arg
  (parse-enumerate-numbers-with-space->matrix
   (slurp "p067_triangle.txt")))

;隣接する2数から大きい方の値を残す
;(6 4 3 5 2) -> (6 4 5 5)
(defn pick-max-adjacent
  [ls]
  (loop [ls ls ret []]
    (if (= 1 (count ls))
        ret
        (recur (rest ls) (conj ret (max (first ls) (second ls)))))))

(defn p67
  [triangle]
  (loop [ls (reverse triangle)
         l (map (fn [i] 0) (first ls))]
    (if (= (count ls) 1)
        (+ (first (first ls)) (first l))
        (recur (rest ls)
               (pick-max-adjacent (map + (first ls) l))))))

(println (pick-max-adjacent [6 4 3 5 2]))
(println (p67 [[1] [1 2] [3 2 4] [1 2 3 4]]))
(println "p67" (p67 p67arg))


;; p69
;; Totient maximum

(defn totient
  [n]
  (if (= n 1)
      1
      (apply * (cons n (map (fn [p] (- 1 (/ 1 p))) 
                            (set (prime-factors n)))))))

(println (totient 1))
(println (totient 2))
(println (totient 3))
(println (totient 4))
(println (totient 5))

(defn p69
  [n]
  (loop [n n maximum 0 ret 0]
    (if (= 1 n) 
        ret
        (let [x (/ n (totient n))]
          (if (< maximum x)
              (recur (- n 1) x n)
              (recur (- n 1) maximum ret))))))

(println (p69 10))
;(println "p69" (p69 1000000))


;; p70
;; Totient permutation
;; (totient 87109) => 79180 となる数字の中で
;; (/ n (totient n))が最小となるn

(defn permutation-numbers?
  [a b]
  (== (list->integer (sort > (integer->list a)))
      (list->integer (sort > (integer->list b)))))

(defn p70 
  [n]
  (loop [i 3 tmin ##Inf ret ##NaN]
    (let [t (totient i)]
      (cond
        (< n i) ret
        (and (permutation-numbers? i t) (< (/ i t) tmin)) 
          (recur (+ i 2) (/ i t) i)
        :else (recur (+ i 2) tmin ret)))))

(println (permutation-numbers? 87109 (totient 87109)))
;(println "p70" (p70 10000000))  ;遅すぎるけど...


;; p71
;; Ordered fractions
;; n/d (n<d d<=1000000)のを昇順に並べて3/7のすぐ左にある分数の分子

(defn p71 
  [limit]
  (loop [d 1 n 1 ret 0]
    (let [x (/ n d)]
      (cond
        (< limit d) (numerator ret)
        (< (/ 3 7) x) (recur (+ d 1) n ret)
        (and (< x (/ 3 7)) (< 0 x)) (recur d (+ n 1) x)
        :else (recur d (+ n 1) ret)))))

(println "p71" (p71 1000000))
