(ns aoc2018-2)

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12

(defn inc-char-count [dic, char]
  (let [cnt (dic char)]
    (if (= cnt nil)
      (assoc dic char 1)
      (update dic char inc)))
  )

(defn added-count-char [string]
  (->> string
       char-array
       (reduce inc-char-count {}))
  )

(defn exists-nth-mentioned [char-count nth]
  (if (empty? (filter (fn [[k v]] (= v nth)) char-count)) 0 1))

(defn mentioned-twice-threetimes [char-count]
  [
   (exists-nth-mentioned char-count 2)
   (exists-nth-mentioned char-count 3)
   ])

(defn sum-each-result [acc, cur]
  [
   (+ (nth acc 0) (nth cur 0))
   (+ (nth acc 1) (nth cur 1))
   ])

(defn multiply [[twice threetimes]]
  (* twice threetimes))

(defn multiply-twice-three-times [strings]
  (->> strings
       (map added-count-char)
       (map mentioned-twice-threetimes)
       (reduce sum-each-result [0 0])
       multiply
       ))


(comment
  (multiply-twice-three-times `("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"))
  ; ("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")
  ; 1. ({a: 0, b: 0, c: 0} {a: 1, b: 2, c: 3}...)
  ; 2. ([0, 0] [1, 1]...)
  ; 3. [1, 1]
  ; 4. 1

  ;(let [[first second] (mentioned-twice-threetimes {\b 3, \a 3, \c 3})] [first, second] )
  ;(inc-char-count {} "a")
  ;(added-count-char "bababc")
  ;
  ;
  ;(reduce (fn [acc, cur]
  ;          (println acc, cur)
  ;          cur)
  ;        (char-array "abdcd")))

;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.


;; #################################
;; ###        Refactoring        ###
;; #################################
