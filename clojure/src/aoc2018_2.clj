(ns aoc2018-2)
(use 'clojure.data)                                         ; use -> required 를 사용하는것을 추천
(use '[clojure.string :all :refer])

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

(defn inc-char-count
  "docstring"
  [dic, char]
  (let [cnt (dic char)]
    (if (= cnt nil)                                         ; 긍정 조건으로 고쳐보기
      (assoc dic char 1)
      (update dic char inc)))
  )
; recommended
;(defn inc-char-count [dic, char]
;  (let [cnt (dic char)
;        cnt-to-update (if cnt (inc cnt) 1)]
;    (assoc dic char cnt-to-update))


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
; recommended
;{:appeared-twice? N :appeared-three-times? N}


(defn sum-each-result [acc, cur]
  [
   (+ (nth acc 0) (nth cur 0))                              ;구조분해 할당을 이용하는 것을 추천
   (+ (nth acc 1) (nth cur 1))
   ])

(defn multiply [[twice threetimes]]
  (* twice threetimes))                                     ;(apply * LIST) 리스트의 모든 element 곱해짐

(defn multiply-twice-three-times [strings]
  (->> strings
       (map added-count-char)
       (map mentioned-twice-threetimes)
       (reduce sum-each-result [0 0])
       multiply
       ))


(comment
  (multiply-twice-three-times `("abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"))
  ; ("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")
  ; 1. ({} {a: 1, b: 2, c: 3}...)
  ; 2. ([0, 0] [1, 1]...)
  ; 3. [1, 1]
  ; 4. 1

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

(defn string->list [string]                                 ;string->vector 이름 추천
  (into [] (char-array string)))                            ; char-array 사용하지 않는것이 좋음 -> return이 java 의 네이티브 객체
  ; map / reduce / vec 를 사용하면 seq -> vector
(vec "aaaaa")                                               ;recommended


(defn compare-ids [target ids]
   (let [targetArr (string->list target)]
     (let [diff (map #(diff targetArr (string->list %)) ids)]
       (->> diff
            (map #(remove nil? (nth % 2)))))
     ))
; recommended
;(->> ids
;     (map string->list)
;     (map #(diff targetArr %))


(defn is-not-last-element [strings]                         ; is / has 식의 함수명 보 not-xxx? 으로 짓는것을 추천 (물음표)
  (> (count (rest strings)) 0))                             ; 이경우 추천 : butlast? (when-not (last-element?  string))
;recommended
;(not(empty? (rest `())))
; count O(n) 성능 연결됨

(defn search-pair-id [strings]
  (let [diff (compare-ids (first strings) (rest strings))]  ; let 을 한번으로 축소 하는것을 추천 (가독성) / let 으로 정의 하는 부분을 함수로 빼는것도 추천
    (let [found-pair (                                      ; 함수로 별도로 추출하는 것을 추천
                ->> diff
                   (remove empty?)
                   (filter #(= (count %) 4))
                   first
                   join)]
      (if (blank? found-pair)
        (if (is-not-last-element strings)
          (search-pair-id (rest strings))                   ; 재귀 호출 스택 쌓이면 실패 / clojure 재귀 안쓰도록 바꿀수 있음 collection 라이브러리 활용.
          nil)                                              ; 해당 조건에는 when 을 사용하는 것을 추천
        found-pair)
      )
  ))

(comment
  (search-pair-id `("abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"))
  (compare-ids "fghij" `("fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"))))
