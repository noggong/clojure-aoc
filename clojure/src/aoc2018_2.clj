(ns aoc2018-2
  (:require [clojure.string :as str]                        ; str 이라는 alias 는 지양함 / 이미 str core 함수가 존재
            [clojure.data :as data]))

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

(defn add-count-by-char
  "문자열을 각 char로 쪼개어 char별 언급횟수를 hash-map 으로 만든다

  Arguments
  - string (string) 대상 문자역

  Return
  - (hash-map) {:char (Int)count}
  "
  [string]
  (->> string
       vec
       frequencies))

(defn check-mentioned-count?
  "char 별 언급횟수가 표현된 hash-map 에서 특정횟수를 언급한 char 가 있는지 확인

  Arguments
  - counted-char-map (hash-map) {:char (Int)count}
  - mentioned-count (Int)

  Return
  - (Boolean) 언급횟수가 일치하는 char 존재: true, 없다면 : false
  "
  [counted-char-map mentioned-count]
  (->> counted-char-map
       (filter (fn [[k v]] (= v mentioned-count)))
       empty?
       not
       ))

; empty? not 의 조합을 seq 로 써서 truthy , falsy 한 값으로 확인 가능.

(defn mentioned-twice-threetimes
  "char 별 언급횟수가 표현된 hash-map 에서 두번과 세번이 언급된 char 가 있는지 확인

  Arguments
  - counted-char-map (hash-map) {:char (Int)count}

  Return
  - (hash-map) { :appeared-twice? (Boolean) 두번언급 존재 여부
                 :appeared-three-times? (Boolean) 세번언급 존재여부 }
  "
  [counted-char-map]
  {:appeared-twice? (check-mentioned-count? counted-char-map 2)
    :appeared-three-times? (check-mentioned-count? counted-char-map 3)})

(defn gather-mentioned
  "두번, 세번 언급된적 있는 문자열들이 몇개가 있는지 취합한다.

  Arguments
  - checked-mentioned-map [(hash-map)]

  Return
  - (vector) [(Int)두번언급된 횟수 (Int)세번 언급된 횟수]
  "
  [checked-mentioned-map]
  [
   (get (frequencies (map :appeared-twice? checked-mentioned-map)) true)
   (get (frequencies (map :appeared-three-times? checked-mentioned-map)) true)])


; recommended
;(defn mentioned? [checked-mentioned-map key]
;  (->> (map key checked-mentioned-map)
;       frequencies
;       (#(get % true))))
;
;(defn gather-mentioned
;  [checked-mentioned-map]
;  (->> [:appeared-twice? :appeared-three-times?]
;       (map (fn [appear] (mentioned? checked-mentioned appear)))))


(defn multiply-twice-three-times
  "문자열들에서 두번, 세번 언급된 문자의 존재 여부를 판단하여 갯수를 확인후 두번갯수 , 세번갯수를 더한다.

  Arguments
  - ((string)list) 문자열 리스트

  Return
  - (Int) 두번갯수 와 세번갯수의 곱
  "
  [strings]
  (->> strings
       (map add-count-by-char)
       (map mentioned-twice-threetimes)
       gather-mentioned
       (apply *)))



(comment
  (multiply-twice-three-times `("abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"))

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

(defn compare-ids
  "대상이 되는 문자열과 ids 들의 각각 id 를 비교하여 동일위치, 동일문자의 집합을 반환한다.

  Argument
  - target (string) 대상문자열
  - ids ((string)list) 대상문자열과 비교할 id 문자열의 리스트

  Return
  - ((string) list)
  "
  [target ids]
  (let [targetArr (vec target)
         compared-result (->> ids
                              (map vec)
                              (map #(data/diff targetArr %)))]
     (->> compared-result
          (map #(remove nil? (nth % 2))))))

(defn not-last?
  "문자열 배열이 마지막 문자열만 존재하는지 판단

  Argument
  - ((string) list)

  Return
  - (boolean) 마지막이면 false 마지막이 아니면 true
  "
  [strings]
  (not (empty? (rest strings))))

(defn find-pair-id
  "짝이 되는 id 를 찾아 반환, 짝이란 5자리중 4자리가 동일위치 동일문자가 위치한다.

  Argument
  - ((string) list) 대상과 비교한 결과 list

  Return
  - (string?) 짝이 되는 id를 반환, 없다면 nil 을 반환
  "
  [compared-result]
  (->> compared-result
       (remove empty?)
       (filter #(= (count %) 4))
       first
       str/join))

(defn search-pair-id [strings]
  "문자열들에서 짝이 되는 아이디를 찾아 중첩된 문자열만을 반환

  Arguments
  - ((string)list) 문자열 리스트

  Return
  - (string) 짝이 되는 아이디의 중첩 문자열
  "
  (let [compared-result (compare-ids (first strings) (rest strings))
        found-pair (find-pair-id compared-result)]
    (if (str/blank? found-pair)
      (when (not-last? strings)
        (recur (rest strings)))
      found-pair))))

(comment
  (search-pair-id `("abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"))
  (compare-ids "fghij" `("fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz")))
