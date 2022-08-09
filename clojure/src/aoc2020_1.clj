(ns aoc2020_1
  (:require [clojure.math.combinatorics :refer [combination]]))

;;## 파트 1
;;더해서 2020이 되는 두 숫자의 곱을 구하시오. (두 숫자는 유일하다고 가정)
;;
;;예) 1721 979 366 299 675 1456 의 경우 1721 * 299 = 514579 를 출력
;;
;;## 파트 2
;;같은 입력이 주어질 때, 더해서 2020이 되는 세 숫자의 합을 구하시오.
;;
;;예) 2020 = 979 + 366 + 675, 곱하면 241861950 을 출력

(def input `(1721
              979
              366
              299
              675
              1456))

(def equal-2020?
  "요소들의 합이 2020 인지 확인"
  (fn [numbers]
    (->> numbers
         (apply +)
         (= 2020))))

(defn find-equally-sum
  "n개의 요소씩 묶인 리스트에서 요소들의 합이 2020과 같은 리스트를 찾는다"
  [elements]
  (->> (filter equal-2020? elements)
    first))

(comment
   (->> (combination input 2)
        find-equally-sum
        (apply *)))

(comment
  (->>
    (-> input
        (combinations 2)
        find-equally-sum)
    (apply *))

  (->>
    (-> input
        (combinations 3)
        find-equally-sum)
    (apply *)))

; #_ 를 통해 함수별 주석 가능
; clojure.math.combinatorics / combo 집합에서 부분집합 가져오는 함수
