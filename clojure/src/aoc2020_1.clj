(ns aoc2020_1)

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

(defn input->cases-2elements
  "리스트에서 두개의 요소들의 집합을 유니크로 케이스들로 가져온다..

  입력 :
  numbers: `(1721 979 366) ; 더하기를 할 숫자들

  출력: #{979 366}
  "
  [numbers]
  (->> (for [first-element numbers
        second-element (disj (set numbers) first-element)]
         (sort [first-element second-element]))
       set))


(defn input->cases-3elements
  "리스트에서 두개의 요소들의 집합을 유니크로 케이스들로 가져온다..

  입력 :
  numbers: `(1721 979 366) ; 더하기를 할 숫자들

  출력: #{979 366}
  "
  [numbers]
  (->> (for [first-element numbers
             second-element (disj (set numbers) first-element)
             third-element (disj (set numbers) [first-element second-element])]
         (sort [first-element second-element third-element]))
       set))

(def equal-2020?
  "요소들의 합이 2020 인지 확인"
  (fn [numbers]
    (->> numbers
         (cons 2020)
         (apply -)
         zero?)))

(defn find-equally-sum
  "n개의 요소씩 묶인 리스트에서 요소들의 합이 2020과 같은 리스트를 찾는다"
  [elements]
  (->> (filter equal-2020? elements)
    first))


(comment
  (->> input
       input->cases-2elements
       find-equally-sum
       (apply *)
       )

  (->> input
       input->cases-3elements
       find-equally-sum
       (apply *)
       )
  )

\