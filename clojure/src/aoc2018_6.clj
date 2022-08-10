(ns aoc2018_6
  (:require [clojure.java.io :as io]))

;; 파트 1
;; 입력 : 좌표의 쌍이 N개 주어짐

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; 각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.


;;  ..........
;;  .A........
;;  ..........
;;  ........C.
;;  ...D......
;;  .....E....
;;  .B........
;;  ..........
;;  ..........
;;  ........F.


;;  aaaaa.cccc
;;  aAaaa.cccc
;;  aaaddecccc
;;  aadddeccCc
;;  ..dDdeeccc
;;  bb.deEeecc
;;  bBb.eeee..
;;  bbb.eeefff
;;  bbb.eeffff
;;  bbb.ffffFf


;; 여기서 . 으로 표기된 부분은 각 출발 지점으로부터 '같은 거리'에 있는 부분을 뜻함.
;; 맵 크기에는 제한이 없어 무한으로 뻗어나간다고 할 때, 가장 큰 유한한 면적의 크기를 반환 (part-1)

(def sample-input
  "1, 1
  1, 6
  8, 3
  3, 4
  5, 5
  8, 9")

(def aoc-input (slurp "resources/2018_6.txt"))

(defn matched->coord
  "re-seq 로 찾은 매치된 리스트에서 x , y 좌표로 분리 한다."
  [[_ x y]]
  {:id (str x y)
   :x (Integer/parseInt x)
   :y (Integer/parseInt y)})

(defn input->coord
  "입력받은 값을 좌표 정보로 변경
  input:
    1, 1
    1, 6
    8, 3
    3, 4
    5, 5
    8, 9
  output:
    `({:id \"11\", :x 1, :y 1}
    {:id \"16\", :x 1, :y 6}
    {:id \"83\", :x 8, :y 3}
    {:id \"34\", :x 3, :y 4}
    {:id \"55\", :x 5, :y 5}
    {:id \"89\", :x 8, :y 9})"
  [input]
  (->> (re-seq #"([0-9]+),\s([0-9]+)" input)
       (map matched->coord)))

(defn value-by-fn
  "리스트에서 함수에 의한 값을 지정된 key 에서 가져온다"
  [fn key coords]
  (apply fn (map #(% key) coords)))

(defn coords->boundary
  "좌표들로 보드의 바운더리을 가져온다"
  [coords]
  {:min-x (value-by-fn min :x coords)
   :min-y (value-by-fn min :y coords)
   :max-x (value-by-fn max :x coords)
   :max-y (value-by-fn max :y coords)})

(defn boundary->all-coords
  "경계선 정보로 전체 좌표를 가져온다"
  [{:keys [min-x min-y max-x max-y]}]
  (for [x (range min-x (inc max-x))
        y (range min-y (inc max-y))]
    {:x x
     :y y
     :owner "."}))

(defn manhattan-distance
  "맨하트 거리를 구한다.
  input: {:x 1 :y 2} {:x 5 :y 4}
  output: 6
  "
  [{from-x :x from-y :y} {to-x :x to-y :y}]
  (+ (abs (- from-x to-x))
     (abs (- from-y to-y))))

(def add-manhattan
  "좌표에 manhattan 정보를 넣는다.
  input:
   {:x 1, :y 1, :owner \".\"}
   `({:id \"11\", :x 1, :y 1} {:id \"16\", :x 1, :y 6})
  output: {:x 1, :y 1, :owner \"11\"}"
  (fn [coord start-coords]
    (let [filled-distance (map #(assoc % :distance (manhattan-distance coord %)) start-coords)
          sum-distance (reduce + (map :distance filled-distance))
          owner-coord (apply min-key :distance filled-distance)
          count-by-distance (frequencies (map :distance filled-distance))
          shortest-count (count-by-distance (:distance owner-coord))]
      (if (= 1 shortest-count)
        (assoc coord :owner (:id owner-coord) :distance-from-start-coords sum-distance)
        (assoc coord :distance-from-start-coords sum-distance)))))

(defn fill-manhattan
  "coords 을 점유한 start-coords 에 아이디를 채워준다
  input:
    `({:id \"11\", :x 1, :y 1} {:id \"16\", :x 1, :y 6})
    `({:x 1, :y 1, :owner \".\"}
      {:x 1, :y 2, :owner \".\"}
      {:x 1, :y 3, :owner \".\"}
      {:x 1, :y 4, :owner \".\"})
  output:
    `({:x 1, :y 1, :owner \"11\"}
      {:x 1, :y 2, :owner \"16\"}
      {:x 1, :y 3, :owner \"11\"}
      {:x 1, :y 4, :owner \".\"})
  "
  [start-coords coords]
  (map add-manhattan coords (repeat start-coords)))

(defn all-coords-on-board
  "보드가 가지고 있는 모든 좌표를 반환한다.
  input:
    {:min-x 1, :min-y 1, :max-x 8, :max-y 9}
    `({:id \"11\", :x 1, :y 1}
      {:id \"16\", :x 1, :y 6})
  "
  [boundary start-coords]
  (->> (boundary->all-coords boundary)
       (fill-manhattan start-coords)))

(defn coords->board
  "좌표를 보드정보로 변경한다.
  input:
    `({:id \"11\", :x 1, :y 1}
    {:id \"16\", :x 1, :y 6})
  output:
  {:all-coords ({:x 1, :y 1, :owner \"11\"}
                {:x 1, :y 2, :owner \"11\"}
                {:x 1, :y 3, :owner \"11\"})
  :boundary {:min-x 1, :min-y 1, :max-x 8, :max-y 9},
  :start-coords ({:id \"11\", :x 1, :y 1}
                {:id \"16\", :x 1, :y 6})"

  [start-coords]
  (let [boundary (coords->boundary start-coords)]
   {:all-coords (all-coords-on-board boundary start-coords)
    :boundary boundary
    :start-coords start-coords}))

(defn board->owner-without-boundary
  "모든 좌표에서 바운더리에 해당하는 근원지는 제외 시킨다."
  [{:keys [all-coords start-coords]}]
  (let [except-owner (set [(:id (apply min-key :x start-coords))
                           (:id (apply max-key :x start-coords))
                           (:id (apply min-key :y start-coords))
                           (:id (apply max-key :y start-coords))
                           "."])]
    (->> (filter #(not (contains? except-owner (:owner %))) all-coords)
        (map :owner))))

(comment
  (->> sample-input
       input->coord
       coords->board
       board->owner-without-boundary
       frequencies
       vals
       (apply max))

  (->> aoc-input
       input->coord
       coords->board
       board->owner-without-boundary
       frequencies
       vals
       (apply max)))

;; 파트 2
;; 안전(safe) 한 지역은 근원지'들'로부터의 맨하탄거리(Manhattan distance, 격자를 상하좌우로만 움직일때의 최단 거리)의 '합'이 N 미만인 지역임.

;;  ..........
;;  .A........
;;  ..........
;;  ...###..C.
;;  ..#D###...
;;  ..###E#...
;;  .B.###....
;;  ..........
;;  ..........
;;  ........F.

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.

(comment
  (->> sample-input
       input->coord
       coords->board
       :all-coords
       (map :distance-from-start-coords)
       (filter #(< % 40))
       count)

  (->> aoc-input
       input->coord
       coords->board
       :all-coords
       (map :distance-from-start-coords)
       (filter #(< % 10000))
       count))