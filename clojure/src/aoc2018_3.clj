(ns aoc2018_3
  (:require [clojure.set]))


;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)

(defn square-info->coordinates
  "시작 부터 끝좌표 까지 거져가는 모든 좌표들 남기기

  Arguments
  - start-x (int) x 시작 좌표
  - end-x (int) x 종료 좌표
  - start-y (int) y 시작 좌표
  - end-y (int) y 종료 좌표

  Return
  [(string) vector] [str 'x좌표' 'x' 'Y좌표']"
  [{:keys [start-x start-y end-x end-y]}]
  (for [x (range start-x end-x)
        y (range start-y end-y)]
    {:x x :y y}))

(defn input->square-info
  "입렵박은 문자열을 사각형의 좌표 정보로 변경한다."
  [string]
  (let [matched-info (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" string)
        square-info-string (subvec matched-info 2)
        square-info-int (map #(Integer. %) square-info-string)
        [start-x start-y width height] square-info-int]
    {:start-x start-x
     :start-y start-y
     :end-x (+ start-x width)
     :end-y (+ start-y height)}))

(defn input->coordinates
  "입력받은 문자열을 사각형들이 가지고 있는 좌표값으로 변경한다."
  [input]
  (->> input
       (map input->square-info)
       (map square-info->coordinates)))

(defn filter-overlapped-coordinates
  "사각형의 겹치는 (중첩된 좌표) 만 걸러내기"
  [coordinates]
  (->> coordinates
       (apply concat)
       frequencies
       (filter (fn [[k v]] (> v 1)))))

(defn overlapped-count
  "문자열에서 좌표에 아이디를 할당해 아이디가 중첩된 좌표의 갯수를 반환

  Arguments
  - strings ((string)[list])

  Return
  - (int) 중첩된 좌표의 수
  "
  [input]
  (->> input
       input->coordinates
       filter-overlapped-coordinates
       count
       ))

(comment
  (overlapped-count `("#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2")))

;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)


(defn footprint-coordinates-as-hashmap [{:keys [id starting-coordinate size]}]
  "사각형의 id 별로 사각형이 포함된 좌표를 반환한다.

  Arguments
  - (hashmap) {:id (string) :starting-coordinate ([int] :size ([int]}

  Return
  - (hash-map) {:id (string) :coordinates ((string) vector)} 아이디와 좌표목록을 갖는 hash-map
  "
  {:id id :coordinates (footprint-coordinates starting-coordinate size)})

(defn compare-far-off
  "대상 사각형이 다른 사각형들과 겹치는 부분이 있는지 확인

  Arguments
  - coordinates ((string) vector) 대상 사각형이 가지고 있는 좌표들
  - rects 비교할 사각형들의 정보

  Returns
  - (Boolean)
  "
  [coordinates, rects]
  (let [intersections (filter (fn [rect]
                                (let [intersection (clojure.set/intersection (set (rect :coordinates)) (set coordinates))]
                                  (->> intersection
                                       empty?
                                       not))
                                ) rects)]
    (empty? intersections)))
; recommended 아래 로직 별도 함수 분리
;(let [intersection (clojure.set/intersection (set (rect :coordinates)) (set coordinates))]
;  (->> intersection
;       empty?
;       not))


(defn remove-idx [i items]
  "list 에서 특정 인덱스에 해당하는 항목을 삭제한 list 를 반환

  Argument
  - i (int) 삭제할 인덱스
  - items ((any) list) 대상 리스트

  Return
  - ((any list) 대상 인덱스를 삭제한 결과의 list
  "
  (keep-indexed #(when-not (= i %1) %2) items))

; recommended
; hash-map id 로 찾아서 삭제 .

(defn not-overlapped
  "겹치는 부분이 없는 사각형의 id 를 가져오는 반환

  Arguments
  - rect-coordinates ((hash-map) list) 사각형들 정보

  Returns
  - (string) 사각형 id
  "
  [rect-coordinates]
  (loop [index 0
         {target-id :id
          target-coordinates :coordinates} (nth rect-coordinates index)]
    (let [far-off? (compare-far-off target-coordinates (remove-idx index rect-coordinates))
          index (inc index)]
      (if far-off?
        target-id
        (recur index (nth rect-coordinates index))))))

(defn rect-far-off
  "좌표에서 사각형이 다른것과 겹치지 않는 사각형의 아이디 가져오기

  Arguments
  - strings ((string)[list])

  Return
  - (int) 중첩된 좌표의 수
  "
  [strings]
  (->> strings
       (map parse-coordinate-info)
       (map footprint-coordinates-as-hashmap)

       ))

(comment
  (rect-far-off `("#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2")))
