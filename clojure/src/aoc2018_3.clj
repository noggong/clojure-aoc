(ns aoc2018_3
  (:require [clojure.data :as data]
            [clojure.set]))


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


(defn expose-id
  "문자열에서 아이디를 추출한다.

  Arguments
  - string (string) 대상 문자열

  Return
  - (string) id 문자열
  "
  [string]
  (->> string
       (re-find #"#([0-9^]+)\s")
       last))

(defn expose-as-vector-by-regex
  "정규표현식을 통해 vector 형태의 좌표를 반환한다.

  Arguments
  - string (string) 대상 문자열

  Return
  - ((Int) vector) [int int] vector
  "
  [string, regex]
  (let [exposed-string (re-find regex  string)
        starting-coordinate (subvec exposed-string 2)]
    (->> starting-coordinate
         (map #(Integer/parseInt %))
         vec))

  )

(defn expose-start-coordinate
  "문자열에서 시작좌표를 가져온다

  Arguments
  - string (string) 대상 문자열

  Return
  - ((Int) vector) 시작 좌표 vector
  "
  [string]
  (expose-as-vector-by-regex string #"@\s(([0-9]+),([0-9]+)):"))

(defn expose-size
  "좌표의 사이즈 공간을 추출한다

  Arguments
  - string (string) 대상 문자열

  Return
  - ((Int) vector) 사이즈 [width height] vector
  "
  [string]
  (expose-as-vector-by-regex string #":\s(([0-9]+)x([0-9]+))"))

(defn parse-coordinate-info
  "문자열에서 아이디, 시작좌표, 격자로 파싱하여 리턴

  Arguments
  - string (string) 문자열  ex) #1 @ 1,3: 4x4

  Return
  - (Hash-map) {:id string, :starting-coordinate [x좌표 y좌표], :size [width height]}
  "
  [string]
  {:id (expose-id string)
   :starting-coordinate (expose-start-coordinate string)
   :size (expose-start-coordinate string)
   })

(defn leave-coordinates-by-size
  "시작 부터 끝좌표 까지 거져가는 모든 좌표들 남기기

  Arguments
  - start-x (int) x 시작 좌표
  - end-x (int) x 종료 좌표
  - start-y (int) y 시작 좌표
  - end-x (int) y 종료 좌표

  Return
  [(string) vector] [str 'x좌표' 'x' 'Y좌표']"
  [start-x end-x start-y end-y]
  (loop [n 0
         i start-x
         j start-y
         footprint []]
    (cond (== j end-y) footprint
          (== i end-x) (recur n start-x (inc j) (conj footprint (str i "x" j)))
          :else (recur (inc n) (inc i) j (conj footprint (str i "x" j))))))


(defn footprint-coordinates
  "시작좌표와 크기 좌표를 가지고 거쳐가는 모든 좌표를 가져온다

  Arguments
  - start-coordinate [(int)] 시작좌표
  - size [(int)] 사각형 크기

  Return
  - [(string)] (str 'x좌표' 'x' 'Y좌표')
  "
  [start-coordinate size]
  (leave-coordinates-by-size (first start-coordinate)
                             (dec (+ (first start-coordinate) (first size)))
                             (last start-coordinate)
                             (+ (last start-coordinate) (last size))))

(defn overlapped-count
  "문자열에서 좌표에 아이디를 할당해 아이디가 중첩된 좌표의 갯수를 반환

  Arguments
  - strings ((string)[list])

  Return
  - (int) 중첩된 좌표의 수
  "
  [strings]
  (->> strings
       (map parse-coordinate-info)
       (map #(footprint-coordinates (% :starting-coordinate)
                                     (% :size)))
       (apply concat)
       frequencies
       (filter (fn [[k v]] (> v 1)))
       count))

(comment
  (expose-id "#1 @ 1,3: 4x4")
  (expose-start-coordinate "#1 @ 1,3: 4x4")
  (expose-size "#1 @ 1,3: 4x4")
  (expose-as-vector-by-regex "#1 @ 1,3: 4x4" #":\s(([0-9]+)x([0-9]+))")
  (parse-coordinate-info "#1 @ 1,3: 4x4")
  (leave-coordinates-by-size 1 4 3 7)
  (footprint-coordinates [1 3] [4 4])
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
       not-overlapped
       ))

(comment
  (expose-id "#1 @ 1,3: 4x4")
  (expose-start-coordinate "#1 @ 1,3: 4x4")
  (expose-size "#1 @ 1,3: 4x4")
  (expose-as-vector-by-regex "#1 @ 1,3: 4x4" #":\s(([0-9]+)x([0-9]+))")
  (parse-coordinate-info "#1 @ 1,3: 4x4")
  (leave-coordinates-by-size 1 4 3 7)
  (footprint-coordinates [1 3] [4 4])
  (footprint-coordinates-as-hashmap {:id "1", :starting-coordinate [1 3], :size [4 4]})
  (compare-far-off ["5x5" "6x5" "7x5" "8x5" "5x6" "6x6" "7x6" "8x6" "5x7" "6x7" "7x7" "8x7" "5x8" "6x8" "7x8" "8x8"]
                   `({:id 2 :coordinates ["3x1" "4x1" "5x1" "6x1" "3x2" "4x2" "5x2" "6x2" "3x3" "4x3" "5x3" "6x3" "3x4" "4x4" "5x4" "6x4"]}
                     {:id 3 :coordinates ["1x3" "2x3" "3x3" "4x3" "1x4" "2x4" "3x4" "4x4" "1x5" "2x5" "3x5" "4x5" "1x6" "2x6" "3x6" "4x6"] }))

  (compare-far-off ["1x3" "2x3" "3x3" "4x3" "1x4" "2x4" "3x4" "4x4" "1x5" "2x5" "3x5" "4x5" "1x6" "2x6" "3x6" "4x6"]
                   `({:id 2 :coordinates ["3x1" "4x1" "5x1" "6x1" "3x2" "4x2" "5x2" "6x2" "3x3" "4x3" "5x3" "6x3" "3x4" "4x4" "5x4" "6x4"]}
                     {:id 3 :coordinates ["5x5" "6x5" "7x5" "8x5" "5x6" "6x6" "7x6" "8x6" "5x7" "6x7" "7x7" "8x7" "5x8" "6x8" "7x8" "8x8"] }))

  (rect-far-off `("#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2")))
