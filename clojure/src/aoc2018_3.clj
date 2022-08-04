(ns aoc2018_3)


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
   :size (expose-start-coordinate size)
   })


(defn footprints-coordinates
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
       (map #(footprints-coordinates (% :starting-coordinate)
                                     (% :size)))
       (apply concat)
       frequencies
       (filter (fn [[k v]] (> v 1)))
       count
       ))

(comment
  (expose-id "#1 @ 1,3: 4x4")
  (expose-start-coordinate "#1 @ 1,3: 4x4")
  (expose-size "#1 @ 1,3: 4x4")
  (expose-as-vector-by-regex "#1 @ 1,3: 4x4" #":\s(([0-9]+)x([0-9]+))")
  (parse-coordinate-info "#1 @ 1,3: 4x4")
  (leave-coordinates-by-size 1 4 3 7)
  (footprints-coordinates [1 3] [4 4])
  (overlapped-count `("#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"))
  )

;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)
