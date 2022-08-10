(ns aoc2018-1)
;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력

(defn split-string [numbers]
  (re-seq #"[^\s]+" numbers))

(defn string->int [numberic]
  (Integer/parseInt numberic))

(defn sum [numbers]
  (->> numbers
       split-string
       (map string->int)
       #_(reduce +)))

(comment
  (sum "+10 -2 -5 +1"))

;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...
;clojure.java.io
;slurp

(defn get-duplicated-item [history number]
  (if (history number)
    (reduced number)
    (conj history number)))

(defn find-freq [numbers]
  (->> numbers
       split-string
       (map string->int)
       cycle
       (reductions +)
       (reduce get-duplicated-item #{})))


(comment
  (find-freq "+3 +3 +4 -2 -4"))
