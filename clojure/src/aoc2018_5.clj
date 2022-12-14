(ns aoc2018_5
  (:require [clojure.string :as s]))
;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.
(def input (slurp "resources/2018_5.txt"))

(defn pair?
  "두 문자가 대소문자만 다르고 같은 알파벳인지 확인한다."
  [target source]
  (= 32 (abs (- (int target) (int source)))))

(def push-not-pair
  "마지막 글자와 새로운 글자가 짝이면 지우고 짝이 아니면 넣는다."
  (fn [stack char]
    (let [last-char-in-stack (peek stack)]
      (if (and last-char-in-stack
               (pair? last-char-in-stack char))
        (pop stack)
        (conj stack char)))))

(defn length-polymer
  "문자열에 반응한 이후의 글자수를 가져온다."
  [input]
  (->> input
       (reduce push-not-pair [])
       count))

(comment
  (length-polymer input))

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.

(defn char->regex
  "알파벳 패턴을 정규표현식으로 만들어준다."
  [char]
  (re-pattern (str "(?i)" char)))

(def remove-char
  "string 내 특정 char 를 대소문자 상관없이 삭제"
  (fn [char string] (s/replace string (char->regex char) "")))

(defn input->removed-units
  "입력받은 문자열을 정제 / 언급된 알파벳을 소문자로 변형하여 추가함"
  [input]
  (let [mentioned (set (map s/lower-case input))]
    (map remove-char mentioned (repeat input))))

(comment
  (->> input
       input->removed-units
       (map length-polymer)
       (apply min)))
