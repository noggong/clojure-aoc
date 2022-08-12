(ns aoc2020_4
  (:require [clojure.spec.alpha :as spec]
            [clojure.string :as s]
            [clojure.set]))

;## 파트 1
;여권이 유효한지 판단하려고 한다. 여권에는 다음과 같은 필드가 있음.
;- byr (Birth Year)
;- iyr (Issue Year)
;- eyr (Expiration Year)
;- hgt (Height)
;- hcl (Hair Color)
;- ecl (Eye Color)
;- pid (Passport ID)
;- cid (Country ID)
;
;파트 1에서는 여권의 모든 필드가 존재하는지의 여부를 검사한다. 주어진 입력에서 '유효한' 여권의 숫자를 반환하여라.
;-첫번째는 유효한 여권이다. 8개의 필드가 전부 존재한다.
;- 두번째는 유효하지 않다. hgt가 없기 때문.
;- 세번째는 cid가 없지만, ** cid는 없어도 되는 ** 것으로 간주한다. 그래서 유효하다.
;- 네번째는 cid와 byr이 없다. byr은 반드시 있어야하는 필드이므로 유효하지 않다.


(def inputs (->> (s/split (slurp "resources/2020_4.txt") #"\n\n")
                 (map #(s/replace % #"\n" " "))))


(def required #{:passport/byr
                :passport/iyr
                :passport/eyr
                :passport/hgt
                :passport/hcl
                :passport/ecl
                :passport/pid})

(spec/def :passport/required? #(clojure.set/subset? required %))
(spec/def :passport/byr (spec/int-in 1920 2003))
(spec/def :passport/iyr (spec/int-in 2010 2021))
(spec/def :passport/eyr (spec/int-in 2020 2031))
(spec/def :passport/hcl #(re-matches #"^#[0-9a-f]{6}$" %))
(spec/def :passport/ecl #(#{:amb :blu :brn :gry :grn :hzl :oth} (keyword %)))
(spec/def :passport/pid #(re-matches #"^0[0-9]{8}$" %))
(spec/def :passport/hgt-cm (spec/int-in 150 194))
(spec/def :passport/hgt-in (spec/int-in 59 77))
(spec/def :passport/eyr (spec/int-in 2020 2031))
(clojure.set/subset? required #:passport{:hcl "#efcc98", :iyr 2011, :ecl "hzl", :eyr 2020, :hgt "174cm", :pid "589700330", :byr :invalid, :hgt-cm 174})


(spec/def :passport/valid?
  (spec/keys :req [:passport/byr
                   :passport/iyr
                   :passport/eyr
                   :passport/hgt
                   :passport/hcl
                   :passport/ecl
                   :passport/pid]
          :opt [:passport/hgt-cm
                :passport/hgt-in]))

(def cast-map (fn [[- key val]] {(keyword "passport" key) val}))

(defn string->int
  "문자를 숫자로 변환, 변환 할수 없다면 :invalid 로 반환"
  [string]
  (try (Integer/parseInt string)
       (catch Exception -
         :invalid)))

(defn cast-year-int
  "년도 데이터들 int 형으로 변경"
  [passport]
  (reduce (fn [acc keyword]
            (if (keyword passport)
              (update acc keyword string->int)
              acc))
          passport [:passport/byr :passport/eyr :passport/iyr]))

; try catch 를 최대한 작게 만들기 위해 type cast 하는 부분만 함수로 분리하는것을 추천.
; parseint 결과 유효하지 않으면 -1 / 0 / nil / :invalid 등으로 대체 해서 넣을수 있다.
; hashmap 에 nil 을 넣는것은 지양한다.

(defn hgt->hgt-by-type
  "키를 in / cm 으로 입력했는지 확인해서 데이터 추가"
  [hgt]
  (let [[_ number type] (re-matches #"([0-9^\w]+)(in|cm)" hgt)]
    (if (nil? type)
      {:passport/hgt :invalid}
      {(keyword "passport" (str "hgt-" type)) (string->int number)})))

(defn extract-passport
  "input 정보에서 passport 정보 추출"
  [input]
  (->> input
       (re-seq #"([^\s^:]+):([^\s^]+)")
       (map cast-map)
       (apply merge)
       cast-year-int))

(defn input->passport
  "input string 을 여권 hash-map 으로 변경한다."
  [input]
  (let [extracted (extract-passport input)
        hgt (extracted :passport/hgt)]
    (if (not= hgt nil)
      (merge extracted (hgt->hgt-by-type hgt))
      extracted)))
(extract-passport "hcl:#efcc98 iyr:2011 ecl:hzl eyr:2020 hgt:174cm pid:589700330")

(defn passport?
  "여권정보가 맞는지 확인한다."
  [input]
  (->> input
       input->passport
       (spec/valid? :passport/required?)))



(defn passport-valid?
  "여권정보가 맞는지 확인한다."
  [input]
  (->> input
       input->passport
       (spec/valid? :passport/valid?)))

(passport-valid? "ecl:brn iyr:2010 eyr:2027 pid:379769214 cid:111 byr:1960 hcl:#cfa07d hgt:169cm")

(comment

  (map #(vec [% (passport? %)]) inputs)
  (->> (filter #(= (passport? %) true) inputs)
       count)

  (map #(vec [% (passport-valid? %)]) inputs)
  (->> (filter #(= (passport-valid? %) true) inputs)))



;## 파트 2
;파트1에서는 필드의 유무만을 검사했다면, 파트2에서는 구체적인 범위가 주어진다.
;- byr (Birth Year) - 4 자리 숫자; 최소 1920 & 최대 2002.
;- iyr (Issue Year) - 4 자리 숫자; 최소 2010 & 최대 2020.
;- eyr (Expiration Year) - 4 자리 숫자; 최소 2020 & 최대 2030.
;- hgt (Height) - 마지막에 cm 혹은 in이 오는 숫자:
;- cm의 경우, 숫자는 최소 150 & 최대 193.
;- in의 경우, 숫자는 최소 59 & 최대 76.
;- hcl (Hair Color) - #뒤에 오는 정확히 6개의 캐릭터 0-9 혹은 a-f.
;- ecl (Eye Color) - 정확히 amb blu brn gry grn hzl oth 중 하나.
;- pid (Passport ID) - 처음 0을 포함하는 9자리 숫자.
;- cid (Country ID) - 없어도 됨.

(comment
  (map #(vec [% (passport-valid? %)]) inputs))
