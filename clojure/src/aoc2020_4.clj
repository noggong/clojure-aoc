(ns aoc2020_4
  (:require [clojure.spec.alpha :as s]
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


(def required `(
                 :passport/byr
                 :passport/iyr
                 :passport/eyr
                 :passport/hgt
                 :passport/hcl
                 :passport/ecl
                 :passport/pid))

(s/def :passport/required? #(clojure.set/subset? required %))

;- byr (Birth Year) - 4 자리 숫자; 최소 1920 & 최대 2002.
(s/def :passport/byr (s/int-in 1920 2002))
;- iyr (Issue Year) - 4 자리 숫자; 최소 2010 & 최대 2020.
(s/def :passport/iyr (s/int-in 2010 2021))
;- eyr (Expiration Year) - 4 자리 숫자; 최소 2020 & 최대 2030.
(s/def :passport/eyr (s/int-in 2020 2031))
(s/def :passport/hgt #(re-matches #"^[0-9]{2,3}(in|cm)$" %))
;(s/valid? :passport/hgt "12in")
;- cm의 경우, 숫자는 최소 150 & 최대 193.
;(s/def :passport/cm의)
;- in의 경우, 숫자는 최소 59 & 최대 76.
;(s/def :passport/in의)
(s/def :passport/hcl #(re-matches #"^#[0-9a-f]{6}$" %))
(s/def :passport/ecl #(#{:amb :blu :brn :gry :grn :hzl :oth} (keyword %)))
(s/def :passport/pid #(re-matches #"^0[0-9]{8}$" %))

(s/def :passport/valid?
  (s/keys :req [:passport/byr
                :passport/iyr
                :passport/eyr
                :passport/hgt
                :passport/hcl
                :passport/ecl
                :passport/pid]))

(def cast-map (fn [[- key val]] {(keyword (str "passport" "/" key)) val}))

(defn cast-year-int
  "년도 데이터들 int 형으로 변경"
  [passport]
  (merge passport {
             :passport/byr (Integer/parseInt (passport :passport/byr))
             :passport/iyr (Integer/parseInt (passport :passport/iyr))
             :passport/eyr (Integer/parseInt (passport :passport/eyr))}))

(defn input->passport
  "input string 을 여권 hash-map 으로 변경한다."
  [input]
  (let [extracted (->> input
                     (re-seq #"([^\s^:]+):([^\s^]+)")
                     (map cast-map))]
    (->> extracted
         (apply merge)
         cast-year-int)))

(defn passport?
  "여권정보가 맞는지 확인한다."
  [input]
  (->> input
       input->passport
       (s/valid? :passport/required?)
       ))

(keyword (str "a/" "b"))
(defn passport-valid?
  "여권정보가 맞는지 확인한다."
  [input]
  (->> input
       input->passport
       (s/valid? :passport/valid?)
       ))

(comment
  (passport? "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm")
  (passport? "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929")
  (passport? "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm")
  (passport? "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in"))

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
  (passport-valid? "ecl:gry pid:060033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm")
  (passport-valid? "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929")
  (passport-valid? "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm")
  (passport-valid? "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in"))