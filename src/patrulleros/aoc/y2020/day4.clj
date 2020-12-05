(ns patrulleros.aoc.y2020.day4
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [malli.core :as malli]
            [malli.transform]
            [patrulleros.aoc.util :as util]))

(def puzzle-input
  (delay (util/read-lines "2020/day4.txt")))

(def example
  ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
   "byr:1937 iyr:2017 cid:147 hgt:183cm"
   ""
   "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
   "hcl:#cfa07d byr:1929"
   ""
   "hcl:#ae17e1 iyr:2013"
   "eyr:2024"
   "ecl:brn pid:760753108 byr:1931"
   "hgt:179cm"
   ""
   "hcl:#cfa07d eyr:2025 pid:166559648"
   "iyr:2011 ecl:brn hgt:59in"])

(def example-invalid
  ["eyr:1972 cid:100"
   "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
   ""
   "iyr:2019"
   "hcl:#602927 eyr:1967 hgt:170cm"
   "ecl:grn pid:012533040 byr:1946"
   ""
   "hcl:dab227 iyr:2012"
   "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
   ""
   "hgt:59cm ecl:zzz"
   "eyr:2038 hcl:74454a iyr:2023"
   "pid:3556412378 byr:2007"])

(defn create-passport [data]
  (->> data
       (mapcat #(str/split % #" "))
       (map #(str/split % #":"))
       (into {})))

(defn valid-passport? [passport]
  (set/subset? #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"}
               (set (keys passport))))

(defn parse-passports [lines]
  (->> lines
       (partition-by #(= "" %))
       (filter #(not= [""] %))
       (mapv create-passport)))

(defn solve-p1
  ([] (solve-p1 @puzzle-input))
  ([lines]
   (let [passports (parse-passports lines)]
     (count (filter valid-passport? passports)))))

(defn valid-hgt? [height]
  (let [value (Integer/parseInt (subs height 0 (- (count height) 2)))]
    (cond
      (str/ends-with? height "cm")
      (<= 150 value 193)

      (str/ends-with? height "in")
      (<= 59 value 76)

      :else
      false)))

(defn valid-hcl? [hair-color]
  (boolean (re-matches #"#[0-9a-f]{6}" hair-color)))

(defn valid-pid? [pid]
  (and (= 9 (count pid))
       (every? #(<= (int \0) (int %) (int \9)) pid)))

(def passport-schema
  [:map
   ["byr" [:int {:min 1920 :max 2002}]]
   ["iyr" [:int {:min 2010 :max 2020}]]
   ["eyr" [:int {:min 2020 :max 2030}]]
   ["hgt" [:and :string [:fn
                         {:error/message "should be between 150 and 195 cm or 59 and 76 in"}
                         valid-hgt?]]]
   ["hcl" [:and :string [:fn
                         {:error/message "should be a 6 digits hexadecimal string"}
                         valid-hcl?]]]
   ["ecl" [:enum "amb" "blu" "brn" "gry" "grn" "hzl" "oth"]]
   ["pid" [:and :string [:fn
                         {:error/message "should be a 9 digits number"}
                         valid-pid?]]]
   ["cid" {:optional true} :string]])

(defn solve-p2
  ([] (solve-p2 @puzzle-input))
  ([lines]
   (let [passports (parse-passports lines)]
     (reduce (fn [cnt passport]
               (let [valid (as-> passport p
                             (malli/decode passport-schema p malli.transform/string-transformer)
                             (malli/validate passport-schema p))]
                 (if valid (inc cnt) cnt)))
             0
             passports))))

(comment
  (assert (= 254 (solve-p1)) "Part 1 solution is wrong.")
  (assert (= 184 (solve-p2)) "Part 2 solution is wrong."))
