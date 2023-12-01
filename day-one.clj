
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["1abc2"
                    "pqr3stu8vwx"
                    "a1b2c3d4e5f"
                    "treb7uchet"])

(def real-data-raw (str/split-lines (slurp "day-one.txt")))

;; In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.

(defn only-numbers [line]
  ;; actually only non zero numbers
  (map (fn [chr] (- (int chr) (int \0)))
       (filter (fn [chr] (let [ord (int chr)]
                           (and (> ord 48) (<= ord 57)))) (char-array line))))

(defn recover-calibration-value [line]
  (let [numbers (only-numbers line)
        first-number (first numbers)
        last-number (last numbers)]
    (Integer/parseInt (str first-number last-number))))

(defn part-one [data]
  (reduce + (map recover-calibration-value data)))

(defn maybe-replace [s from to fn]
  (if (and from to)
    (if (nil? (str/index-of s from))
      (fn s (re-pattern (apply str (rest from))) to)
      (fn s (re-pattern from) to))
    s))

(defn replace-first-and-last-text-numbers [line]
  (let [all-replacements (filter (fn [[index f t]] (not (nil? index))) (mapcat conj (map
                                                                                     (fn location-of [[replacement-from replacement-to]]
                                                                                       [[(str/last-index-of line replacement-from) replacement-from replacement-to]
                                                                                        [(str/index-of line replacement-from) replacement-from replacement-to]])
                                                                                     [["one" "1"] ["two" "2"] ["three" "3"] ["four" "4"] ["five" "5"] ["six" "6"] ["seven" "7"] ["eight" "8"] ["nine" "9"]])))
        sorted-replacements (sort (fn replacement-compare [[index f1 t1] [other-index f2 t2]] (< index other-index)) all-replacements)
        ;; just apply the first and last replacements
        first-replacement (or (first sorted-replacements) [[0 "" ""]])
        [i first-replacement-from first-replacement-to] first-replacement
        last-replacement (or (last sorted-replacements) [[0 "" ""]])
        [i last-replacement-from last-replacement-to] last-replacement]
    (-> line
        (maybe-replace first-replacement-from first-replacement-to str/replace-first)
        (maybe-replace last-replacement-from last-replacement-to str/replace))))

(replace-first-and-last-text-numbers "fourfive4tttldbmmkxvhqrmvmrkpxfzbd7")
(replace-first-and-last-text-numbers "two1nine")
(replace-first-and-last-text-numbers "eightwothree")
(replace-first-and-last-text-numbers "abcone2threexyz")
(replace-first-and-last-text-numbers "xtwone3four")
(replace-first-and-last-text-numbers "4nineeightseven2")
(replace-first-and-last-text-numbers "zoneight234")
(replace-first-and-last-text-numbers "7pqrstsixteen")
(replace-first-and-last-text-numbers "dxxzrlzkksfsffp4")
;; my bugs
(replace-first-and-last-text-numbers "eightwooneonetwo")
(replace-first-and-last-text-numbers "eightwooneoneeightwo")
;; really? These map to 83 and 79.
(replace-first-and-last-text-numbers "eighthree")
(replace-first-and-last-text-numbers "sevenine")

(def part-two-test-data-raw ["two1nine"
                             "eightwothree"
                             "abcone2threexyz"
                             "xtwone3four"
                             "4nineeightseven2"
                             "zoneight234"
                             "7pqrstsixteen"])

(defn part-two [data]
  (reduce + (map (fn [line] (recover-calibration-value (replace-first-and-last-text-numbers line))) data)))


