
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["Time:      7  15   30"
                    "Distance:  9  40  200"])

(def real-data-raw (str/split-lines (slurp "day-six.txt")))

;; Your toy boat has a starting speed of zero millimeters per millisecond. For
;; each whole millisecond you spend at the beginning of the race holding down
;; the button, the boat's speed increases by one millimeter per millisecond.

(defn extract-numbers [str]
  (map #(Integer/parseInt %) (rest (filter #(not= "" %) (str/split str #" ")))))

(defn parse-previous-race-results-part-one [data]
  (let [times-str (first data)
        distances-str (second data)
        times (extract-numbers times-str)
        distances (extract-numbers distances-str)]
    (map-indexed (fn [i time] [ time (nth distances i)]) times)))

(defn distance-for-hold-time [hold-time total-race-time]
  (let [speed hold-time]
    (* speed (- total-race-time hold-time))))

(defn winning-hold-times-for-race [[total-race-time distance-to-beat]]
  (let [hold-times-to-test (range 1 total-race-time)]
    (filter #(< distance-to-beat %) (map (fn [hold-time] (distance-for-hold-time hold-time total-race-time)) hold-times-to-test))))

(defn part-one [data]
  (let [race-results (parse-previous-race-results-part-one data)]
    (apply * (map (fn [race] (count (winning-hold-times-for-race race))) race-results))))

(defn parse-previous-race-results-part-two [data]
  (let [time-str (first data)
        distance-str (second data)
        time (Long/parseLong (str/replace (subs time-str 5) #"\s+" ""))
        distance (Long/parseLong (str/replace (subs distance-str 9) #"\s+" ""))]
    [[time distance]]))

(defn part-two [data]
  (let [race-results (parse-previous-race-results-part-two data)]
    (apply * (map (fn [race] (count (winning-hold-times-for-race race))) race-results))))
