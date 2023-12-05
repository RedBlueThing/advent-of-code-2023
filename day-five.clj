
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["seeds: 79 14 55 13"
                    ""
                    "seed-to-soil map:"
                    "50 98 2"
                    "52 50 48"
                    ""
                    "soil-to-fertilizer map:"
                    "0 15 37"
                    "37 52 2"
                    "39 0 15"
                    ""
                    "fertilizer-to-water map:"
                    "49 53 8"
                    "0 11 42"
                    "42 0 7"
                    "57 7 4"
                    ""
                    "water-to-light map:"
                    "88 18 7"
                    "18 25 70"
                    ""
                    "light-to-temperature map:"
                    "45 77 23"
                    "81 45 19"
                    "68 64 13"
                    ""
                    "temperature-to-humidity map:"
                    "0 69 1"
                    "1 0 69"
                    ""
                    "humidity-to-location map:"
                    "60 56 37"
                    "56 93 4"])

(def real-data-raw (str/split-lines (slurp "day-five.txt")))

(defn check-mapping [mapping value]
  (let [[from-start to-start length] mapping]
    (if (and (>= value from-start) (< value (+ from-start length)))
      ;; it's a match
      (+ to-start (- value from-start))
      ;; no match, so nil
      nil)))

(defn map-or-return [mappings value]
  ;; ok wait. If we use regular dictionaries, ie, this code
  ;; (or (dictionary value) value)
  ;; things get out of hand with the very big ranges in the real data.
  ;; So instead let's just lookat the from and to info
  (or (loop [i 0 maybe-found-mapping nil]
     (if (or (= i (count mappings)) maybe-found-mapping)
      ;; we found a mapping or ran out of mappings, so return the thing we maybe
      ;; found.
       maybe-found-mapping
      ;; otherwise, keep looking
       (recur (inc i) (check-mapping (nth mappings i) value)))) value))

(defn map-location-to-seed-number [almanac location]
  (let [{soil-to-seed :soil-to-seed
         fertilizer-to-soil :fertilizer-to-soil
         water-to-fertilizer :water-to-fertilizer
         light-to-water :light-to-water
         temperature-to-light :temperature-to-light
         humidity-to-temperature :humidity-to-temperature
         location-to-humidity :location-to-humidity} almanac]
    (->> location
         (map-or-return location-to-humidity)
         (map-or-return humidity-to-temperature)
         (map-or-return temperature-to-light)
         (map-or-return light-to-water)
         (map-or-return water-to-fertilizer)
         (map-or-return fertilizer-to-soil)
         (map-or-return soil-to-seed))))

(defn parse-numbers-str [numbers-str]
  (map
   (fn [value-str]
     (Long/parseLong value-str))
   (str/split numbers-str #" ")))


(defn reverse-mapping-keyword [s]
  (let [[from to] (str/split s #"-to-")]
    (str to "-to-" from)))

(defn parse-mapping-data [mapping-data-entry]
  (let [dictionary-key-str (first mapping-data-entry)
        trimmed-dictionary-key (keyword (reverse-mapping-keyword (subs dictionary-key-str 0 (- (count dictionary-key-str) 5))))
        mappings (map parse-numbers-str (rest mapping-data-entry))]
    [trimmed-dictionary-key mappings]))

(defn parse-seed-data [seed-data]
  (let [seed-numbers-str (second (str/split seed-data #": "))]
    (parse-numbers-str seed-numbers-str)))

(defn create-almanac [parsed-mapping-data]
  (into {} parsed-mapping-data))

(defn parse-data [data]
  (let [partitioned-data (filter (fn [segment] (not= (first segment) "")) (partition-by #(= "" %) data))
        seed-data (parse-seed-data (first (first partitioned-data)))
        mapping-data (rest partitioned-data)
        parsed-mapping-data (map parse-mapping-data mapping-data)
        almanac (create-almanac parsed-mapping-data)]
    [seed-data almanac]))

(defn min-valid-location [data check-seed-fn]
  (let [[seeds almanac] (parse-data data)]
    (loop [current-location 1
           min-valid-location nil]
      (if (or (= current-location 99999999) min-valid-location)
        min-valid-location
        (recur (inc current-location) (check-seed-fn seeds current-location (map-location-to-seed-number almanac current-location)))))))

(defn part-one-check-seed-fn [seeds location seed-number-to-check]
  ;; is the location one of the seeds (just a list of seed numbers)
  (if (some #(= % seed-number-to-check) seeds)
    location
    nil))
