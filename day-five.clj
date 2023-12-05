
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

(defn map-or-return [dictionary value]
  ;; either map the value if it's in the dictionary, or return it
  (or (dictionary value) value))

(defn map-seed-number-to-location [almanac seed-number]
  ;; an almanac is a series of dictionaries that map values
  (let [{seed-to-soil :seed-to-soil
         soil-to-fertilizer :soil-to-fertilizer
         fertilizer-to-water :fertilizer-to-water
         water-to-light :water-to-light
         light-to-temperature :light-to-temperature
         temperature-to-humidity :temperature-to-humidity
         humidity-to-location :humidity-to-location} almanac]
    (->> seed-number
         (map-or-return seed-to-soil)
         (map-or-return soil-to-fertilizer)
         (map-or-return fertilizer-to-water)
         (map-or-return water-to-light)
         (map-or-return light-to-temperature)
         (map-or-return temperature-to-humidity)
         (map-or-return humidity-to-location))))

(assert (= (map-seed-number-to-location {:seed-to-soil {1 2}
                                  :soil-to-fertilizer {2 3}
                                  :fertilizer-to-water {}
                                  :water-to-light {}
                                  :light-to-temperature {}
                                  :temperature-to-humidity {}
                                  :humidity-to-location {}} 1) 3))

(defn parse-numbers-str [numbers-str]
  (map
   (fn [value-str]
     (Long/parseLong value-str))
   (str/split numbers-str #" ")))

(defn parse-mapping-data [mapping-data-entry]
  (let [dictionary-key-str (first mapping-data-entry)
        trimmed-dictionary-key (keyword (subs dictionary-key-str 0 (- (count dictionary-key-str) 5)))
        mappings (map parse-numbers-str (rest mapping-data-entry))]
    [trimmed-dictionary-key mappings]))

(defn parse-seed-data [seed-data]
  (let [seed-numbers-str (second (str/split seed-data #": "))]
    (parse-numbers-str seed-numbers-str)))

(defn range-of-values-for-mapping [[to-start from-start length]]
  (map (fn [index] [(+ from-start index) (+ to-start index)]) (range 0 length)))

(defn create-almanac [parsed-mapping-data]
  (let [into-dictionaries-vector (map (fn [[keyword mappings]] [keyword (into {} (mapcat range-of-values-for-mapping mappings))]) parsed-mapping-data)]
    (into {} into-dictionaries-vector)))

(defn parse-data [data]
  (let [partitioned-data (filter (fn [segment] (not= (first segment) "")) (partition-by #(= "" %) data))
        seed-data (parse-seed-data (first (first partitioned-data)))
        mapping-data (rest partitioned-data)
        parsed-mapping-data (map parse-mapping-data mapping-data)
        almanac (create-almanac parsed-mapping-data)]
    [seed-data almanac]))

(defn part-one [data]
  (let [[seeds almanac] (parse-data data)
        mapped-seeds (map (fn [seed-number] (map-seed-number-to-location almanac seed-number)) seeds)]
    (apply min mapped-seeds)))

