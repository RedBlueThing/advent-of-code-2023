
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

(defn check-mapping [mapping value reverse-map]
  (let [[to-start from-start length] (if reverse-map (reverse mapping) mapping)]
    (if (and (>= value from-start) (< value (+ from-start length)))
      ;; it's a match
      (+ to-start (- value from-start))
      ;; no match, so nil
      nil)))

(defn map-or-return [mappings value reverse-map]
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
          (recur (inc i) (check-mapping (nth mappings i) value reverse-map)))) value))

(defn reverse-map-or-return [mappings value]
  (map-or-return mappings value true))

(defn forward-map-or-return [mappings value]
  (map-or-return mappings value false))

(defn map-seed-number-location [almanac seed-number-or-location reverse-map]
  ;; an almanac is a series of dictionaries that map values
  (let [{seed-to-soil :seed-to-soil
         soil-to-fertilizer :soil-to-fertilizer
         fertilizer-to-water :fertilizer-to-water
         water-to-light :water-to-light
         light-to-temperature :light-to-temperature
         temperature-to-humidity :temperature-to-humidity
         humidity-to-location :humidity-to-location} almanac
        forward-mappings [seed-to-soil
                          soil-to-fertilizer
                          fertilizer-to-water
                          water-to-light
                          light-to-temperature
                          temperature-to-humidity
                          humidity-to-location]
        reverse-mappings (reverse forward-mappings)
        mappings (if reverse-map reverse-mappings forward-mappings)]
    (println mappings)
    (reduce (fn [number mapping]
              (map-or-return mapping number reverse-map)) seed-number-or-location mappings)))

(defn map-location-to-seed-number [almanac location]
  (let [{soil-to-seed :soil-to-seed
         fertilizer-to-soil :fertilizer-to-soil
         water-to-fertilizer :water-to-fertilizer
         light-to-water :light-to-water
         temperature-to-light :temperature-to-light
         humidity-to-temperature :humidity-to-temperature
         location-to-humidity :location-to-humidity} almanac]
    (->> location
         (reverse-map-or-return location-to-humidity)
         (reverse-map-or-return humidity-to-temperature)
         (reverse-map-or-return temperature-to-light)
         (reverse-map-or-return light-to-water)
         (reverse-map-or-return water-to-fertilizer)
         (reverse-map-or-return fertilizer-to-soil)
         (reverse-map-or-return soil-to-seed))))

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
         (forward-map-or-return seed-to-soil)
         (forward-map-or-return soil-to-fertilizer)
         (forward-map-or-return fertilizer-to-water)
         (forward-map-or-return water-to-light)
         (forward-map-or-return light-to-temperature)
         (forward-map-or-return temperature-to-humidity)
         (forward-map-or-return humidity-to-location))))

(let [[seeds almanac] (parse-data real-data-raw)]
  (map-seed-number-location almanac 1 false))

(let [[seeds almanac] (parse-data real-data-raw)]
  (map-seed-number-to-location almanac 1))

(let [[seeds almanac] (parse-data real-data-raw)]
  (map-location-to-seed-number almanac 1))

(let [[seeds almanac] (parse-data real-data-raw)]
  (map-seed-number-location almanac 1 true))


(def map-seed-number-location-memo (memoize map-seed-number-location))

(assert (= (map-seed-number-location {:seed-to-soil [[3 1 1]]
                                         :soil-to-fertilizer []
                                         :fertilizer-to-water []
                                         :water-to-light []
                                         :light-to-temperature []
                                         :temperature-to-humidity []
                                         :humidity-to-location []} 1 false) 3))

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
  ;; mistakes were made
  (map (fn [index] [(+ from-start index) (+ to-start index)]) (range 0 length)))

(defn create-almanac [parsed-mapping-data]
  (into {} parsed-mapping-data))

(defn parse-data [data]
  (let [partitioned-data (filter (fn [segment] (not= (first segment) "")) (partition-by #(= "" %) data))
        seed-data (parse-seed-data (first (first partitioned-data)))
        mapping-data (rest partitioned-data)
        parsed-mapping-data (map parse-mapping-data mapping-data)
        almanac (create-almanac parsed-mapping-data)]
    [seed-data almanac]))

(defn part-one [data]
  (let [[seeds almanac] (parse-data data)
        mapped-seeds (map (fn [seed-number] (map-seed-number-location-memo almanac seed-number false)) seeds)]
    (apply min mapped-seeds)))

(defn part-two-test [data]
  (let [[seeds almanac] (parse-data data)
        partitioned-seeds (partition 2 seeds)]
    (reduce + (map second partitioned-seeds))))

(defn stepped-range [start count step]
  (->> (iterate #(+ % (max step 1)) start)
       (take (/ count step))))

(defn map-seed-numbers-to-min-location [almanac [start count]]
  ;; if the count is big we used a stepped range to check (ie big gaps in the
  ;; data) to find a lowest location. Once we have that we are going to scan
  ;; downwards to see if there are any lower viable locations (which there
  ;; probably will be)
  (let [range-to-check (if (> count 10000)
                         (stepped-range 0 count (int (/ count 100000)))
                         (range 0 count))]
    (apply min (map (fn [index]
                      (map-seed-number-location-memo almanac (+ start index) false))
                    range-to-check))))

(defn part-two-check-seed-fn [seeds location seed-number-to-check]
  ;; is the location one of the seeds (just a list of seed numbers)
  (let [partitioned-seeds (partition 2 seeds)]
    (if (some (fn [[start count]]
                (println seed-number-to-check)
                (println start count)
                (and (>= seed-number-to-check start) (< seed-number-to-check (+ start count)))) partitioned-seeds)
      location
      nil)))

(defn scan-for-lower-locations [seeds almanac starting-location]
  (loop [current-location starting-location
         locations []]
      ;; check a few thousand lower locations to see if we can find a lower
      ;; location (based on our sampled lowest location)
    (if (< current-location (- starting-location 100))
      locations
      (recur (dec current-location) (let [maybe-location (part-two-check-seed-fn
                                                          seeds
                                                          current-location
                                                          (map-seed-number-location-memo almanac current-location true))]
                                      (if maybe-location (conj locations maybe-location) locations))))))

(defn part-two [data]
  (let [[seeds almanac] (parse-data data)
        partitioned-seeds (partition 2 seeds)
        futures (map (fn [partitioned-seed]
                       (future (map-seed-numbers-to-min-location almanac partitioned-seed)))
                     partitioned-seeds)
        maybe-lowest-location (apply min (map deref futures))]
    maybe-lowest-location))


