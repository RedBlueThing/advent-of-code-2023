
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["467..114.."
"...*......"
"..35..633."
"......#..."
"617*......"
".....+.58."
"..592....."
"......755."
"...$.*...."
".664.598.."])

(def real-data-raw (str/split-lines (slurp "day-three.txt")))

(defn map-for-data [data]
  (let [height (count data)
        width (count (first data))
        buffer (apply vector (apply concat (map (fn [line] (char-array line)) data)))]
    {:width width :height height :buffer buffer}))

(defn index-for-row-and-column [schematic-map row column]
  (let [{width :width} schematic-map]
    (+ (* row width) column)))

(defn is-number? [chr]
  (let [ord (int chr)] (and (>= ord (int \0)) (<= ord (int \9)))))

(defn is-gear? [chr]
  (= chr \*))

(defn is-symbol? [chr]
  ;; if it's not a number or a period, then it's a symbol
  (and (not= chr \.) (not (is-number? chr))))

(defn index-out-of-range? [schematic-map index]
  (let [{buffer :buffer} schematic-map]
    (or (< index 0) (>= index (count buffer)))))

(defn row-column-out-of-range? [schematic-map row-index column-index]
  (let [{buffer :buffer width :width height :height} schematic-map]
    (or (< row-index 0) (>= row-index height)
        (< column-index 0) (>= column-index width))))

(defn neighbour-indices [schematic-map row-index column-index]
  ;; just return the valid neighbour index
  (map (fn [[row-index column-index]] (index-for-row-and-column schematic-map row-index column-index))
       (filter (fn [[row-index column-index]]
                 (not (row-column-out-of-range? schematic-map row-index column-index)))
               [[(dec row-index) (dec column-index)]
                [(dec row-index) column-index]
                [(dec row-index) (inc column-index)]
                [row-index (dec column-index)]
                [row-index (inc column-index)]
                [(inc row-index) (dec column-index)]
                [(inc row-index) column-index]
                [(inc row-index) (inc column-index)]])))

(defn numbers-info-row [data row-index]
  ;; get indices and number values for numbers in a row
  (let [schematic-map (map-for-data data)
        indexed-number-data (filter (fn [[column-index map-index value]]
                                      (is-number? value))
                                    (map-indexed
                                     (fn [i chr] [i (index-for-row-and-column schematic-map row-index i) chr])
                                     (char-array (nth data row-index))))
        grouped-number-data (reduce (fn [groups next-number]
                                      (let [previous-group (last groups)
                                            previous-number (last previous-group)]
                                        (if previous-number
                                          ;; decide if this number is part of the previous group
                                          (let [[previous-column-index previous-map-index previous-value] previous-number
                                                [column-index map-index value] next-number]
                                            (if (= previous-column-index (dec column-index))
                                              (conj (apply vector (butlast groups)) (conj previous-group next-number))
                                              (conj groups [next-number])))
                                          (conj groups [next-number]))))
                                    [] indexed-number-data)
        numbers-and-neighbours (map (fn [grouped-number]
                                      (let [number (Integer/parseInt (apply str (map #(nth % 2) grouped-number)))
                                            map-indices-for-numbers (set (map (fn [[column-index map-index value]]
                                                                                map-index) grouped-number))
                                            neighbour-indices-with-overlaps (set (flatten (map (fn [[column-index map-index value]]
                                                                                                 (neighbour-indices schematic-map row-index column-index))
                                                                                               grouped-number)))
                                            neighbour-index-set (set/difference neighbour-indices-with-overlaps map-indices-for-numbers)]
                                        [number neighbour-index-set])) grouped-number-data)]
    numbers-and-neighbours))

(defn neighbours-contain-symbol? [schematic-map neighbour-indices]
  (let [{buffer :buffer} schematic-map]
    (not (empty? (filter (fn [neighbour-index]
                           (is-symbol? (nth buffer neighbour-index)))
                         neighbour-indices)))))

(defn part-number-sum [data]
  (let [schematic-map (map-for-data data)
        numbers-info (mapcat (fn [row-index]
                               (numbers-info-row data row-index)) (range 0 (count data)))
        numbers-with-neighbours-containing-symbols (filter (fn [[number neighbours]]
                                                             (neighbours-contain-symbol? schematic-map neighbours)) numbers-info)]
    (reduce + (map first numbers-with-neighbours-containing-symbols))))

(defn maybe-gear-info-row [data row-index]
  ;; maybe gears, because we can only confirm it's a gear when we look at the
  ;; number info
  (let [schematic-map (map-for-data data)
        indexed-gear-data (filter (fn [[column-index map-index value]]
                                    (is-gear? value))
                                  (map-indexed
                                   (fn [i chr] [i (index-for-row-and-column schematic-map row-index i) chr])
                                   (char-array (nth data row-index))))
        gears-with-location (map (fn [[column-index map-index value]]
                                    (set [map-index])) indexed-gear-data)]
    gears-with-location))

(defn gear-ratio-sum [data]
  (let [schematic-map (map-for-data data)
        numbers-info (mapcat (fn [row-index]
                               (numbers-info-row data row-index)) (range 0 (count data)))
        maybe-gear-info (mapcat (fn [row-index]
                                  (maybe-gear-info-row data row-index)) (range 0 (count data)))]
    ;; so for each of my gear info sets I want to check how many number info
    ;; sets I overlap with. If a maybe-gear set overlaps with exactly two number
    ;; info sets, then we have a gear. Then we just need to grab the multiple of
    ;; the two part numbers we found. We can do that with a reducer.
    ;;
    ;; Once we have the gear ratios, we just add them up (with another reducer)
    (reduce + (reduce (fn [found-gear-ratios next-gear-set]
                        (let [neighbour-part-numbers (map first
                                                          (filter (fn [[number neighbours]]
                                                                    (boolean (not-empty (set/intersection neighbours next-gear-set)))) numbers-info))]
                          (if (= (count neighbour-part-numbers) 2)
                            ;; we found a gear, add it's gear ratio
                            (conj found-gear-ratios (apply * neighbour-part-numbers))
                            ;; we didn't find a near gear, so just continue with the ones we have already found
                            found-gear-ratios))) [] maybe-gear-info))))
