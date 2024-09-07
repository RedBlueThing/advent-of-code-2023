
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def real-data-raw (str/split-lines (slurp "day-sixteen.txt")))

;; let's put the test data in a file this time so we don't have problems with
;; escaping \
(def test-data-raw (str/split-lines (slurp "day-sixteen-test.txt")))

(defn new-location-in-direction [row column direction]
  (case direction
    :up [(dec row) column direction]
    :right [row (inc column) direction]
    :down [(inc row) column direction]
    :left [row (dec column) direction]))

(defn in? [coll elm] (some #(= elm %) coll))

(defn apply-energy-reducer [energized [row column direction]]
    (assoc-in energized [row column] (conj (get-in energized [row column]) direction)))

(defn filter-for-revisit [contraption energized active-beams]
  (filter (fn [[row column direction]]
            (let [direction-set (get-in energized [row column])
                  symbol-at-location (get-in contraption [row column])]
              ;; keep the beam if the direction set doesn't already contain this
              ;; direction (the :else case)
              ;;
              ;; OR
              ;;
              ;; The direction of the beam has already been handled by a
              ;; splitter location. Or, depending on what type of splitter it
              ;; is, does the set of directions on that location intersect with
              ;; the set of either directions that resolve to the same split.
              (cond (and (= symbol-at-location \|) (in? [:left :right] direction)) (empty? (set/intersection direction-set #{:left :right}))
                    (and (= symbol-at-location \-) (in? [:up :down] direction)) (empty? (set/intersection direction-set #{:down :up}))
                    :else (nil? (direction-set direction)))))

          active-beams))

(defn move-beam [contraption [row column direction]]
  ;; Returns a vector of beams (the original beam + any newly created beams)
  (let [symbol-at-location (get-in contraption [row column])]
    (case symbol-at-location
      \. [(new-location-in-direction row column direction)]
      \| (cond
           ;; if we are going up or down, we just continue in that direction
           (or (= direction :up) (= direction :down)) [(new-location-in-direction row column direction)]
           ;; if we are going left or right, we split and go up and down.
           (or (= direction :right) (= direction :left)) [(new-location-in-direction row column :up)
                                                          (new-location-in-direction row column :down)])
      \- (cond
           ;; if we are going left or right, we just continue in that direction
           (or (= direction :right) (= direction :left)) [(new-location-in-direction row column direction)]
           ;; if we are going up or down, we split and go left and right.
           (or (= direction :up) (= direction :down)) [(new-location-in-direction row column :left)
                                                       (new-location-in-direction row column :right)])
      \\ (case direction
           :left [(new-location-in-direction row column :up)]
           :right [(new-location-in-direction row column :down)]
           :up [(new-location-in-direction row column :left)]
           :down [(new-location-in-direction row column :right)])
      \/ (case direction
           :left [(new-location-in-direction row column :down)]
           :right [(new-location-in-direction row column :up)]
           :up [(new-location-in-direction row column :right)]
           :down [(new-location-in-direction row column :left)]))))

(defn filter-out-of-range-beams [contraption active-beams]
  (let [rows (count contraption)
        columns (count (first contraption))]
    (filter (fn [[row column _]] (and (>= row 0)
                                      (< row rows)
                                      (>= column 0)
                                      (< column columns))) active-beams)))

(def debug false)
(defn dump [data]
  (println (vec (range 0 (count (first data)))))
  (doseq [[i v] (map-indexed (fn [i v] [i v]) data)]
    (println (map count v) i)))

(defn simulate-round-impl [[contraption energized active-beams]]
  (let [
        ;; check if a beam has visited its current location in a compatible direction before
        unique-active-beams (filter-for-revisit contraption energized active-beams)
        ;; first update the record of energized cells
        new-energized (reduce apply-energy-reducer energized unique-active-beams)
        ;; Next move the active beams
        new-active-beams (filter-out-of-range-beams contraption (mapcat (fn [active-beam] (move-beam contraption active-beam)) unique-active-beams))]
    (when debug
      (println new-active-beams)
      (dump new-energized))
    [contraption new-energized new-active-beams]))

(def simulate-round (memoize simulate-round-impl))

(defn count-energized [energized]
  (reduce + (map (fn [energized-row] (reduce + (map (fn [energized-directions] (if (empty? energized-directions) 0 1))energized-row))) energized)))

(defn simulate-all-rounds [data beam-start]
  (let [rows (count data)
        columns (count (first data))]
    (loop [contraption data
           energized (vec (map (fn [i] (vec (repeat columns #{}))) (range 0 rows)))
           active-beams [beam-start]]
      (if (empty? active-beams)
        energized
        (let [[new-contraption new-energized new-active-beams] (simulate-round [contraption energized active-beams])]
          (recur new-contraption new-energized new-active-beams))))))

(defn part-one [data]
  (count-energized (simulate-all-rounds data [0 0 :right])))

(defn potential-starting-beams [data]
  (let [rows (count data)
        columns (count (first data))]
    (mapcat identity [;; from top going down
                      (map (fn [column] [0 column :down]) (range 0 columns))
                      ;; from bottom going up
                      (map (fn [column] [(dec rows) column :up]) (range 0 columns))
                      ;; from left going right
                      (map (fn [row] [row 0 :right]) (range 0 rows))
                      ;; from right going left
                      (map (fn [row] [row (dec columns) :left]) (range 0 rows))])))

(defn part-two [data]
  (let [all-round-results (map (fn [beam-start] (count-energized (simulate-all-rounds data beam-start))) (potential-starting-beams data))]
    (apply max all-round-results)))
