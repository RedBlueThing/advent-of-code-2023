
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["...#......"
                    ".......#.."
                    "#........."
                    ".........."
                    "......#..."
                    ".#........"
                    ".........#"
                    ".........."
                    ".......#.."
                    "#...#....."])

(def real-data-raw (str/split-lines (slurp "day-eleven.txt")))

(defn read-data [data]
  (let [buffer (apply str data)
        width (count (first data))
        height (count data)]
    [width height (reduce (fn galaxy-reader [galaxies index]
                            (if (= (nth buffer index) \#)
                              (conj galaxies [(int (/ index width)) (mod index width)])
                              galaxies)) [] (range 0 (* width height)))]))

(defn empty-rows-and-columns [galaxy-data]
  (let [[width height galaxies] galaxy-data
        all-rows (set (range 0 height))
        all-columns (set (range 0 width))
        rows-with-galaxies (set (map first galaxies))
        columns-with-galaxies (set (map second galaxies))]
    [(set/difference all-rows rows-with-galaxies) (set/difference all-columns columns-with-galaxies)]))

(defn part-one-expansion [expanding-rows-or-columns]
  (* expanding-rows-or-columns 1))

(defn part-two-expansion [expanding-rows-or-columns]
  (* expanding-rows-or-columns 999999))

(defn expand-galaxy [galaxy-data expansion-fn]
  (let [[width height galaxies] galaxy-data
        [empty-row-set empty-column-set] (empty-rows-and-columns galaxy-data)]
    [width height (map (fn expand [[row column]]
                         (let [preceeding-rows (count (filter (fn [set-row] (< set-row row)) empty-row-set))
                               preceeding-columns (count (filter (fn [set-column] (< set-column column)) empty-column-set))]
                           [(+ row (expansion-fn preceeding-rows)) (+ column (expansion-fn preceeding-columns))]))
                       galaxies)]))

(defn distance [[[g1-row g1-column] [g2-row g2-column]]]
  (let [rows (sort [g1-row g2-row])
        columns (sort [g1-column g2-column])]
    (+ (- (second rows) (first rows)) (- (second columns) (first columns)))))

(defn solve [data expansion-fn]
  (let [galaxy-data (read-data data)
        [width height galaxies] galaxy-data
        galaxy-pairs (combo/combinations (last (expand-galaxy galaxy-data expansion-fn)) 2)]
    (reduce + (map distance galaxy-pairs))))

(defn part-one [data]
  (solve data part-one-expansion))

(defn part-two [data]
  (solve data part-two-expansion))
