
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw-part-one ["7-F7-"
                    ".FJ|7"
                    "SJLL7"
                    "|F--J"
                    "LJ.LJ"])

(def test-data-raw ["FF7FSF7F7F7F7F7F---7"
  "L|LJ||||||||||||F--J"
  "FL-7LJLJ||||||LJL-77"
  "F--JF--7||LJLJ7F7FJ-"
  "L---JF-JLJ.||-FJLJJ7"
  "|F|F-JF---7F7-L7L|7|"
  "|FFJF7L7F-JF7|JL---7"
  "7-L-JL7||F7|L7F-7F7|"
  "L.L7LFJ|||||FJL7||LJ"
  "L7JLJL-JLJLJL--JLJ.L"])

(def real-data-raw (str/split-lines (slurp "day-ten.txt")))

(defn read-map [data]
  [(count (first data)) (count data) (apply str data)])

(defn find-start [map-data]
   (let [[width height buffer] map-data]
     (str/index-of buffer \S)))

(defn row-and-column [map-data index]
  (let [[width height buffer] map-data]
    [(int (/ index width)) (mod index width)]))

(defn index-for-row-and-column [map-data row column]
  ;; check if the row and column is in range and return the index if it is
  (let [[width height buffer] map-data]
    (if (or (>= row height) (< row 0) (>= column width) (< column 0))
      nil
      (+ (* row width) column))))

(defn index-for-direction [map-data index direction]
  (let [[row column] (row-and-column map-data index)]
    (case direction
          :north (index-for-row-and-column map-data (dec row) column)
          :south (index-for-row-and-column map-data (inc row) column)
          :east (index-for-row-and-column map-data row (inc column))
          :west (index-for-row-and-column map-data row (dec column)))))

(defn indexes-for-neighbours [map-data index]
  (filter some? [(index-for-direction map-data index :north)
                 (index-for-direction map-data index :east)
                 (index-for-direction map-data index :south)
                 (index-for-direction map-data index :west)]))

(defn exits-for-location [map-data index]
  ;; given a location on the map, find all the possible exits
  (assert index)
  (let [[width height buffer] map-data
        character-at-location (nth buffer index)]
    (filter some? (case character-at-location
                    \S []
                    \| [(index-for-direction map-data index :north) (index-for-direction map-data index :south)]
                    \- [(index-for-direction map-data index :east) (index-for-direction map-data index :west)]
                    \L [(index-for-direction map-data index :north) (index-for-direction map-data index :east)]
                    \J [(index-for-direction map-data index :north) (index-for-direction map-data index :west)]
                    \7 [(index-for-direction map-data index :south) (index-for-direction map-data index :west)]
                    \F [(index-for-direction map-data index :south) (index-for-direction map-data index :east)]
                    \. []))))

(defn exits-for-start-location [map-data index]
  ;; given a location on the map, find all the possible exits
  (let [neighbour-indexes (indexes-for-neighbours map-data index)]
    ;; which of the neighbours has an exit that is our start location
    (filter (fn [neighbour-index] (some (fn [index-for-neightbour-exit] (= index-for-neightbour-exit index))
                                        (exits-for-location map-data neighbour-index))) neighbour-indexes)))


(defn next-index-for-path [map-data path]
  (let [current (last path)
        previous (or (last (butlast path)) (find-start map-data))
        exits (exits-for-location map-data current)]
    (first (filter (fn [index] (not= index previous)) exits))))

(defn loop-for-map [data]
  (let [map-data (read-map data)
        index-for-start (find-start map-data)
        [path-one-start path-two-start] (exits-for-start-location map-data index-for-start)]
    (loop [neighbour-paths [[path-one-start] [path-two-start]]]
      (let [[path-one path-two] neighbour-paths]
        (if (= (last path-one) (last path-two))
          ;; we are done
          [(count path-one) (concat path-one (reverse (butlast path-two)))]
          ;; keep going
          (recur [(conj path-one (next-index-for-path map-data path-one))
                  (conj path-two (next-index-for-path map-data path-two))]))))))

(defn part-one [data]
  (let [[longest-path entire-loop] (loop-for-map data)]
    longest-path))
