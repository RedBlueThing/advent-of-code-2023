
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["O....#...."
                    "O.OO#....#"
                    ".....##..."
                    "OO.#O....O"
                    ".O.....O#."
                    "O.#..O.#.#"
                    "..O..#O..O"
                    ".......O.."
                    "#....###.."
                    "#OO..#...."])

(def real-data-raw (str/split-lines (slurp "day-fourteen.txt")))

(defn rotate-data-90-cw [data]
  ;; this works for row data
  (apply mapv str (map vec (reverse data))))

(defn rows-to-columns [data]
  ;; convert the data into a vector of columns (instead of the rows we read in)
  (apply mapv str (map vec data)))

(def columns-to-rows rows-to-columns)

(defn weigh-column [column]
  ;; given a string that represents a column where zero is the edge we are
  ;; tiling towards and (dec (count column)) is the edge furthest from the edge,
  ;; we can determine weight of all the rocks \O
  ;;
  ;; "The amount of load caused by a single rounded rock (O) is equal to the
  ;; number of rows from the rock to the south edge of the platform, including
  ;; the row the rock is on."
  (reduce + (map-indexed (fn [index chr]
                           (if (= chr \O)
                             (- (count column) index)
                             0)) column)))


(defn slide-to [i column]
  (assert (< i (dec (count column))))
  (case (nth column i)
    (\# \O) [(inc i) column]
    ;; If the spot is empty then we can scan forward to the next non-empty spot
    \.      (let [rest              (subs column (inc i))
                  round-rock-groups (re-find #"([^O]*)(O+)" rest)]
              (if (nil? round-rock-groups)
                ;; if there are no more round-rocks, then advance to the end
                [(count column) column]
                ;; if there are round-rocks, then need to look at the pre-round-rock stuff
                (let [[_ pre-round-rocks round-rocks] round-rock-groups
                      square-rock-index             (str/index-of pre-round-rocks "#")]
                  (if (nil? square-rock-index)
                    ;; it's all free so we can just move the round rocks
                    [(+ i (count round-rocks)) (let [cleared-space (+ (count pre-round-rocks) (count round-rocks))]
                                                 (apply str (concat
                                                              ;; everything before i
                                                             (subs column 0 i)
                                                              ;; the round rocks
                                                             round-rocks
                                                              ;; the length of round-rocks replaced with blank spaces
                                                             (apply str (take (inc (count pre-round-rocks)) (repeat \.)))
                                                              ;; the rest of the string
                                                             (subs column (inc (+ cleared-space i))))))]

                    ;; otherwise we move past the square rock without any changes
                    [(+ i (inc square-rock-index)) column]))))))

(defn tilt-column-impl [column]
  ;; Slide any \O characters towards zero (stopping_if they hit a \# or \O)
  (loop [i  0
         remaining-column column]
    (if (>= i (dec (count column)))
      remaining-column
      (let [[next-i new-remaining-column] (slide-to i remaining-column)]
        (recur next-i new-remaining-column)))))

(def tilt-column (memoize tilt-column-impl))

(reduce + (map (fn [data] (weigh-column (tilt-column data))) (rows-to-columns test-data-raw)))

(def part-two-cycles 1000000000)

(defn tilt-board-impl [board-data]
  (map (fn [data] (tilt-column data)) board-data))

(def tilt-board (memoize tilt-board-impl))

(defn rotate-columns-90-cw [data]
  (mapv #(apply str %) (reverse (apply mapv vector (map vec data)))))

(defn run-cycle-impl [data]
  (-> data
      tilt-board
      rotate-columns-90-cw
      tilt-board
      rotate-columns-90-cw
      tilt-board
      rotate-columns-90-cw
      tilt-board
      rotate-columns-90-cw))

(def run-cycle (memoize run-cycle-impl))

(defn part-two-data [data cycles]
  (loop [i 0
         current-data data]
    (if (= i cycles)
      current-data
      (recur (inc i) (run-cycle current-data)))))

(defn part-two [data cycles]
  (reduce + (map (fn [column-data] (weigh-column column-data)) (part-two-data data cycles))))
