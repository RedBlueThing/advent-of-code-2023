
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw-part-one ["7-F7-"
                    ".FJ|7"
                    "SJLL7"
                    "|F--J"
                    "LJ.LJ"])

(def test-data-raw-complex ["FF7FSF7F7F7F7F7F---7"
                    "L|LJ||||||||||||F--J"
                    "FL-7LJLJ||||||LJL-77"
                    "F--JF--7||LJLJ7F7FJ-"
                    "L---JF-JLJ.||-FJLJJ7"
                    "|F|F-JF---7F7-L7L|7|"
                    "|FFJF7L7F-JF7|JL---7"
                    "7-L-JL7||F7|L7F-7F7|"
                    "L.L7LFJ|||||FJL7||LJ"
                    "L7JLJL-JLJLJL--JLJ.L"])


(def  test-data-raw-closed [".........."
  ".S------7."
  ".|F----7|."
  ".||OOOO||."
  ".||OOOO||."
  ".|L-7F-J|."
  ".|II||II|."
  ".L--JL--J."
  ".........."])

(def test-data-raw ["..........."
                    ".S-------7."
                    ".|F-----7|."
                    ".||.....||."
                    ".||.....||."
                    ".|L-7.F-J|."
                    ".|..|.|..|."
                    ".L--J.L--J."
                    "..........."])

(def real-data-raw (str/split-lines (slurp "day-ten.txt")))

(defn read-map [data]
  [(count (first data)) (count data) (apply str data)])

(defn find-start [map-data]
  (let [[width height buffer] map-data]
    (str/index-of buffer \S)))

(defn row-and-column [map-data index]
  (assert index)
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

(defn loop-for-map [map-data]
  (let [index-for-start (find-start map-data)
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
  (let [[longest-path entire-loop] (loop-for-map (read-map data))]
    longest-path))

(defn add-indices-to-flood-fill [current-set indicies]
  (apply conj current-set indicies))

(defn contains-value? [coll value]
  (some #(= % value) coll))

(def contains-value-memo? (memoize contains-value?))

(defn flood-fill [map-data index path]
  (if (contains-value-memo? path index)
    []
    (loop [remaining [index]
           filled #{}]
      (if (empty? remaining)
        filled
        (let [current (first remaining)
              new-remaining (rest remaining)
              neighbours (indexes-for-neighbours map-data current)
              check-index (fn [index] (and (not (contains-value-memo? filled index))
                                           (not (contains-value-memo? path index))
                                           (not (contains-value-memo? remaining index))))
              unfilled-neighbours (filter check-index neighbours)]
          (recur (vec (apply conj new-remaining unfilled-neighbours)) (if (contains-value-memo? path current) filled (conj filled current))))))))

(def flood-fill-memo (memoize flood-fill))

(defn combine-flood-fills [map-data cells entire-loop filled-set]
  (apply concat (map (fn [index] (if (contains-value-memo? filled-set index)
                                   ;; if the filled set already contains this index, then nothing to do here.
                                   #{}
                                   (flood-fill-memo map-data index entire-loop))) cells)))

(defn index-in-direction-or-nil [map-data index direction]
  (let [[row column] (row-and-column map-data index)]
    (case direction
      :east (index-for-row-and-column map-data row (inc column))
      :west (index-for-row-and-column map-data row (dec column))
      :north (index-for-row-and-column map-data (dec row) column)
      :south (index-for-row-and-column map-data (inc row) column))))

(defn left-right-indicies [map-data path path-index]
  (let [[width height buffer] map-data
        previous-path-index (if (= path-index 0) (dec (count path)) (dec path-index))
        index-at-previous-path-location (nth path previous-path-index)
        character-at-previous-path-location (nth buffer index-at-previous-path-location)
        index-at-path-location (nth path path-index)
        character-at-path-location (nth buffer index-at-path-location)
        character-at-previous-path-location (nth buffer index-at-previous-path-location)
        direction-into-path-location (cond
                                       (= index-at-previous-path-location (dec index-at-path-location)) :east
                                       (= index-at-previous-path-location (inc index-at-path-location)) :west
                                       (< index-at-previous-path-location index-at-path-location) :south
                                       (> index-at-previous-path-location index-at-path-location) :north)
        ;; we are returning cells to the left and right respectively.
        [left-cells right-cells] (case character-at-path-location
                                   ;; FIXME maybe (this didn't matter in the end
                                   ;; because there weren't any fillable cells
                                   ;; near the start).
                                   \S [[] []]
                                   \| (if (= direction-into-path-location :south)
                                        [[(index-in-direction-or-nil map-data index-at-path-location :east)]
                                         [(index-in-direction-or-nil map-data index-at-path-location :west)]]
                                        [[(index-in-direction-or-nil map-data index-at-path-location :west)]
                                         [(index-in-direction-or-nil map-data index-at-path-location :east)]])
                                   \- (if (= direction-into-path-location :east)
                                        [[(index-in-direction-or-nil map-data index-at-path-location :north)]
                                         [(index-in-direction-or-nil map-data index-at-path-location :south)]]
                                        [[(index-in-direction-or-nil map-data index-at-path-location :south)]
                                         [(index-in-direction-or-nil map-data index-at-path-location :north)]])
                                   \L (if (= direction-into-path-location :north)
                                        [[] [(index-in-direction-or-nil map-data index-at-path-location :south)
                                             (index-in-direction-or-nil map-data index-at-path-location :west)]]
                                        [[(index-in-direction-or-nil map-data index-at-path-location :west)
                                          (index-in-direction-or-nil map-data index-at-path-location :south)] []])
                                   \J (if (= direction-into-path-location :south)
                                        [[(index-in-direction-or-nil map-data index-at-path-location :east)
                                          (index-in-direction-or-nil map-data index-at-path-location :south)] []]
                                        [[] [(index-in-direction-or-nil map-data index-at-path-location :east)
                                             (index-in-direction-or-nil map-data index-at-path-location :south)]])
                                   \7 (if (= direction-into-path-location :north)
                                        [[] [(index-in-direction-or-nil map-data index-at-path-location :east)
                                             (index-in-direction-or-nil map-data index-at-path-location :north)]]
                                        [[(index-in-direction-or-nil map-data index-at-path-location :north)
                                          (index-in-direction-or-nil map-data index-at-path-location :east)] []])
                                   \F (if (= direction-into-path-location :north)
                                        [[(index-in-direction-or-nil map-data index-at-path-location :west)
                                          (index-in-direction-or-nil map-data index-at-path-location :north)] []]
                                        [[] [(index-in-direction-or-nil map-data index-at-path-location :north)
                                             (index-in-direction-or-nil map-data index-at-path-location :west)]]))]

    (map (fn filter-for-nills [cells] (filter some? cells)) [left-cells right-cells])))

(let [map-data (read-map test-data-raw)
      [longest-path entire-loop] (loop-for-map map-data)]
  (left-right-indicies map-data entire-loop 7))

(defn part-two [data]
  (let [map-data (read-map data)
        [longest-path most-loop] (loop-for-map map-data)
        entire-loop (concat most-loop [(find-start map-data)])]
    (println "Scanning" (count entire-loop))
    (loop [i 0
           left-set #{}
           right-set #{}]
      (if (= (mod i 1000) 0) (println "Checking" i))
      (if (= (count entire-loop) i)
        ;; we are done looking at the whole loop
        [left-set right-set]
        (let [[left-cells right-cells] (left-right-indicies map-data entire-loop i)
              [left-fills right-fills] [(combine-flood-fills map-data left-cells entire-loop left-set)
                                        (combine-flood-fills map-data right-cells entire-loop right-set)]]
          (recur
           (inc i)
           (add-indices-to-flood-fill left-set left-fills)
           (add-indices-to-flood-fill right-set right-fills)))))))

(defn replace-at [s idx replacement]
  (str (subs s 0 idx) replacement (subs s (inc idx))))

(defn render-map [map-data overlay]
  (let [[width height buffer] map-data
        updated-buffer (reduce (fn [current-buffer index] (replace-at current-buffer index \X)) buffer overlay)]
    (doseq [row (range 0 height)]
      (let [index-at-start (index-for-row-and-column map-data row 0)
            sub-array (subs updated-buffer index-at-start (+ index-at-start width))]
        (println sub-array)))))

(require '[clojure.java.io :as io])
(import '[java.awt Color Graphics2D Rectangle]
        '[java.awt.image BufferedImage]
        '[javax.imageio ImageIO]
        '[javax.swing ImageIcon JLabel JFrame])

(defn coor-for-cell [character]
  (or ({\- Color/RED
        \| Color/RED
        \F Color/RED
        \J Color/RED
        \7 Color/RED
        \L Color/RED} character) Color/GRAY))

(def gutter 0)
(def tile-size 10)

(defn render-size [map-data]
  (let [[width height buffer] map-data]
    [(* width (+ tile-size (* gutter 2))) (* height (+ tile-size (* gutter 2)))]))

(defn draw-character [graphics character cell-left cell-top]
  (let [color (color-for-cell character)
        inset (/ tile-size 3)
        inset-width-height (- tile-size (* inset 2))]
    (case character
      \- (do (.setColor graphics color)
             (.fillRect graphics cell-left (+ cell-top inset) tile-size inset))
      \| (do (.setColor graphics color)
             (.fillRect graphics (+ cell-left inset) cell-top inset tile-size))
      \F (do (.setColor graphics color)
             (.fillRect graphics (+ cell-left inset) (+ cell-top inset) (* inset-width-height 2) inset-width-height)
             (.fillRect graphics (+ cell-left inset) (+ cell-top inset) (/ tile-size 3) (* inset-width-height 2)))
      \J (do (.setColor graphics color)
             (.fillRect graphics cell-left (+ cell-top inset) (* inset-width-height 2) inset-width-height)
             (.fillRect graphics (+ cell-left inset) cell-top (/ tile-size 3) (* inset-width-height 2)))
      \7 (do (.setColor graphics color)
             (.fillRect graphics cell-left (+ cell-top inset) (* inset-width-height 2) inset-width-height)
             (.fillRect graphics (+ cell-left inset) (+ cell-top inset) (/ tile-size 3) (* inset-width-height 2)))
      \L (do (.setColor graphics color)
             (.fillRect graphics (+ cell-left inset) (+ cell-top inset) (* inset-width-height 2) inset-width-height)
             (.fillRect graphics (+ cell-left inset) cell-top (/ tile-size 3) (* inset-width-height 2)))
      \X (do (.setColor graphics color)
             (.fillRect graphics (+ cell-left inset) (+ cell-top inset) inset-width-height inset-width-height))
      nil)))

(defn remove-crud [map-data]
  (let [[width height buffer] map-data
        [longest-path loop] (loop-for-map map-data)
        updated-buffer (reduce (fn [current-buffer index]
                                 (if (contains-value-memo? loop index)
                                   current-buffer
                                   (replace-at current-buffer index \.))) buffer (range 0 (count buffer)))]
    [width height updated-buffer]))

(defn add-fill [map-data fill-set]
  (let [[width height buffer] map-data
        updated-buffer (reduce (fn [current-buffer index] (replace-at current-buffer index \X)) buffer fill-set)]
    [width height updated-buffer]))

(defn draw-map [map-data graphics]
  (let [[width height buffer] map-data]
    (doseq [row (range 0 height)]
      (let [index-at-start (index-for-row-and-column map-data row 0)]
        (doseq [column (range 0 width)]
          (let [cell-left (* column tile-size)
                cell-top (* row tile-size)
                character (nth buffer (index-for-row-and-column map-data row column))]
            (draw-character graphics character cell-left cell-top)))))))

;; (doseq [[row-index row] (map-indexed vector (reverse (:data game-board)))]
;;     (doseq [[cell-index cell] (map-indexed vector row)]
;;       (let [cell-left (* cell-index tile-size)
;;             cell-top (* row-index tile-size)
;;             color (color-for-cell cell)]
;;         (do (.setColor graphics color)
;;             (.fillRect graphics cell-left cell-top tile-size tile-size)
;;             (.setColor graphics (.darker color))
;;             (.fillRect graphics (+ cell-left 2) (+ cell-top 2) (- tile-size 4) (- tile-size 4))))))

(def map-file "maze.png")

(defn render-map-graphics [map-data fn]
  (let [filename fn
        [width height] (render-size map-data)
        image (BufferedImage. width height BufferedImage/TYPE_INT_RGB)]
    (draw-map map-data (.createGraphics image))
    (ImageIO/write image "png" (io/file filename))
    (doto (JFrame. "Image")
      (.add (doto (JLabel.)
              (.setIcon (ImageIcon. (str (io/file filename))))))
      (.pack)
      (.setVisible true))))
