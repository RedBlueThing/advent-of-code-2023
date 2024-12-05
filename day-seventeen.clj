
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(defn in? [coll elm] (some #(= elm %) coll))

(def real-data-raw (str/split-lines (slurp "day-seventeen.txt")))

(def test-data-raw ["2413432311323"
                    "3215453535623"
                    "3255245654254"
                    "3446585845452"
                    "4546657867536"
                    "1438598798454"
                    "4457876987766"
                    "3637877979653"
                    "4654967986887"
                    "4564679986453"
                    "1224686865563"
                    "2546548887735"
                    "4322674655533"])

(defn row-column-in-range [data row column]
  (let [rows (count data)
        columns (count (first data))]
    (and (>= row 0)
         (< row rows)
         (>= column 0)
         (< column columns))))

(defn row-and-column-in-direction [row column direction]
  (case direction
    :up [(dec row) column direction]
    :right [row (inc column) direction]
    :down [(inc row) column direction]
    :left [row (dec column) direction]))

(def max-moves-without-turns 3)

(defn allowed-moves-at-location-in-direction [data [[row column] current-direction no-turn-count heat-loss]]
  (let [allowed-directions (case current-direction
                             :left [:left :up :down]
                             :right [:right :up :down]
                             :up [:left :right :up]
                             :down [:left :right :down])
        filtered-allowed-directions (filter (fn [allowed-direction] (if (>= (inc no-turn-count) max-moves-without-turns)
                                                                          ;; remove the current direction
                                                                      (not= allowed-direction current-direction)
                                                                          ;; if we haven't hit the limit, we just keep all the directions
                                                                      true)) allowed-directions)]
    (map (fn [[row column direction]] [[row column] {:no-turn-count (if (= current-direction direction)
                                                                      (inc no-turn-count)
                                                                      0)
                                                     :direction direction
                                                     :current-heat-loss (+ heat-loss (Character/getNumericValue (nth (nth data row) column)))}])
         (filter
          ;; filter new moves to make sure they are in range
          (fn [[row column _]] (row-column-in-range data row column))
          ;; get the locations for the moves we decided on
          (map (fn [new-direction] (row-and-column-in-direction row column new-direction)) filtered-allowed-directions)))))

(defn prefer-move-instructions? [new-move-instructions existing-move-instructions]
  (let [{new-no-turn-count :no-turn-count
         new-direction :direction
         new-current-heat-loss :current-heat-loss} new-move-instructions
        {existing-no-turn-count :no-turn-count
         existing-direction :direction
         existing-current-heat-loss :current-heat-loss} existing-move-instructions]
    (assert (= new-direction existing-direction))
    (or (< new-current-heat-loss existing-current-heat-loss)
        (and (= new-current-heat-loss existing-current-heat-loss) (< new-no-turn-count existing-no-turn-count)))))

(defn consolidate-moves [moves]
  ;; Where a move looks like:
  ;; [[row column] {:no-turn-count 0, :direction :down, :current-heat-loss 1}]

  ;; Multiple directions on the current node can map to the same direction on
  ;; neighbour nodes. In that scenario we pick the "move" with the lowest
  ;; current-heat-loss. With matching current-heat-loss we will pick the move with the
  ;; lowest no-turn-count.
  (reduce (fn [consolidated-moves move]
            (let [[move-address move-instruction] move
                  existing-move-instruction-to-location (consolidated-moves move-address)]
              (if (nil? existing-move-instruction-to-location)
                (assoc consolidated-moves move-address move-instruction)
                ;; work out if this move is better than the existing one
                (if (prefer-move-instructions? move-instruction existing-move-instruction-to-location)
                  (assoc consolidated-moves move-address move-instruction)
                    ;; otherwise just leave as is
                  consolidated-moves)))) {} moves))

(defn smallest-heat-loss-for-node [distances-by-direction]
  ;; Returns the shortest path we have found to this node from ay direction. If
  ;; we haven't found this node (empty distances-by-direction) then the shortest
  ;; path is infinite.
  (if (empty? distances-by-direction)
    { :current-heat-loss ##Inf :no-turn-count ##Inf }
    (first (sort (fn [x y] (< (x :current-heat-loss) (y :current-heat-loss))) distances-by-direction))))

(defn smallest-finite-distance [nodes]
  ;; Run through our unvisited nodes and find the node with the smallest finite
  ;; distance from any direction.
  (first (sort (fn [first last] (let [first-heat-loss (smallest-heat-loss-for-node (second first))
                                      second-heat-loss (smallest-heat-loss-for-node (second last))]
                                  (or (< (first-heat-loss :current-heat-loss) (second-heat-loss :current-heat-loss))
                                      (and (= (first-heat-loss :current-heat-loss) (second-heat-loss :current-heat-loss))
                                           (< (first-heat-loss :no-turn-count) (second-heat-loss :no-turn-count)))))) nodes)))

;; Dijkstra's

;; 1. Mark all nodes as unvisited. Create a set of all the unvisited nodes called
;; the unvisited set.

;; 2. Assign to every node a distance from start value: for the starting node, it
;; is zero, and for all other nodes, it is infinity, since initially no path is
;; known to these nodes. During execution of the algorithm, the distance of a
;; node N is the length of the shortest path discovered so far between the
;; starting node and N.[17]

;; 3. From the unvisited set, select the current node to be the one with the
;; smallest finite distance; initially, this will be the starting node, which
;; has distance zero. If the unvisited set is empty, or contains only nodes with
;; infinite distance (which are unreachable), then the algorithm terminates by
;; going to step 6. If we are only concerned about the path to a target node, we
;; may terminate here if the current node is the target node. Otherwise, we can
;; continue to find the shortest paths to all reachable nodes.

;; 4. For the current node, consider all of its unvisited neighbors and update
;; their distances through the current node; compare the newly calculated
;; distance to the one currently assigned to the neighbor and assign it the
;; smaller one. For example, if the current node A is marked with a distance of
;; 6, and the edge connecting it with its neighbor B has length 2, then the
;; distance to B through A is 6 + 2 = 8. If B was previously marked with a
;; distance greater than 8, then update it to 8 (the path to B through A is
;; shorter). Otherwise, keep its current distance (the path to B through A is
;; not the shortest).

;; 5. When we are done considering all of the unvisited neighbors of the current
;; node, mark the current node as visited and remove it from the unvisited set.
;; This is so that a visited node is never checked again, which is correct
;; because the distance recorded on the current node is minimal (as ensured in
;; step 3), and thus final. Go back to step 3.

;; 6. Once the loop exits (steps 3–5), every visited node will contain its shortest
;; distance from the starting node.

;; hard coded start location
(def start-node [0 0])
;; hard code the distances by direction, given top/left, we enter by top left
;; and have zero current steps.
(def start-node-distances [{:direction :down :current-heat-loss 0 :no-turn-count 0}
                           {:direction :right :current-heat-loss 0 :no-turn-count 0}])

;; given the data get the target node (currently bottom right)
(defn target-node [data]
  (let [last-column-index (dec (count (first data)))
        last-row-index (dec (count data))]
    ;; get the bottom right node given the data
    [last-column-index last-row-index]))

(defn finished-dijkstra? [data current-node]
  (let [current-target-node (target-node data)
        [current-node-address current-node-distances-by-direction] current-node]
    (or
     (empty? current-node-distances-by-direction)
     (nil? current-node)
     (= current-node-address current-target-node))))

(defn dijkstra-update-neighbours [data unvisited current-node]
  ;; For each of the directions associated with the current node, work out
  ;; allowed neighbour nodes (with associated directions and new no-turn-counts
  ;; and steps). Once we have the moves for each neighbour, we need to
  ;; consolidate them (consolidate-moves)
  (let [[current-node-address current-node-distances-by-direction] current-node

        new-moves (mapcat identity (map (fn [{direction :direction
                                              current-heat-loss :current-heat-loss
                                              no-turn-count :no-turn-count}]
                                          (allowed-moves-at-location-in-direction data [current-node-address direction no-turn-count current-heat-loss])) current-node-distances-by-direction))

        consolidated-moves (consolidate-moves new-moves)
        ;; only included moves to nodes in the unvisited
        filtered-consolidated-moves (into {} (filter (fn [[k v]] (not (nil? (unvisited k)))) consolidated-moves))]
    (reduce (fn [updated-unvisited move] (let [existing-distances-by-direction (updated-unvisited (first move))]
                                           (assoc updated-unvisited (first move) (conj existing-distances-by-direction (second move))))) unvisited filtered-consolidated-moves)))

;; (assert (= (dijkstra-update-neighbours test-data-raw {} [[0 0] start-node-distances]) []))

(defn dijkstra [data]
  (let [columns                   (count (first data))
        rows                      (count data)
        unvisited                 (into {} (mapcat identity (map (fn [row] (map (fn [column] [[row column] []]) (range 0 columns))) (range 0 rows))))
        current-start-node        start-node
        unvisited-with-start-node (assoc unvisited current-start-node start-node-distances)]
    (loop [current-unvisited unvisited-with-start-node
           ;; don't really need this but keeping it for debugging.
           current-visited   {}
           current-node      (smallest-finite-distance current-unvisited)
           i                 0]
      (if (or (finished-dijkstra? data current-node)
              ;; escape valve
              ;; (= i 10)
              )
        current-node
        (let [new-visited   (apply assoc current-visited current-node)
              new-unvisited (apply dissoc (dijkstra-update-neighbours data current-unvisited current-node) current-node)]
          (recur new-unvisited new-visited (smallest-finite-distance new-unvisited) (inc i)))))))
