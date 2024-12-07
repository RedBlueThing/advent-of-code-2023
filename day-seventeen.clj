
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])
(require '[clojure.data.priority-map :as pm])

(import 'java.util.PriorityQueue)

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

(defn core-allowed-states-at-location-in-direction [data [[row column] current-direction no-turn-count heat-loss]]
  (let [allowed-directions (case current-direction
                             :left [:left :up :down]
                             :right [:right :up :down]
                             :up [:left :right :up]
                             :down [:left :right :down])
        filtered-allowed-directions (filter (fn [allowed-direction]
                                              (if (>= (inc no-turn-count) max-moves-without-turns)
                                                ;; remove the current direction
                                                (not= allowed-direction current-direction)
                                                ;; if we haven't hit the limit, we just keep all the directions
                                                true)) allowed-directions)]
    (map (fn [[row column direction]] [[row column] {:no-turn-count (if (= current-direction direction) (inc no-turn-count) 0)
                                                     :direction direction
                                                     :current-heat-loss (+ heat-loss (nth (nth data row) column))}])
         (filter
          ;; filter new moves to make sure they are in range
          (fn [[row column _]] (row-column-in-range data row column))
          ;; get the locations for the moves we decided on
          (map (fn [new-direction] (row-and-column-in-direction row column new-direction)) filtered-allowed-directions)))))

(def allowed-states-at-location-in-direction (memoize core-allowed-states-at-location-in-direction))

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

;; Our start states are that we entered the start-node from the top and the
;; left, so travelling down and right, and have zero no-turn-count.
(def start-states [[start-node {:direction :down :current-heat-loss 0 :no-turn-count 0}]
                   [start-node {:direction :right :current-heat-loss 0 :no-turn-count 0}]])

;; given the data get the target node (currently bottom right)
(defn target-node [data]
  (let [last-column-index (dec (count (first data)))
        last-row-index (dec (count data))]
    ;; get the bottom right node given the data
    [last-column-index last-row-index]))

(defn finished-dijkstra? [data current-state]
  (let [current-target-node (target-node data)
        [current-state-address current-state-data] current-state]
    (= current-state-address current-target-node)))

(defn dijkstra-new-neighbour-states [data current-state current-visited]
  ;; We are going to returns states for neighbours that we
  ;; can visit from the current state.
  ;;
  ;; We have the curent visisted because there is no point returning new states
  ;; with a larger no-turn-count for an address (we know the heat loss will be
  ;; higher because we sorted unvisited each time we dequeue)
  (let [[current-state-address {direction :direction current-heat-loss :current-heat-loss no-turn-count :no-turn-count}] current-state]
    (filter (fn [new-allowed-state]
              (let [[check-address {check-direction :direction
                                    check-current-heat-loss :current-heat-loss
                                    check-no-turn-count :no-turn-count}] new-allowed-state
                    previous-no-turn-count (or (current-visited check-address) ##Inf)]
                (< check-no-turn-count previous-no-turn-count)))
            (allowed-states-at-location-in-direction data [current-state-address direction no-turn-count current-heat-loss]))))

(defn update-visited [visited current-state-address no-turn-count]
  (let [smallest-no-turn-count (or (visited current-state-address) ##Inf)]
    (if (< no-turn-count smallest-no-turn-count)
      (assoc visited current-state-address no-turn-count)
      visited
    )))

(defn dijkstra [data]
  ;; using this mutable java priority queue thing is weird and feels very wrong in Clojure
  (let [mutable-unvisited-queue (PriorityQueue. 196000 (reify java.util.Comparator
                                                         (compare [_ [first-address {first-direction :direction
                                                                                     first-current-heat-loss :current-heat-loss
                                                                                     first-no-turn-count :no-turn-count}]
                                                                   [last-address {last-direction :direction
                                                                                  last-current-heat-loss :current-heat-loss
                                                                                  last-no-turn-count :no-turn-count}]]
                                                           (compare first-current-heat-loss last-current-heat-loss))))
        parsed-data (map (fn [line] (map (fn [chr] (Character/getNumericValue chr)) line)) data)]

    ;; before we start we have to putate our mutable-unvisited-queue with our start states
    (.add mutable-unvisited-queue (nth start-states 0))
    (.add mutable-unvisited-queue (nth start-states 1))

    (loop [current-visited   {}
           current-state     (.poll mutable-unvisited-queue)
           i                 0
           [max-row max-column]    [0 0]]
      (if (or (finished-dijkstra? parsed-data current-state)
              ;; escape valve
              ;; (= i 10000)
              )
        current-state
        (let [[current-state-address {direction :direction current-heat-loss :current-heat-loss no-turn-count :no-turn-count}] current-state
              [row column] current-state-address
              [new-max-row new-max-column] [(max max-row row) (max max-column column)]
              new-visited   (update-visited current-visited current-state-address no-turn-count)]

          ;; (when (or (> new-max-row max-row) (> new-max-column max-column)) (println "New max" new-max-row new-max-column row column))
          ;; (when (= 0 (mod current-heat-loss 100)) (println current-heat-loss))
          ;; (println (count new-visited))

          ;; mutate our mutable-unvisisted with the new neighbour states
          (doseq [new-state (dijkstra-new-neighbour-states parsed-data current-state new-visited)]
            (when (not (.contains mutable-unvisited-queue new-state))
              (.add mutable-unvisited-queue new-state)))

          (recur new-visited (.poll mutable-unvisited-queue) (inc i) [new-max-row new-max-column]))))))

