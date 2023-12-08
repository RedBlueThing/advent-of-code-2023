
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def part-one-test-data-raw ["RL"
                    ""
                    "AAA = (BBB, CCC)"
                    "BBB = (DDD, EEE)"
                    "CCC = (ZZZ, GGG)"
                    "DDD = (DDD, DDD)"
                    "EEE = (EEE, EEE)"
                    "GGG = (GGG, GGG)"
                    "ZZZ = (ZZZ, ZZZ)"
                    ])

(def test-data-raw ["LR"
  ""
  "11A = (11B, XXX)"
  "11B = (XXX, 11Z)"
  "11Z = (11B, XXX)"
  "22A = (22B, XXX)"
  "22B = (22C, 22C)"
  "22C = (22Z, 22Z)"
  "22Z = (22B, 22B)"
  "XXX = (XXX, XXX)"])

(def real-data-raw (str/split-lines (slurp "day-eight.txt")))

(defn instruction-at-index [instructions index]
  (nth instructions (mod index (count instructions))))

(defn parse-instructions-and-tree [data]
  (let [instructions (first data)
        tree-data (rest (rest data))
        tree (reduce (fn [tree line]
                       (let [[root leaves] (str/split line #" = ")]
                         (assoc tree root (str/split (subs leaves 1 (dec (count leaves))) #", ")))) {} tree-data)]
    [instructions tree]))

(defn index-for-direction [instruction]
  (case instruction \L 0 \R 1))

(defn steps-between-nodes [data from is-finished?]
  (let [[instructions tree] (parse-instructions-and-tree data)]
    (loop [steps 0
           current-root from]
      (if (is-finished? current-root)
        steps
        (let [root-branches (tree current-root)]
          (recur (inc steps) (nth root-branches (index-for-direction (instruction-at-index instructions steps)))))))))

;; part one
(defn part-one [data]
  (steps-between-nodes data "AAA" (fn [node] (= node "ZZZ"))))

(defn nodes-ending-in [tree ending]
  (filter (fn [node] (= (last node) ending)) (keys tree)))

(defn node-ending-in? [node ending]
  (= (last node) ending))

(defn are-nodes-ending-in? [nodes ending]
  (every? (fn [node] (node-ending-in? node ending)) nodes))

(defn next-roots-for-index-and-current-roots [instructions tree steps current-roots]
  (doall (map (fn [current-root]
                (nth (tree current-root)
                     (index-for-direction (instruction-at-index instructions steps)))) current-roots)))

(def memo-next-roots-for-index-and-current-roots (memoize next-roots-for-index-and-current-roots))

(defn part-two-brute-force [data]
  ;; brute force? nope
  (let [[instructions tree] (parse-instructions-and-tree data)]
    (loop [steps 0
           current-roots (nodes-ending-in tree \A)]
      (if (are-nodes-ending-in? current-roots \Z)
        steps
        (do
          (recur (inc steps) (memo-next-roots-for-index-and-current-roots instructions tree steps current-roots)))))))


(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn lcm-seq [numbers]
  (reduce lcm numbers))

(defn part-two [data]
  (let [[instructions tree] (parse-instructions-and-tree data)
        ;; get a number of steps for each ..A -> ..Z
        all-steps (map (fn [from] (steps-between-nodes data from (fn [node] (node-ending-in? node \Z)))) (nodes-ending-in tree \A))]
    (lcm-seq all-steps)))
