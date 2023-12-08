
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["RL"
                    ""
                    "AAA = (BBB, CCC)"
                    "BBB = (DDD, EEE)"
                    "CCC = (ZZZ, GGG)"
                    "DDD = (DDD, DDD)"
                    "EEE = (EEE, EEE)"
                    "GGG = (GGG, GGG)"
                    "ZZZ = (ZZZ, ZZZ)"
                    ])

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

(defn part-one [data]
  (let [[instructions tree] (parse-instructions-and-tree data)]
    (loop [steps 0
           current-root "AAA"]
      (if (= current-root "ZZZ")
        steps
        (let [root-branches (tree current-root)]
          (recur (inc steps) (nth root-branches (index-for-direction (instruction-at-index instructions steps)))))))))
