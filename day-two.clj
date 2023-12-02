
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
                    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
                    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
                    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
                    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"])

(def possible-games [1 2 5])

(def real-data-raw (str/split-lines (slurp "day-two.txt")))

(defn parse-cube [cube-data]
  (let [[number-str color] (str/split cube-data #" ")]
    [color (Integer/parseInt (str/trim number-str))]))

(defn parse-subset [subset]
  (let [all-cube-data (str/split (str/trim subset) #", ")]
    (into {} (map parse-cube all-cube-data))))

(defn parse-game-line [line]
  (let [[game cubes-str] (str/split line #": ")
        game-number (Integer/parseInt (second (str/split game #" ")))
        cube-subsets (str/split cubes-str #";")
        subsets (map parse-subset cube-subsets)]
    [game-number subsets]))

(defn parse-games [data]
  (map parse-game-line data))

(def part-one-cubes {"red" 12
                      "green" 13
                      "blue" 14})

(defn game-possible? [game possible-cubes]
  ;; does any subset of this game have more cubes than possible cubes?
  (empty? (filter (fn subset-possible [subset]
                    (let [impossible-colors (filter (fn color-possible [color]
                                                      (> (or (subset color) 0) (possible-cubes color)))
                                                    (keys possible-cubes))]
                      (seq impossible-colors)))
                  (second game))))

(defn part-one [data possible-cubes]
  (reduce + (map first (filter (fn [game] (game-possible? game possible-cubes)) (parse-games data)))))

(defn max-cubes [game]
  (let [subsets (second game)]
    (loop [i 0
           max-cubes-dictionary {"green" 0 "red" 0 "blue" 0}]
      (if (= i (count subsets))
        max-cubes-dictionary
        (let [current-subset (nth subsets i)
              new-max-cubes-dictionary (reduce (fn max-reducer [dict next-subset-key]
                                                 (assoc dict next-subset-key (if (> (current-subset next-subset-key)
                                                                                    (dict next-subset-key))
                                                                               (current-subset next-subset-key)
                                                                               (dict next-subset-key)
                                                                               ))) max-cubes-dictionary (keys current-subset))]
          (recur (inc i) new-max-cubes-dictionary))))))

(defn min-viable-cubes-power [game]
  (let [max-cubes-dictionary (max-cubes game)]
    (apply * (map (fn [color] (max-cubes-dictionary color)) ["green" "red" "blue"]))))

(defn part-two [data]
  (reduce + (map min-viable-cubes-power (parse-games data))))
