
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
    (let [rgb-dictionary (into {} (map parse-cube all-cube-data))]
         (apply vector (map
                        (fn [color] (or (rgb-dictionary color) 0))
                        ["red" "green" "blue"])))))

(defn parse-game-line [line]
  (let [[game cubes-str] (str/split line #": ")
        game-number (Integer/parseInt (second (str/split game #" ")))
        cube-subsets (str/split cubes-str #";")
        subsets (map parse-subset cube-subsets)]
    [game-number subsets]))

(defn parse-games [data]
  (map parse-game-line data))

(def part-one-rgb [12 13 14])

(defn game-possible? [game [possible-r possible-g possible-b]]
  ;; does any subset of this game have more cubes than possible cubes?
  (boolean (every? (fn subset-possible [[subset-r subset-g subset-b]]
           (and (<= subset-r possible-r)
               (<= subset-g possible-g)
               (<= subset-b possible-b))) (second game))))

(defn part-one [data possible-rgb]
  (reduce + (map first (filter (fn [game] (game-possible? game possible-rgb)) (parse-games data)))))

(defn max-rgb-for-subsets [subsets]
  (map (fn max-color-at-index [color-index]
         (apply max (map
                     (fn nth-color[subset]
                       (nth subset color-index)) subsets)) ) [0 1 2]))

(defn min-viable-cubes-power [game]
  (apply * (max-rgb-for-subsets (second game))))

(defn part-two [data]
  (reduce + (map min-viable-cubes-power (parse-games data))))
