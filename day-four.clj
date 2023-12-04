
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
                    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
                    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
                    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
                    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
                    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"])

(def real-data-raw (str/split-lines (slurp "day-four.txt")))

(defn parse-numbers [numbers-str]
  (map (fn [number-str] (Integer/parseInt number-str)) (filter #(not= % "") (str/split (str/trim numbers-str) #" "))))

(defn parse-card [line]
  (let [[card-str numbers-str] (str/split line #": ")
        card-number (Integer/parseInt (last (str/split card-str #" ")))
        [winning-numbers-str have-numbers-str] (str/split numbers-str #" \| ")
        winning-numbers (parse-numbers winning-numbers-str)
        have-numbers (parse-numbers have-numbers-str)]
    [card-number (set winning-numbers) (set have-numbers)]))

(defn parse-cards [data]
  (map parse-card data))

(defn wins-for-card [card]
    (let [[card-number winning-numbers-set have-numbers-set] card]
      (count (set/intersection have-numbers-set winning-numbers-set))))

(defn score-card [card]
  (let [[card-number winning-numbers-set have-numbers-set] card
        winning-numbers (wins-for-card card)]
    (cond
      (= winning-numbers 1) 1
      (> winning-numbers 1) (pow 2 (dec winning-numbers))
      :else 0)))

(defn part-one [data]
  (int (reduce + (map score-card (parse-cards data)))))

(defn part-two [data]
  (let [wins-for-cards (map wins-for-card (parse-cards data))
        indexed-wins-for-cards (map-indexed (fn [index wins] [index wins]) wins-for-cards)
        ;; just one for each card
        starting-cards (apply vector (map (fn [index] 1) (range 0 (count wins-for-cards))))
        cards-with-copies (reduce (fn [current-cards [index wins]]
                                    (if (> wins 0)
                                      ;; we had wins so we need to update the number of following cards
                                      (let [first-index (inc index)
                                            last-index (+ index wins)
                                            following-indexes-to-update (range first-index (inc last-index))]
                                        (reduce (fn [new-current-cards following-card-index]
                                                  (let [cards-at-index (nth new-current-cards index)
                                                        previous-cards-at-index  (nth new-current-cards following-card-index)]
                                                    ;; where cards-at-index is
                                                    ;; the number of cards
                                                    ;; generating new cards for
                                                    ;; this
                                                    ;; range (following-card-index).
                                                    ;; Hence the multiple.
                                                    (assoc new-current-cards following-card-index (+ previous-cards-at-index cards-at-index))))
                                                current-cards following-indexes-to-update))
                                      ;; nothing changes
                                      current-cards))

                                  starting-cards
                                  indexed-wins-for-cards)]
    (reduce + cards-with-copies)))

