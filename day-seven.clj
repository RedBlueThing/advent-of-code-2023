
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["32T3K 765"
                    "T55J5 684"
                    "KK677 28"
                    "KTJJT 220"
                    "QQQJA 483"])

(def real-data-raw (str/split-lines (slurp "day-seven.txt")))

(defn scores-for-things [things]
  (into {} (map-indexed (fn [i thing] [thing i]) (reverse things))))

(def part-one-card-strength-scores (scores-for-things [\A \K \Q \J \T \9 \8 \7 \6 \5 \4 \3 \2]))
(def part-two-card-strength-scores (scores-for-things [\A \K \Q \T \9 \8 \7 \6 \5 \4 \3 \2 \J]))

(def hand-type-scores (scores-for-things [:five-of-a-kind :four-of-a-kind :full-house :three-of-a-kind :two-pair :one-pair :high-card]))

(defn hand-card-counts [hand]
  (let [cards (char-array hand)
        card-counts (reduce (fn [counts card]
                              (let [previous-count (or (counts card) 0)]
                                (assoc counts card (inc previous-count))
                                )) {} cards)]
    card-counts))

(defn is-five-of-a-kind? [card-counts]
  ;; if we only found one type of card, it's a five of a kind
  (= (count (keys card-counts)) 1))

(defn is-four-of-a-kind? [card-counts]
  ;; if there are two types of cards and either of them is repeated four times,
  ;; it's a four of a kind.
  (let [card-types (keys card-counts)]
    (and (= (count card-types) 2) (or
                                   (= (card-counts (first card-types)) 4)
                                   (= (card-counts (second card-types)) 4)))))

(defn is-full-house? [card-counts]
  ;; if again there are only two types of cards, but either of them is repeated
  ;; three times, it's a full house
  (let [card-types (keys card-counts)]
    (and (= (count card-types) 2) (some true? (map (fn [card] (= (card-counts card) 3)) card-types)))))

(defn is-three-of-a-kind? [card-counts]
  ;; if there are three types of card, but one of them is repeated three times
  (let [card-types (keys card-counts)]
    (and (= (count card-types) 3) (some true? (map (fn [card] (= (card-counts card) 3)) card-types)))))

(defn is-two-pair? [card-counts]
  ;; if there are three types of card, but one of them is repeated two times
  (let [card-types (keys card-counts)]
    (and (= (count card-types) 3) (some true? (map (fn [card] (= (card-counts card) 2)) card-types)))))

(defn is-one-pair? [card-counts]
  ;; if there are four types of card, but one of them is repeated
  (let [card-types (keys card-counts)]
    (and (= (count card-types) 4) (some true? (map (fn [card] (= (card-counts card) 2)) card-types)))))

(defn is-high-card? [card-counts]
  ;; five types of card
  (= (count (keys card-counts)) 5))

(defn part-one-type-for-hand [hand]
  (let [card-counts (hand-card-counts hand)]
    (cond
      (is-five-of-a-kind? card-counts) :five-of-a-kind
      (is-four-of-a-kind? card-counts) :four-of-a-kind
      (is-full-house? card-counts) :full-house
      (is-three-of-a-kind? card-counts) :three-of-a-kind
      (is-two-pair? card-counts) :two-pair
      (is-one-pair? card-counts) :one-pair
      (is-high-card? card-counts) :high-card)))

(def memo-type-for-hand (memoize type-for-hand))

(assert (= (part-one-type-for-hand "AAAAA") :five-of-a-kind))
(assert (= (part-one-type-for-hand "AA8AA") :four-of-a-kind))
(assert (= (part-one-type-for-hand "23332") :full-house))
(assert (= (part-one-type-for-hand "TTT98") :three-of-a-kind))
(assert (= (part-one-type-for-hand "23432") :two-pair))
(assert (= (part-one-type-for-hand "A23A4") :one-pair))
(assert (= (part-one-type-for-hand "23456") :high-card))

(defn equal-type-hand-comparitor [first-hand second-hand part-card-strength-scores]
  ;; look at each card in turn and return card from first > card from second
  (let [first-hand-cards (char-array first-hand)
        second-hand-cards (char-array second-hand)]
    (loop [i 0
           result nil]
      (if (or (= i (count first-hand)) (not (nil? result)))
        ;; we are done
        (do
          (assert (not (nil? result)))
          result)
        ;; keep looking
        (let [first-hand-card-strength (part-card-strength-scores (nth first-hand-cards i))
              second-hand-card-strength (part-card-strength-scores (nth second-hand-cards i))
              result (if (= first-hand-card-strength second-hand-card-strength)
                       nil
                       (> first-hand-card-strength second-hand-card-strength))]
          (recur (inc i) result))))))

(defn hand-comparitor [first-hand second-hand part-card-strength-scores part-type-for-hand]
  (let [first-hand-type (part-type-for-hand first-hand)
        second-hand-type (part-type-for-hand second-hand)]
    (if
      (= first-hand-type second-hand-type) (equal-type-hand-comparitor first-hand second-hand part-card-strength-scores)
      (> (hand-type-scores first-hand-type) (hand-type-scores second-hand-type)))))

(defn parse-hand-bid-line [line]
  (let [[hand bid] (str/split line #" ")]
    [hand (Integer/parseInt bid)]))

(defn maybe-upgrade-hand-type [hand]
  (let [card-counts (hand-card-counts hand)
        candidate-dictionary (dissoc card-counts \J)
        best-joker-candidate (first (apply max-key val (if (empty? candidate-dictionary) {\A 5} candidate-dictionary)))
        new-version-of-the-hand (str/replace hand \J best-joker-candidate)]
    (part-one-type-for-hand new-version-of-the-hand)))

(maybe-upgrade-hand-type "T55J5")
(maybe-upgrade-hand-type "KTJJT")
(maybe-upgrade-hand-type "QQQJA")

(defn part-two-type-for-hand [hand]
  (let [card-counts (hand-card-counts hand)]
    (if (> (or (card-counts \J) 0) 0)
      (maybe-upgrade-hand-type hand)
      ;; if there are no jokers, it's just a normal hand type check
      (part-one-type-for-hand hand))))

(defn solve [data part-card-strength-scores part-type-for-hand]
  (let [hands-and-bids (map parse-hand-bid-line data)
        sorted-hands-and-bids (sort (fn comparitor [[first-hand b1] [second-hand b2]]
                                      (hand-comparitor
                                       first-hand
                                       second-hand
                                       part-card-strength-scores
                                       part-type-for-hand)) hands-and-bids)]

    (reduce + (map-indexed (fn [i [hand bid]] (* bid (inc i))) (reverse sorted-hands-and-bids)))))

(defn part-one [data]
  (solve data part-one-card-strength-scores part-one-type-for-hand))

(defn part-two [data]
  (solve data part-two-card-strength-scores part-two-type-for-hand))
