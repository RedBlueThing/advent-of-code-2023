
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["0 3 6 9 12 15"
                    "1 3 6 10 15 21"
                    "10 13 16 21 30 45"])

(def real-data-raw (str/split-lines (slurp "day-nine.txt")))

(defn parse-line [line]
  (map #(Integer/parseInt %) (str/split line #" ")))

(defn gaps-for-sequence [sequence]
  (map-indexed (fn [i value]
         (let [next-value (nth sequence (inc i))]
           (- next-value value)
           )
         )
       (butlast sequence)))

(defn sequence-resolved [sequence]
  (= (count (set sequence)) 1))

(defn propagate [derived-sequences value val-pos-fn val-fn]
  (if (= (count derived-sequences) 0)
    value
    (propagate (butlast derived-sequences) (val-fn (val-pos-fn (last derived-sequences)) value) val-pos-fn val-fn)))

(defn get-value [sequence val-pos-fn val-fn]
  (loop [derived-sequences []
         current-sequence sequence]
    (let [gaps-for-current-sequence (gaps-for-sequence current-sequence)]
      (if (sequence-resolved gaps-for-current-sequence)
        ;; we are done, time to work out the next value for all the derived sequences
        (propagate (conj derived-sequences current-sequence) (val-pos-fn gaps-for-current-sequence) val-pos-fn val-fn)
        ;; recurse in with the gaps we just got
        (recur (conj derived-sequences current-sequence) gaps-for-current-sequence)))))

(defn part-one [data]
  (let [sequence-data (map parse-line data)]
    (reduce + (map (fn [sequence] (get-value sequence last +)) sequence-data))))

(defn part-two [data]
  (let [sequence-data (map parse-line data)]
    (reduce + (map (fn [sequence] (get-value sequence first -)) sequence-data))))



