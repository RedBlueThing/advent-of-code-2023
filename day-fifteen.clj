
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"])

(def real-data-raw (str/split-lines (slurp "day-fifteen.txt")))

(defn read-data [raw-data]
  (let [the-line (first raw-data)]
    (str/split the-line #",")))

(defn calculate-hash [s]
  ;; Determine the ASCII code for the current character of the string.
  ;; Increase the current value by the ASCII code you just determined.
  ;; Set the current value to itself multiplied by 17.
  ;; Set the current value to the remainder of dividing itself by 256.
  (reduce (fn [current-value ascii-value] (mod (* 17 (+ current-value ascii-value)) 256)) 0 (map int s)))

(defn part-one [data]
   (reduce + (map calculate-hash data)))
