
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw [
"#.##..##."
"..#.##.#."
"##......#"
"##......#"
"..#.##.#."
"..##..##."
"#.#.##.#."
""
"#...##..#"
"#....#..#"
"..##..###"
"#####.##."
"#####.##."
"..##..###"
"#....#..#"])

(def real-data-raw (str/split-lines (slurp "day-thirteen.txt")))

(defn convert-to-vectors [data] (mapv (fn [row] (mapv vec row)) data))

(defn read-data [data]
  (convert-to-vectors (filter (fn [records] (not= (count records) 1)) (partition-by #(= [] %) (map #(apply vector %) data)))))


(defn symmetrical-vertical? [data]
  (every? (fn [row] (let [split-index (/ (count (first data)) 2)
                          [first-half-row second-half-row] (split-at split-index row)]
                      (= (reverse first-half-row) second-half-row))) data))

(defn symmetrical-horizontal? [data]
  (let [split-index (/ (count data) 2)
        [first-half-rows second-half-rows] (split-at split-index data)]
    (= (reverse first-half-rows) second-half-rows)))

(assert (= (symmetrical-vertical? [[\# \. \. \#] [\# \# \# \#] [\# \. \. \#] [\. \# \# \.]]) true))
(assert (= (symmetrical-vertical? [[\# \. \. \#] [\# \# \# \#] [\# \. \. \#] [\. \# \# \#]]) false))
(assert (= (symmetrical-vertical? [[\# \# \# \#] [\# \. \. \#]]) true))
(assert (= (symmetrical-horizontal? [[\# \# \# \#] [\# \. \. \#] [\# \. \. \#] [\# \# \# \#]]) true))
(assert (= (symmetrical-horizontal? [[\# \# \# \#] [\# \# \. \#] [\# \. \. \#] [\# \# \# \#]]) false))

(def x [[\# \# \# \.] [\# \. \. \#] [\# \. \. \#] [\# \# \# \.]])

(defn horizontal-slices-indicies [data]
  (range 1 (count data)))

(defn vertical-slices-indicies [data]
  (range 1 (count (first data))))

(defn data-slice-for-vertical-index [data, index]
  (let [columns (count (first data))
        slice-width (min (- columns index) index)]
    (map (fn [row] (subvec row (- index slice-width) (+ index slice-width))) data)))

(defn data-slice-for-horizontal-index [data, index]
  (let [rows (count data)
        slice-height (min (- rows index) index)]
    (subvec data (- index slice-height) (+ index slice-height))))

(defn find-symmetry-point [data ignore]
  (let [horizontal-symmetrical (map (fn [index] [(* index 100) (symmetrical-horizontal? (data-slice-for-horizontal-index data index))]) (horizontal-slices-indicies data))
        vertical-symmetrical (map (fn [index] [index (symmetrical-vertical? (data-slice-for-vertical-index data index))]) (vertical-slices-indicies data))
        horizontal-symmetry (last (filter (fn [[value symmetrical]] (and symmetrical (not= value ignore))) horizontal-symmetrical))
        vertical-symmetry (last (filter (fn [[value symmetrical]] (and symmetrical (not= value ignore))) vertical-symmetrical))]
    ;; wasn't sure if multiple lines of symmetry were in the data, so I just added up the value for any lines I found.
    (reduce + (map (fn [[value symmetry]] value) (filter #(not (nil? %)) [vertical-symmetry horizontal-symmetry])))))

(defn mutate-mirror [data index]
  (let [columns (count (first data))
        row-index (int (/ index columns))
        column-index (mod index columns)
        current-char (get-in data [row-index column-index])
        new-char (if (= current-char \#) \. \#)]
    (assoc-in data [row-index column-index] new-char)))

;; working on the test data
(reduce + (map (fn [data] (find-symmetry-point data 0)) (read-data test-data-raw)))

;; real data - part one
(reduce + (map (fn [data] (find-symmetry-point data 0)) (read-data real-data-raw)))

;; Given a single mirror, run through all the variant mirrors and find one that
;; has a reflection that is different from the original mirror
(defn find-alternative-symmetry-point [data]
  (let [rows (count data)
        columns (count (first data))
        original-symmetry-point (find-symmetry-point data 0)
        other-symmetry-points (filter (fn [result] (> result 0)) (map (fn [index] (find-symmetry-point (mutate-mirror data index) original-symmetry-point)) (range 0 (* rows columns))))]
    (first other-symmetry-points)))

;; working on the test data
(reduce + (map find-alternative-symmetry-point (read-data test-data-raw)))

;; real data - part two
(reduce + (map find-alternative-symmetry-point (read-data real-data-raw)))
