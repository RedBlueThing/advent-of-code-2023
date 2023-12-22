
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["???.### 1,1,3"
                    ".??..??...?##. 1,1,3"
                    "?#?#?#?#?#?#?#? 1,3,1,6"
                    "????.#...#... 4,1,1"
                    "????.######..#####. 1,6,5"
                    "?###???????? 3,2,1"])

(def other-test-data  [ "?#?###???#??#?.??? 11,1,1"
".????#?#???#??????? 12,1,1"
"?#???##??#?????#.??? 2,11,1"
"#??##???????????. 1,11,1"
".??.?.?#?##?#???#?? 1,11"
"??#.?#???####??#??.? 1,11,1" ])
(def expected-other-test-data-part-one 31)

(def real-data-raw (str/split-lines (slurp "day-twelve.txt")))

(defn read-data-line [line]
  (let [[springs groups] (str/split line #" ")
        group-data (str/split groups #",")
        spring-unknowns (reduce (fn [unknowns [index chr]] (if (= chr \?)
                                                     (conj unknowns index)
                                                     unknowns
                                                     )) [] (map-indexed (fn [i v] [i v]) (char-array springs)))]
    [springs spring-unknowns (map (fn [str] (Integer/parseInt str)) group-data)]))

(defn read-data [data]
  (map read-data-line data))

(defn replace-at [s idx replacement]
  (str (subs s 0 idx) replacement (subs s (inc idx))))

(defn generate-custom-binary-sequences [digits [zero-char one-char]]
  (let [max-value (Math/pow 2 digits)
        binary-format (str "%" digits "s")
        replace-binary (fn [binary]
                         (-> binary
                             (clojure.string/replace " " (str zero-char))
                             (clojure.string/replace "0" (str zero-char))
                             (clojure.string/replace "1" (str one-char))))]
    (map (comp replace-binary #(format binary-format (Integer/toBinaryString %)))
         (range max-value))))

(defn spring-combinations [spring-counts]
  (generate-custom-binary-sequences spring-counts [\. \#]))

(defn all-arrangements [springs spring-unknowns]
  ;; for each of the spring unknown indexes cycle through \. and \#
  (let [combinations (spring-combinations (count spring-unknowns))]
    (map (fn apply-conbination [combination]
           (reduce (fn [new-springs [i unknown]]
                     (replace-at new-springs unknown (nth combination i)))
                   springs (map-indexed (fn [i v] [i v]) spring-unknowns))) combinations)))

(defn failed-spring-groups-for-arrangement [arrangement]
  ;; \# are failed springs. How are they grouped contiguously?
  (map count (filter #(= (first %) \#) (partition-by #(= % \#) arrangement))))

(defn valid-arrangement [arrangement groups]
  (= (failed-spring-groups-for-arrangement arrangement) groups))

(defn valid-arrangements [[springs spring-unknowns groups]]
  (let [arrangements (all-arrangements springs spring-unknowns)]
    (filter (fn [arrangement] (valid-arrangement arrangement groups)) arrangements)))

(defn valid-arrangements-count [[springs spring-unknowns groups]]
  (count (set (valid-arrangements [springs spring-unknowns groups]))))

(defn part-one [data]
  (reduce + (map valid-arrangements-count (read-data data))))
