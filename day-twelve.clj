
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

(def generate-custom-binary-sequences-memo (memoize generate-custom-binary-sequences))

(defn spring-combinations [spring-counts]
  (generate-custom-binary-sequences-memo spring-counts [\. \#]))

(defn all-arrangements [springs spring-unknowns]
  ;; for each of the spring unknown indexes cycle through \. and \#
  (let [combinations (spring-combinations (count spring-unknowns))]
    (map (fn apply-conbination [combination]
           (reduce (fn [new-springs [i unknown]]
                     (replace-at new-springs unknown (nth combination i)))
                   springs (map-indexed (fn [i v] [i v]) spring-unknowns))) combinations)))

(def all-arrangements-memo (memoize all-arrangements))

(defn failed-spring-groups-for-arrangement [arrangement]
  ;; \# are failed springs. How are they grouped contiguously?
  (map count (filter #(= (first %) \#) (partition-by #(= % \#) arrangement))))

(defn valid-arrangement [arrangement groups]
  (= (failed-spring-groups-for-arrangement arrangement) groups))

(defn valid-arrangements [[springs spring-unknowns groups]]
  (let [arrangements (all-arrangements-memo springs spring-unknowns)]
    (filter (fn [arrangement] (valid-arrangement arrangement groups)) arrangements)))

(defn valid-arrangements-count [[springs spring-unknowns groups]]
  (count (set (valid-arrangements [springs spring-unknowns groups]))))

(defn solve-v1 [data]
  (reduce + (map valid-arrangements-count (read-data data))))

(defn part-one [data]
  (solve-v1 data))

(defn update-for-part-two [line]
  (let [multiple 5
        [springs groups] (str/split line #" ")
        repeated-springs (take multiple (repeat springs))
        repeated-groups (take multiple (repeat groups))]
    (str (str/join "?" repeated-springs) " " (str/join "," repeated-groups))))

;; So, this row:

;; .# 1

;; Would become:

;; .#?.#?.#?.#?.# 1,1,1,1,1

(assert (= (update-for-part-two ".# 1") ".#?.#?.#?.#?.# 1,1,1,1,1"))

(defn options-for-spring-status [spring-status]
  (case spring-status
    ;; Operational
    \. [\.]
    ;; Fail
    \# [\#]
    ;; Unknown
    \? [\# \.]))

(defn valid-springs-given-groups [current-contiguous-failed-springs active-failed-spring-group remaining-groups]
  (if (empty? remaining-groups)
    ;; if there are no failed spring groups left, then we can only have operational springs
    #{\.}
    (if (not active-failed-spring-group)
      ;; if the current failed group isn't set, then we can have either type (an
      ;; operation spring won't change anything and a failed spring will start
      ;; the next group)
      #{\. \#}
      (if (< current-contiguous-failed-springs (first remaining-groups))
        ;; if the current contiguous failed springs is less than the current
        ;; groups maximum contiguous failed springs, then we can add a failed
        ;; spring and still be valid
        #{\#}
        ;; if the current contiguous-failed-springs is equal to the current
        ;; group, then all we can have is an operational spring next
        #{\.}))))

(defn recurse-valid-arrangements [springs current-contiguous-failed-springs active-failed-spring-group remaining-groups]
  (if (empty? springs)
    ;; we checked the entire string and there were no remaining groups we can return 1 (ie a valid path)
    (do
      ;; (println "We got to the end of a path and remaining groups were " remaining-groups " and contiguous-failed-springs was " current-contiguous-failed-springs)
      (if (or (empty? remaining-groups) (and (= (count remaining-groups) 1) (= (first remaining-groups) current-contiguous-failed-springs))) 1 0))
    (let [valid-springs (valid-springs-given-groups current-contiguous-failed-springs active-failed-spring-group remaining-groups)
          potential-springs-for-next-index (case (first springs)
                                             ;; Operational
                                             \. [\.]
                                             ;; Fail
                                             \# [\#]
                                             ;; Unknown
                                             \? [\# \.])
          available-springs-for-next-index (filter valid-springs potential-springs-for-next-index)]
      (if (empty? available-springs-for-next-index)
        ;; If there are no available springs to pick, then we can't continue down
        ;; this recursive path and just return zero
        0
        ;; otherwise we continue to recurse for each of the
        ;; available-springs-for-next-index
        (reduce + (map (fn [available-spring]
                         (case available-spring
                           ;; we are trying an operation spring, if previously
                           ;; there was an active-failed-spring-group we need to
                           ;; remove the previously active group
                           \. ((memoize recurse-valid-arrangements) (rest springs) 0 false (if active-failed-spring-group (rest remaining-groups) remaining-groups))
                           \# ((memoize recurse-valid-arrangements) (rest springs) (inc current-contiguous-failed-springs) true remaining-groups)))
                       available-springs-for-next-index))))))

(defn solve-last-v2 [data]
  (reduce + (map (fn [[springs spring-unknowns groups]] (recurse-valid-arrangements springs 0 false groups)) [(last (read-data data))])))

(defn solve-v2 [data]
  (reduce + (map (fn [[springs spring-unknowns groups]] (recurse-valid-arrangements springs 0 false groups)) (read-data data))))

(defn part-two [data]
  (solve-v2 (map update-for-part-two data)))
