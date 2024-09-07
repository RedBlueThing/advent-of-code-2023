
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

(defn parse-item [item]
  (let [[_ text symbol number] (re-matches #"([a-z]+)(-|=)(\d*)?" item)]
    {:label text
     :box-number (calculate-hash text)
     :symbol (first symbol)
     :focal-length (if (empty? number) nil (Integer/parseInt number))}))

(defn index-matching-label [box label]
  (let [{slots :slots} box
        filtered-slots (keep-indexed (fn [index slot] (let [{slot-label :label} slot]
                                                        (when (= label slot-label) index))) slots)]
    (first filtered-slots)))

(defn equal-operation-to-box [box operation]
  ;; If there is already a lens in the box with the same label, replace the old
  ;; lens with the new lens: remove the old lens and put the new lens in its
  ;; place, not moving any other lens in the box.
  ;;
  ;; If there is not already a lens in the box with the same label, add the lens
  ;; to the box immediately behind any lens already in the box. Don't move any
  ;; of the other lens when you do this. If there aren't any lens in the
  ;; box, the new lens goes all the way to the front of the box.
  (let [{label :label focal-length :focal-length} operation
        {slots :slots} box
        slot-index-of-matching-label (index-matching-label box label)]
    (assoc box :slots
           (if (nil? slot-index-of-matching-label)
              ;; if the label doesn't exist in the box, just add the lens
             (conj slots {:focal-length focal-length :label label})
              ;; if the label does exists, replace the lens
             (assoc slots slot-index-of-matching-label {:focal-length focal-length :label label})))))

(defn drop-nth [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll))

(defn minus-operation-to-box [box operation]
  ;; If the operation character is a dash (-), go to the relevant box and remove
  ;; the lens with the given label if it is present in the box. Then, move any
  ;; remaining lens as far forward in the box as they can go without changing
  ;; their order, filling any space made by removing the indicated lens. (If no
  ;; lens in that box has the given label, nothing happens.)
  (let [{label :label} operation
        {slots :slots} box
        slot-index-of-matching-label (index-matching-label box label)]
    (assoc box :slots
           (if (nil? slot-index-of-matching-label)
              ;; if a lens with the label isn't in the box, do nothing
             slots
              ;; otherwise remove that existing lens
             (vec (drop-nth slot-index-of-matching-label slots))))))

(defn operation-reducer [boxes operation]
  (let [{box-number :box-number symbol :symbol} operation
        box (nth boxes box-number)]
    (case symbol
      \= (assoc boxes box-number (equal-operation-to-box box operation))
      \- (assoc boxes box-number (minus-operation-to-box box operation)))))

(defn process-operations [operations]
  (let [boxes (vec (map (fn [box-number] {:box-number box-number :slots []}) (range 0 256)))]
    (reduce operation-reducer boxes operations)))

(defn focusing-power [box]
  ;; The focusing power of a single lens is the result of multiplying together:
  ;;
  ;; One plus the box number of the lens in question.
  ;; The slot number of the lens within the box: 1 for the first lens, 2 for the second lens, and so on.
  ;; The focal length of the lens.
  (let [{box-number :box-number slots :slots} box]
    ;; add up the focusing power of each lens
    (reduce + (map-indexed (fn [index slot]
                     (let [{focal-length :focal-length} slot]
                       (* (inc box-number) (inc index) focal-length))) slots))))

;; ot: 4 (box 3) * 1 (first slot) * 7 (focal length) = 28
;; ab: 4 (box 3) * 2 (second slot) * 5 (focal length) = 40
;; pc: 4 (box 3) * 3 (third slot) * 6 (focal length) = 72
(assert (= (focusing-power {:box-number 3 :slots [{:focal-length 7} {:focal-length 5} {:focal-length 6}]}) 140))

(defn part-two [data]
  (let [boxes (process-operations (map parse-item (read-data data)))]
    (reduce + (map focusing-power boxes))))
