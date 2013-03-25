(defn e 
  ([proc start] (e proc start start))
  ([proc start finish] (map (fn [s] [proc (keyword (str "e" s))]) (range start (inc finish)))))

(defn msg 
  ([[e] t c] [[e t c]])
  ([[e] t c [m]] [[e t c m]]))

(defn events [proc es]
  [proc (apply concat es)])


(def no-msg-log
  (into {} [
    (events :p0 [(e :p0 0 3)])
    (events :p1 [(e :p1 0 2)]) ]))
(def msg-log
  (into {} [
    (events :p0 [(msg (e :p0 0) :send :p1)              (msg (e :p0 1) :receive :p1 (e :p1 1))])
    (events :p1 [(msg (e :p1 0) :receive :p0 (e :p0 0)) (msg (e :p1 1) :send :p0)]) ]))
(def simple-log 
  (into {} [
    (events :p0 [(e :p0 0 3) (msg (e :p0 4) :send :p1) (e :p0 5) (msg (e :p0 6) :receive :p1 (e :p1 5))])
    (events :p1 [(e :p1 0) (msg (e :p1 1) :receive :p0 (e :p0 4)) (e :p1 2 4)]) ]))



(defn receive? [e]
  (and
    (coll? e)
    (not (empty? (filter #(= :receive %) e))) ))

(defn send-receive-pair [[[p e] t c m :as rec]]
  [[m :send p] rec])

(defn expected-send-receive-msgs [dist]
  (mapcat (fn [[a b]] (map send-receive-pair (filter receive? b))) dist))

(defn missing-send-msgs [dist]
  (let [expected (expected-send-receive-msgs dist)
        msg-sets (into {} (map (fn [[a b]] [a (set b)]) dist))]
    (filter (fn [[[[p e] t c :as s] r]] (not (contains? (p msg-sets) s))) expected)))

(defn missing-send-msgs? [dist]
  (not (empty? (missing-send-msgs dist))))



(defn head-lists 
  ([es] (head-lists (reverse es) []))
  ([es as]
    (if (empty? es) 
      as
      (recur (rest es) (conj as (reverse es))))))

(defn keyed-base-counter 
  ([sets n] 
    (let [procs (keys sets)] 
      (keyed-base-counter procs sets (into {} (map (fn [proc] [proc (count (proc sets))]) procs)) n {})))
  ([procs sets bases n result]
    (let [this-base (first procs)
          rest-bases (dissoc bases this-base)]
      (if (empty? rest-bases) 
        (conj result [this-base (nth (this-base sets) n)])
        (let [factor (apply * (vals rest-bases))
              this-item (conj result [this-base (nth (this-base sets) (quot n factor))])]
          (recur (rest procs) sets rest-bases (rem n factor) this-item))))))

(defn log-enumerator [dist n]
  (keyed-base-counter (into {} (map (fn [[a b]] [a (head-lists b)]) dist)) n))

(defn consistant-logs [dist] 
  (let [ordinals (range (apply * (map count (vals dist))))]
    (filter #(not (missing-send-msgs? %)) (map #(log-enumerator dist %) ordinals))))
