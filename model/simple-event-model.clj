(defn e 
  ([proc start] (e proc start start))
  ([proc start finish] (map (fn [s] [proc (keyword (str "e" s))]) (range start (inc finish)))))

(defn msg 
  ([[e] t c] [[e t c]])
  ([[e] t c [m]] [[e t c m]]))

(defn events [proc es]
  [proc (apply concat es)])

(defn receive? [e]
  (and
    (coll? e)
    (not (empty? (filter #(= :receive %) e))) ))

(defn send-receive-pair [[[p e] t c m :as rec]]
  [[m :send p] rec])

(defn expected-send-receive-msgs [dist]
  (mapcat (fn [[a b]] (map send-receive-pair (filter receive? b))) dist))

(defn sender [[[[p e] t c :as s] r]] p)

(defn missing-send-msgs [dist]
  (let [expected (expected-send-receive-msgs dist)
        msg-sets (into {} (map (fn [[a b]] [a (set b)]) dist))]
    (filter (fn [[[[p e] t c :as s] r]] (not (contains? (p msg-sets) s))) expected)))

(defn missing-send-msgs? [dist]
  (not (empty? (missing-send-msgs dist))))


(def simple-log 
  (into {} [
    (events :p0 [(e :p0 0 3) (msg (e :p0 4) :send :p1) (e :p0 5) (msg (e :p0 6) :receive :p1 (e :p1 5))])
    (events :p1 [(e :p1 0) (msg (e :p1 1) :receive :p0 (e :p0 4)) (e :p1 2 4)])]))