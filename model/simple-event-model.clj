(defn e 
  ([start] (e start start))
  ([start finish] (map #(keyword (str "e" %)) (range start (inc finish)))))

(defn msg 
  ([[e] t c] [[e t c]])
  ([[e] t c [m]] [[e t c m]]))

(defn events [proc-id es]
  [proc-id (apply concat es)])

(defn receive? [e]
  (and
    (coll? e)
    (not (empty? (filter #(= :rec %) e))) ))

(def dist2 
  (into {} [
    (events :p0 [(e 0 1) (msg (e 2) :send :p1) (e 3)])
    (events :p1 [(e 0) (msg (e 1) :rec :p0 (e 2)) (e 2 4)])]))