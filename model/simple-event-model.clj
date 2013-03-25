(defn event [proc-id [[event-type & msg] event-id]] 
  {:process proc-id 
   :event-id (keyword (str "e" event-id)) 
   :type (condp = event-type
            :local   :local
            :send    [:send (first msg) (second msg)]
            :receive [:receive (first msg) (second msg)])})

(defn events [proc-id es]
  (let [es (mapcat #(if (number? %) (repeat % '(:local)) (list %)) es)
        es (map vector es (range))
        e  (partial event proc-id)]
    [proc-id (map e es)]))

(def dist1 
  (into {}
    [(events :p0 [1 '(:send :p1 :u1) 2])
     (events :p1 [4 '(:receive :p0 :u1)])]))  