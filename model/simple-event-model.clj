(defn event [proc-id [[event-type & event-partner] event-id]] 
  {:process proc-id 
   :event-id event-id 
   :type (condp = event-type
            :local   :local
            :send    [:send (first event-partner)]
            :receive [:receive (first event-partner)])})

(defn events [proc-id es]
  (let [es (mapcat #(if (number? %) (repeat % '(:local)) (list %)) es)
        es (map vector es (range))
        e  (partial event proc-id)]
    [proc-id (map e es)]))

(def dist1 
  (into {}
    [(events :p0 [1 '(:send :p1) 2])
     (events :p1 [4 '(:receive :p0)])]))  