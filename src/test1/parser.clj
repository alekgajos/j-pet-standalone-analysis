(ns test1.parser
  (:require [clojure.java.io :as io]
            [cheshire.core :as json]
            )
  (:use clojure.pprint)
  (:gen-class)
  )


(defn hexify [s]
  (format "%x" s))

(defn decode-header [datum tdc-setup]
  (let [tdc-no (bit-and datum 0xffff)]
    {:tdc-number tdc-no
     :internal-size
     (if (contains? tdc-setup (hexify tdc-no))
     (bit-shift-right datum 16)  ;; store actual no of records to read
       0 ;; do not read anything if the TDC does not exist in the setup
       )
     }
    )
  )

(defn decode-entry [datum]
  (let [header (bit-shift-right datum 29)]
    {:header header
     :payload
     (cond
       (= header 3) {:epoch (bit-and datum 0xfffffff)}
       (= header 4) {:channel (bit-and (bit-shift-right datum 22) 0x7f)
                     :coarse (bit-and datum 0x7ff)
                     :fine (bit-and (bit-shift-right datum 12) 0x3ff)
                     :is-rising (bit-and (bit-shift-right datum 11) 0x1)
                     }
       )
     }
    )
  )

(defn make-time [entry epoch channel-offset]
  (let [entry (:payload entry)]
    {:channel (+ (:channel entry)
                 (if (= (:channel entry) 0) 0 channel-offset)
                 )
     :time
     (+ (* (bit-shift-left epoch 11) 5.0)
        (/ (- (* (:coarse entry) 5000.) (* 10 (:fine entry))) 1000.)
        )
     }
    )
  )

(defn f [tdc-setup subevent-data datum]
  (if (> (:to-read subevent-data) 0)
    (let [entry (decode-entry datum)
          header (:header entry)
          ]
      (cond (= header 3)
            (assoc subevent-data
                   :to-read (dec (:to-read subevent-data))
                   :epoch (get-in entry [:payload :epoch])
                   )
            (= header 4)
            (assoc subevent-data
                   :to-read (dec (:to-read subevent-data))
                   :entries (conj (:entries subevent-data)
                                  (make-time entry
                                             (:epoch subevent-data)
                                             (get tdc-setup (hexify (:tdc-number subevent-data)))
                                             )
                                  )
                   )
            true
            (assoc subevent-data
                   :to-read (dec (:to-read subevent-data)))
            )
      )
    
    (let [header-data (decode-header datum tdc-setup)]
      (assoc subevent-data
             :to-read (:internal-size header-data)
             :tdc-number (:tdc-number header-data)
             )
      )
    )
  )


(defn make-times [data tdc-setup]

  (loop [absolute-times (:entries (reduce (partial f tdc-setup) {:to-read 0 :entries [] :tdc-number 0 :epoch 0} data))
         times-in-window []
         ref-time 0.
         ]
    (if (empty? absolute-times)
      times-in-window
      (let [head (first absolute-times)
            tail (rest absolute-times)
            ]
        (if (= (:channel head) 0)
          (recur tail times-in-window (:time head)) ;; proceed further with ref. time set
          (recur tail (conj times-in-window (assoc head :time (- (:time head) ref-time))) ref-time)
          )
        ) 
      )
    )
  )

(defn json-extract-setup [json]
  (nth (first json) 1)
  )

(defn make-tdcs-set [setup]
  (reduce (fn [tdcs-set tdc-entry]
            (conj tdcs-set [(:trbnet_address tdc-entry) (:channel_offset tdc-entry)])
            )
          {} (:tdc setup))
  )

(defn parse-setup-for-tdc-info [filename]
   (make-tdcs-set
     (json-extract-setup (json/parse-stream (io/reader filename) true))
    )  
  )

(defn -main [& args]
  ;; retrieve all necessary TDC info from the JSON setup file
  (pprint
   (parse-setup-for-tdc-info "../setup.json")
   )
  )

