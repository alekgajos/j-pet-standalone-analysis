(ns test1.reader
  (:require [org.clojars.smee.binary.core :as b]
            [clojure.java.io :as io]
            [test1.parser :as p])
  (:import org.clojars.smee.binary.core.BinaryIO
           java.io.DataInput)
  (:use clojure.pprint)
  (:gen-class)
  )

(def ^:dynamic *remaining-bytes* 0)

(defn align8 [size]
  (* (+ (quot (- size 1) 8) 1) 8)
  )

(defn n-bytes [len]
  (b/repeated :byte :length len))

(def hld-file-header
  (b/ordered-map
   :magic-number (n-bytes 32)))

(def event-header
  (b/ordered-map
   :full-size :uint-le
   :decoding :uint-le
   :id :uint-le
   :seq-nr :uint-le
   :date :uint-le
   :time :uint-le
   :run-nr :uint-le
   :pad :uint-le
   )
  )

(def sub-event-header
  (b/ordered-map
   :size :uint-be
   :decoding :uint-be
   :hub-addr :uint-be
   :trg-nr :uint-be
   )
  )

(def sub-event-codec
  (b/ordered-map
   :sub-event (b/header
               sub-event-header
               (fn [{:keys [size]}]
                 (def ^:dynamic *remaining-bytes* (- *remaining-bytes* (align8 size)))
                 
                 (b/ordered-map
                  :this-subevent (b/align 
                                  (b/repeated :uint-be :length (/ (- size 16) 4))
                                  :modulo 8
                                  )
                  :rest (if (> *remaining-bytes* 0) sub-event-codec (b/compile-codec {}))
                  )                 
                 )
               nil
               )
   )
  )

(def event-codec
  (b/ordered-map
   :event (b/header
           event-header
           (fn [{:keys [full-size]}]
;;             (println "full-size" full-size)
             (def ^:dynamic *remaining-bytes* (- full-size 32))
             (b/compile-codec
               sub-event-codec
              )
             )
           nil
           )
   )
  )

(defn extract-payload [event-object]
  (loop [tdc-data []
         subevent (:sub-event (:event event-object))
         ]
    (if (nil? subevent)
      tdc-data
      (recur (into tdc-data (:this-subevent subevent)) (:sub-event (:rest subevent)))
      )
    )
  )

(defn decode-hld
  ;; for reading first no-events events from the file
  ([filename tdc-setup no-events]
  "Reads events from a hld file"
   (let [in (io/input-stream filename)
         post-action (if (>= no-events 0)
                       (fn [n] (dec n))
                       (fn [n] 1) ;; for infinite looping
                       )
         ]
    (b/decode hld-file-header in)
    (try

      (loop [n (post-action no-events)]
        (if (>= n 0)
          (do
            (pprint
             (p/make-times (extract-payload (b/decode event-codec in)) tdc-setup)
             )
            (recur (post-action n))
            )
          nil
          )
        )
      
      (catch java.io.EOFException e
        (println "Done.")
        )
      )
    )
  )
  ;; version for reading all events in the file
  ([filename tdc-setup]
  (decode-hld filename -1)
   )
  )

;;(def filename "../dabc_18243160558.hld")
(def filename "../small.hld")

(defn -main [& args]
  (let [tdcs (p/parse-setup-for-tdc-info "../setup.json")]
    (decode-hld filename tdcs 2)
    )
  )
