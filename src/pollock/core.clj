(ns pollock.core
  (:gen-class)
  [:require [clojure.tools.cli :refer [cli]]
            [cheshire.core :as json]
            [pollock.out.debug :as debug-output]
            [pollock.out.image :as image-output]
            [pollock.util :as util]])

(def desired-dpi 300)
(def actual-dpi 72)

(def desired-dpcm (quot desired-dpi 2.54))
(def actual-dpcm (quot actual-dpi 2.54))
(def dpcm-scaling (/ desired-dpi actual-dpi))

(def width-cm 10)
(def depth-cm 10)
(def height-cm 5)

(defn to-pixels [cm] (int (* (* cm actual-dpcm) dpcm-scaling)))

(def width (to-pixels width-cm))
(def depth (to-pixels depth-cm))
(def height (to-pixels height-cm))

(def default-config
  {
   :dimensions { :width width :height height :depth depth }

   :colors { :background [255 255 255] }
   
   ;; gravity is -980cms-2 and we need to convert this to pixels
   :gravity [0 (* -980 actual-dpcm) 0]

   ;; when working out the impacts we need to convert the arbritary paint
   ;; unit to a mass and we use this multiply it by this number to get a mass
   :mass-per-unit 1.0

   ;; this is the 'flow rate' of the paint. this translates as how much
   ;; paint each point along the stroke recieves as function of the
   ;; initial amount. this is initial * (1 / [1 + (point_num * flow-rate)])
   :flow-rate 0.08

   :splatter
   {
    ;; splatter is setup so that any impact points with a speed higher
    ;; than the percentile set here will generate splatter
    :percentile 90

    ;; even if the impact falls within the above percentile we do not want
    ;; it to always cause splitter, so (rand) is compared to this value
    ;; and if it is less then splatter is caused.
    :likelihood 0.5

    ;; this is what the reflected splatted velocity vector will be
    ;; multiplied with during splatter calculation. it should be in
    ;; between 0.0 and 1.0. it is 3.0 at the moment because gravity is
    ;; super strong and i'm not sure how i want to tweak all params.
    :velocity-dampening 3.0

    ;; when an impact point emits splatter to work out the amount of paint
    ;; in the splatter we multiply the impact's amount of paint with this constant.
    :paint-dampening 0.5
   }
   })

(defn- slurp-config [paths]
  (map (fn [path]
         (let [string (slurp path)]
           (json/parse-string string true)))
       paths))

(defn- start [cli-options config]
  (if (:debug cli-options)
    (debug-output/start (:num cli-options) config)
    (image-output/start (:num cli-options) (:output cli-options) config)))

(defn -main [& cli-args]
  (let [[options args banner]
        (cli cli-args
             ["-h" "--help" "Help." :flag true :default false]
             ["-d" "--debug" "Debug mode." :flag true :default false]
             ["-n" "--num" "Number of strokes." :default 10
                                                :parse-fn #(Integer. %)]
             ["-s" "--seed" "Random seed" :default (System/currentTimeMillis)
                                          :parse-fn #(Long. %)]
             ["-o" "--output" "Output path." :default "pollock.png"])]
    
    (when (:help options)
      (println banner)
      (System/exit 0))

    (println "Current run seed:" (:seed options))
    (util/set-seed (:seed options))
    
    (if (> (count args) 0)
      (let [slurped-config (slurp-config args)
            merged-config (apply util/deep-merge
                                 (conj slurped-config default-config))]
        (start options merged-config))
      (start options default-config))))
