(ns pollock.entry
  (:gen-class)
  [:require [clojure.tools.cli :refer [cli]]
            [cheshire.core :as json]
            [docopt.core :as dc]
            [docopt.match :as dm]
            [pollock.output.debug :as debug-output]
            [pollock.output.image :as image-output]
            [pollock.util :as util]])

(def default-config
  {
   :seed (System/currentTimeMillis)
   
   :num-strokes 10
   
   :dimensions { :width 1166 :height 583 :depth 1166 }

   :colors { :background [255 255 255] }
   
   :stroke-length { :max 291 :min 150 }
   
   ;; gravity measured in pixels per second per second
   :gravity [0 -10000 0]

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
        (let [string (slurp (if (= path "-") *in* path))]
          (json/parse-string string true)))
       paths))

(defn- start [debug output config]
  (if debug
    (debug-output/start config)
    (image-output/start output config)))

(defn- to-cli-config [num seed]
  (let [cli-config (transient {})]
    (if (not (nil? num)) (assoc! cli-config :num-strokes (Integer/parseInt num)))
    (if (not (nil? seed)) (assoc! cli-config :seed (Long/parseLong seed)))
    (persistent! cli-config)))

(def usage-string "Pollock

Usage:
  pollock [options] [<config>]...

Options:
  -h --help            Show this screen.
  -v --version         Show this version
  --debug              Run in debug mode (3D window).
  --num-strokes=<num>  Number of strokes.
  --seed=<seed>        Random seed.
  --output=<path>      Output path [default:./pollock.png].")

(def version "Pollock 0.1.0")

(defn -main [& args]

  (let [arg-map      (dm/match-argv (dc/parse usage-string) args)
        debug        (arg-map "--debug")
        raw-seed     (arg-map "--seed")
        raw-num      (arg-map "--num-strokes")
        output       (arg-map "--output")
        config-files (arg-map "<config>")]

    (cond
     (or (nil? arg-map)
         (arg-map "--help")) (println usage-string)

     (arg-map "--version")   (println version)

     :else (let [slurped-config (slurp-config config-files)
                 file-config    (apply util/deep-merge
                                       (conj slurped-config default-config))
                 merged-config  (merge file-config (to-cli-config raw-num raw-seed))
                 seed (:seed merged-config)]

             (println "Version:" version)
             
             (if-let [build-info-resource (clojure.java.io/resource "build.info")]
               (let [build-info (slurp build-info-resource)]
                 (print build-info)))

             (println (json/generate-string merged-config {:pretty true}))
             
             (util/set-seed seed)
             (start debug output merged-config)))))

