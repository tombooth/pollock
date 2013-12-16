(ns pollock.output.debug.mouse-camera
  [:require [quil.core :as q]
            [clojure.pprint :as pp]])


(def mouse-look-centre (atom {}))
(def camera-settings (atom {
  :eye-x 980
  :eye-y 580
  :eye-z 930
  :center-x 0
  :center-y 0
  :center-z 0
  :up-x 0
  :up-y -1
  :up-z 0
}))

(defn mouse-pressed []
  (swap! mouse-look-centre assoc :x (q/mouse-x) :y (q/mouse-y)))

(defn- look-pan [x y x-diff y-diff]
  (swap!
    camera-settings
    (fn [{:keys [eye-x eye-y] :as settings}]
      (assoc settings
        :eye-x (+ eye-x x-diff)
        :eye-y (+ eye-y y-diff)))))

(defn- look-tilt [x y x-diff y-diff]
  (print "tilt" x y x-diff y-diff))

(defn- look-zoom [x y x-diff y-diff]
  (swap!
    camera-settings
    (fn [{:keys [eye-z] :as settings}]
      (assoc settings :eye-z (+ eye-z y-diff)))))


(def button-mapping {
  :left look-zoom
  :right look-pan
  :center look-zoom
})

(defn mouse-dragged []
  (let [look-centre @mouse-look-centre
        x (q/mouse-x)
        y (q/mouse-y)
        x-diff (- (q/pmouse-x) x)
        y-diff (- (q/pmouse-y) y)]
    ((button-mapping (q/mouse-button)) x y x-diff y-diff)))

(defn set-camera! [ex ey ez cx cy cz ux uy uz]
  (swap! camera-settings assoc
    :eye-x ex :eye-y ey :eye-z ez :center-x cx :center-y cy :center-z cz :up-x ux :up-y uy :up-z uz))


(defn move-camera []
  (let [settings @camera-settings
        {ex :eye-x ey :eye-y ez :eye-z cx :center-x cy :center-y cz :center-z ux :up-x uy :up-y uz :up-z} settings]
    (q/camera ex ey ez cx cy cz ux uy uz)))

