(ns picasso.cube)

(defn init-row [color]
  (take 3 (repeat color)))

(defn init-face [color]
  (take 3 (repeat (init-row color))))

(defn cube-setup []
  (let [colors [:w :r :g :y :b :o]]
    (map init-face colors)))
