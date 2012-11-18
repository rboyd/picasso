(ns picasso.ui
  (:import [processing.core PConstants])
  (:use quil.core))

(def screen-w 640)
(def screen-h 360)

(def xmag 0.0)
(def ymag 0.0)


(def front
  [[-1 -1 1]
   [-1 1 1]
   [1 1 1]
   [1 -1 1]])

(def back
  [[-1 -1 -1]
   [-1 1 -1]
   [1 1 -1]
   [1 -1 -1]])

(def right
  [[1 1 1]
   [1 1 -1]
   [1 -1 -1]
   [1 -1 1]])

(def left
  [[-1 1 1]
   [-1 1 -1]
   [-1 -1 -1]
   [-1 -1 1]])

(def up
  [[1 -1 -1]
   [-1 -1 -1]
   [-1 -1 1]
   [1 -1 1]])

(def down
  [[1 1 -1]
   [-1 1 -1]
   [-1 1 1]
   [1 1 1]])


(defn setup []
  (no-stroke)
  (color-mode :rgb 1))


(def s 40)

(defn draw-face [vertices color]
  (if color
    (do
      (apply fill color)
       (doseq [v vertices]
         (apply vertex v)))))

(defn draw-cubie [cubie]
  (let [x (+ (* s (:x cubie) 2) (/ screen-w 2))
        y (+ (* s (:y cubie) 2) (/ screen-h 2))
        z (* s (:z cubie) 2)]


  (push-matrix)

  (translate x y z)
  (scale s)
  (begin-shape :quads)

  (draw-face up (:u cubie))
  (draw-face down (:d cubie))
  (draw-face front (:f cubie))
  (draw-face right (:r cubie))
  (draw-face left (:l cubie))
  (draw-face back (:b cubie))

 (end-shape)


  (pop-matrix))
)



(defn draw-cube [cube]

  (doseq [cubie cube]
   (draw-cubie cubie))
)

(defn rand-color []
  (vec (repeatedly 3 #(rand))))

(def cube [{:x -1 :y 0 :z 0 :u [1 0 0] :f [0 0 1] :d [0 1 1] :b [1 1 0] :l (rand-color) :r (rand-color)}
           {:x 0  :y 0 :z 0 :f [0 1 0] :u [1 1 1] :d [0 1 1] :b [1 1 0] :l (rand-color) :r (rand-color)}
           {:x 1  :y 0 :z 0 :f (rand-color) } ])

(defn random-cubie [x y z]
  {:x x :y y :z z :u (rand-color) :d (rand-color) :f (rand-color) :b (rand-color) :l (rand-color) :r (rand-color)})

(defn random-cube []
  (for [x (range -1 2)
        y (range -1 2)
        z (range -1 2)]
    (random-cubie x y z)))

(def cube-white [1 1 1])
(def cube-yellow [1 1 0])
(def cube-red [1 0 0])
(def cube-orange [1 0.4 1])
(def cube-blue [0 0 1])
(def cube-green [0.12 0.75 0.24])

(defn add-color [cubie [key color cond]]
  (if cond
    (merge cubie {key color})
    cubie))

(defn solved-cubie [x y z]
  (let [blank-cubie {:x x :y y :z z}]
   (reduce add-color blank-cubie [
    [:u cube-white (= y -1)]
    [:d cube-yellow (= y 1)]
    [:l cube-red (= x -1)]
    [:r cube-orange (= x 1)]
    [:f cube-blue (= z 1)]
    [:b cube-green (= z -1)]])))


(defn solved-cube []
  (for [x (range -1 2)
        y (range -1 2)
        z (range -1 2)]
    (solved-cubie x y z)))

(def cube (solved-cube))

(defn draw []
  (background 0)

  (push-matrix)

  (let [new-xmag (* (/ (mouse-x) screen-w) PConstants/TWO_PI)
        new-ymag (* (/ (mouse-y) screen-h) PConstants/TWO_PI)
        xmag-diff (- xmag new-xmag)
        ymag-diff (- ymag new-ymag)]
    (if (> (Math/abs xmag-diff) 0.01)
      (rotate-y (- xmag (/ xmag-diff 2.0)))
      (rotate-y (- xmag)))
    (if (> (Math/abs ymag-diff) 0.01)
      (rotate-x (- ymag (/ ymag-diff 2.0)))
      (rotate-x (- ymag))))



  (draw-cube cube)





  (pop-matrix)


  )

(defsketch main
  :title "rgb cube"
  :setup setup
  :size [screen-w screen-h]
  :draw draw
  :renderer :opengl)
