(ns picasso.ui
  (:import [processing.core PConstants])
  (:use [quil.core]
        [tween-clj.core :exclude [constrain]]))

(def screen-w 640)
(def screen-h 360)

(def xmag 0.0)
(def ymag 0.0)


(defn current-time []
 (mod (System/currentTimeMillis) 10000))

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

(def cube-white [1 1 1])
(def cube-yellow [1 1 0])
(def cube-red [1 0 0])
(def cube-orange [1 0.2705 0])
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

(def remaining-moves (atom []))

(defn setup []
  (camera 0  0  600
          0 0 -5000
          0 1 0)

  (def remaining-moves (atom [:r :r :r :r :r :r]))
  (def cube (atom (solved-cube)))

  (no-stroke)
  (color-mode :rgb 1))


(def SCALE 40)

(defn draw-face [vertices color]
  (if color
    (do
      (apply fill color)
       (doseq [v vertices]
         (apply vertex v)))))

(defn draw-cubie [cubie]
  (let [x (* SCALE (:x cubie) 2)
        y (* SCALE (:y cubie) 2)
        z (* SCALE (:z cubie) 2)]
    (push-matrix)
    (translate x y z)
    (scale 38)
    (begin-shape :quads)
    (draw-face up (:u cubie))
    (draw-face down (:d cubie))
    (draw-face front (:f cubie))
    (draw-face right (:r cubie))
    (draw-face left (:l cubie))
    (draw-face back (:b cubie))
    (end-shape)
    (pop-matrix)))

(defn cubies-from-move []
  (case (first @remaining-moves)
    :u #(= (:y %) -1)
    :d #(= (:y %) 1)
    :l #(= (:x %) -1)
    :r #(= (:x %) 1)
    :f #(= (:z %) 1)
    :b #(= (:z %) -1)
    #(not (= %))))

(defn active [cube]
  (filter (cubies-from-move) cube))


(defn draw-cube [cube]

  (doseq [cubie (filter #(not (contains? (set (active cube)) %)) cube)]
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

(defn rotation-from-move []
  (case (first @remaining-moves)
    :u [0 -1 0]
    :d [0 1 0]
    :l [-1 0 0]
    :r [1 0 0]
    :f [0 0 1]
    :b [0 0 -1]
    [0 0 0]))

(def last-time (atom (System/currentTimeMillis)))

(defn move-completed? []
  (if (>= (- (System/currentTimeMillis) @last-time) 1000)
    (reset! last-time (System/currentTimeMillis))
    false))

(def MOVE_TIME 1500)

(defn rotate-u [cubie]
  (if (= (:y cubie) -1)
    { :x (- (:z cubie)) :y -1 :z (:x cubie) :f (:r cubie) :l (:f cubie) :b (:l cubie) :r (:b cubie) :u (:u cubie) }
    cubie))

(defn rotate-d [cubie]
  (if (= (:y cubie) 1)
    { :x (:z cubie) :y 1 :z (- (:x cubie)) :f (:l cubie) :l (:b cubie) :b (:r cubie) :r (:f cubie) :d (:d cubie) }
    cubie))

(defn rotate-r [cubie]
  (if (= (:x cubie) 1)
    { :x 1 :y (- (:z cubie)) :z (:y cubie) :f (:d cubie) :u (:f cubie) :b (:u cubie) :d (:b cubie) :r (:r cubie) }
    cubie))

(defn rotate-l [cubie]
  (if (= (:x cubie) -1)
    { :x -1 :y (:z cubie) :z (- (:y cubie)) :f (:u cubie) :u (:b cubie) :b (:d cubie) :d (:f cubie) :l (:l cubie) }
    cubie))

(defn rotate-f [cubie]
  (if (= (:z cubie) 1)
    { :x (- (:y cubie)) :y (:x cubie) :z 1 :f (:f cubie) :u (:l cubie) :r (:u cubie) :d (:r cubie) :l (:d cubie) }
    cubie))

(defn rotate-b [cubie]
  (if (= (:z cubie) -1)
    { :x (:y cubie) :y (- (:x cubie)) :z -1 :b (:b cubie) :u (:r cubie) :r (:d cubie) :d (:l cubie) :l (:u cubie) }
    cubie))





(defn find-func [move]
  (ns-resolve 'picasso.ui (symbol (str 'rotate- (name move)))))

(defn repaint! []
  (let [move (first @remaining-moves)]
    (case move
;      (:u :d) (reset! cube (map rotate-u @cube))
      (:u :d :r :l :f :b) (reset! cube (map (find-func move) @cube))
      nil)))

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


  (push-matrix)
  (doseq [cubie (active @cube)]

   (with-rotation (concat [(* Math/PI 0.5 (ease-out transition-back (/ (- (System/currentTimeMillis) @last-time) MOVE_TIME)))] (rotation-from-move))
     (with-translation [0 0 0]
       (draw-cubie cubie))))

  (pop-matrix)
  (draw-cube @cube)
  (pop-matrix)

  (if (move-completed?)
    (do
      (repaint!)
      (reset! remaining-moves (shuffle [:u :d :l :r :f :b]))
      (swap! remaining-moves rest))))

(defsketch main
  :title "rgb cube"
  :setup setup
  :size [screen-w screen-h]
  :draw draw
  :renderer :opengl)
