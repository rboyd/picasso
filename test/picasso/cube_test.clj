(ns picasso.cube-test
  (:use picasso.cube lazytest.describe))

(def solved-cube
  [[[:w :w :w]
    [:w :w :w]
    [:w :w :w]]

   [[:r :r :r]
    [:r :r :r]
    [:r :r :r]]

   [[:g :g :g]
    [:g :g :g]
    [:g :g :g]]

   [[:y :y :y]
    [:y :y :y]
    [:y :y :y]]

   [[:b :b :b]
    [:b :b :b]
    [:b :b :b]]

   [[:o :o :o]
    [:o :o :o]
    [:o :o :o]]])

(describe "Cube setup"
  (it "initializes a solved cube"
    (= (cube-setup) solved-cube)))

(describe "init-row"
  (it "initializes a row"
    (= (init-row :w) [:w :w :w])))
