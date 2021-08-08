(ns snek-clj.core
  (:import (java.awt Color)
           (javax.swing JPanel JFrame)
           (java.awt.event KeyListener)
           (java.lang Thread))
  (:gen-class))

;; TODO: make apples never generate in snake
;;       make all state atomic
;;       handle boundaries
;;       make apples never spawn next to border
;;       speed up game with time
;;       polish UI
;;       center game in frame
;;       refactor to be more functional?

(def VK_LEFT (java.awt.event.KeyEvent/VK_LEFT))
(def VK_RIGHT (java.awt.event.KeyEvent/VK_RIGHT))
(def VK_UP (java.awt.event.KeyEvent/VK_UP))
(def VK_DOWN (java.awt.event.KeyEvent/VK_DOWN))
(def VK_SPACE (java.awt.event.KeyEvent/VK_SPACE))
(def VK_H (java.awt.event.KeyEvent/VK_H))
(def VK_J (java.awt.event.KeyEvent/VK_J))
(def VK_K (java.awt.event.KeyEvent/VK_K))
(def VK_L (java.awt.event.KeyEvent/VK_L))

(def block-size 8)
(def direction (atom :right))
(def speed (atom 100))
(def paused? (atom true))
;; TODO: change this to an atom
(def snek-blocks (atom [[16 16] [24 16] [32 16] [40 16]]))
(def grow-snek? (atom false))
;; TODO: change this to an atom
(def apple-blocks (atom #{}))
(def allow-keypress? (atom true))
(def apple-odds 25)
(def max-apples 8)
(def max-apple-loc 38)
(def score (atom 0))

(defn random-apple-loc
  []
  [(* block-size (rand-int max-apple-loc)) (* block-size (rand-int max-apple-loc))])

(defn reset-game-state
  []
  (reset! score 0)
  (reset! allow-keypress? true)
  (reset! paused? true)
  (reset! direction :right)
  (reset! apple-blocks (hash-set (random-apple-loc) (random-apple-loc) (random-apple-loc)))
  (reset! snek-blocks [[16 16] [24 16] [32 16] [40 16]])
  (reset! speed 100))

(defn handle-keypress
  [k]
  (when @allow-keypress?
    (when (not @paused?)
      (reset! allow-keypress? false))
    (cond
      ;; TODO: make a function for changing directions
      (and (not @paused?)
           (not= @direction :right)
           (or (= k VK_LEFT) (= k VK_H)))  (reset! direction :left)
      (and (not @paused?)
           (not= @direction :left)
           (or (= k VK_RIGHT) (= k VK_L))) (reset! direction :right)
      (and (not @paused?)
           (not= @direction :up)
           (or (= k VK_DOWN) (= k VK_J)))  (reset! direction :down)
      (and (not @paused?)
           (not= @direction :down)
           (or (= k VK_UP) (= k VK_K)))    (reset! direction :up)
      (= k VK_SPACE)                       (do (reset! allow-keypress? true)
                                               (reset! paused? (not @paused?))))))

(defn generate-apple
  []
  (when (and (< (count @apple-blocks) max-apples) (= 0 (rand-int apple-odds)))
    (swap! apple-blocks conj (random-apple-loc))))

(defn move-snek
  []
  (let [body (if @grow-snek? @snek-blocks (subvec @snek-blocks 1))
        old-head (last body)
        old-x (first old-head)
        old-y (second old-head)
        new-head (case @direction
                   :right [(+ old-x block-size) old-y]
                   :left  [(- old-x block-size) old-y]
                   :down  [old-x (+ old-y block-size)]
                   :up    [old-x (- old-y block-size)])]
    (reset! snek-blocks (conj body new-head))
    (reset! grow-snek? false)))

(defn draw-block [g [x y] color]
  (doto g
    (.setColor color)
    (.fillRect x y block-size block-size)))

;; TODO: combine draw-block and draw apple
(defn draw-apple [g [x y] color]
  (doto g
    (.setColor color)
    (.fillOval x y block-size block-size)))

(defn draw-snek
  []
  (proxy [JPanel KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (doseq [block @snek-blocks]
        (draw-block g block Color/GREEN))
      (doseq [apple @apple-blocks]
        (draw-apple g apple Color/RED))
      (.setColor g Color/BLUE)
      (.drawRect g 0 0 304 304))
    (keyPressed [e]
      (handle-keypress (.getKeyCode e)))
    (keyReleased [e]
      nil)
    (keyTyped [e]
      nil)))

(defn game-loop [snek]
  (loop []
    (when (not @paused?)
      (generate-apple)
      (move-snek)
      ;; check for overlaps with apples / body
      (when (@apple-blocks (last @snek-blocks))
        (swap! score inc)
        (reset! grow-snek? true)
        (reset! speed (- @speed 3))
        (swap! apple-blocks disj (last @snek-blocks)))
      (when (or (< (first (last @snek-blocks)) 0)
                (< (second (last @snek-blocks)) 0)
                (>= (first (last @snek-blocks)) 304)
                (>= (second (last @snek-blocks)) 304)
                ((set (subvec @snek-blocks 0 (dec (count @snek-blocks))))
                 (last @snek-blocks)))
        (println "score: " @score)
        (reset-game-state))
      (.repaint snek)
      (reset! allow-keypress? true)
      (Thread/sleep @speed))
    (recur)))

(defn -main
  [& _args]
  (let [snek (draw-snek)
        frame (JFrame. "no step on snek")]
    (reset-game-state)
    (doto snek
      (.setFocusable true)
      (.setBounds 40 80 300 300)
      (.addKeyListener snek))
    (doto frame
      (.add snek)
      (.setSize 400 500)
      (.setVisible true))
    (game-loop snek)))
