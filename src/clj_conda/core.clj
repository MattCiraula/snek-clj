(ns clj-conda.core
  (:import (java.awt Color Dimension) 
	         (javax.swing JPanel JFrame Timer JOptionPane JButton)
           (java.awt.event ActionListener KeyListener)
           (java.lang Thread))
  (:gen-class))

;; TODO: handle apple generation
;;       handle game over
;;       handle boundaries
;;       speed up game with time
;;       implement scoring
;;       polish UI

(def VK_LEFT (java.awt.event.KeyEvent/VK_LEFT))
(def VK_RIGHT (java.awt.event.KeyEvent/VK_RIGHT))
(def VK_UP (java.awt.event.KeyEvent/VK_UP))
(def VK_DOWN (java.awt.event.KeyEvent/VK_DOWN))
(def VK_SPACE (java.awt.event.KeyEvent/VK_SPACE))
(def VK_Q (java.awt.event.KeyEvent/VK_Q))
(def VK_H (java.awt.event.KeyEvent/VK_H))
(def VK_J (java.awt.event.KeyEvent/VK_J))
(def VK_K (java.awt.event.KeyEvent/VK_K))
(def VK_L (java.awt.event.KeyEvent/VK_L))

(def block-size 8)
(def direction (atom :right))
(def speed (atom 100))
;; TODO: change this to an atom
(def snek-blocks [[16 16] [24 16] [32 16] [40 16]])
(def grow-snek? (atom false))
;; TODO: change this to an atom
(def apple-blocks #{[64 64] [80 104] [104 56] [256 256] [64 160] [24 80]})

(defn handle-keypress
  [k]
  (cond
    ;; TODO: make a function for changing directions
    (and (not= @direction :right)
         (or (= k VK_LEFT) (= k VK_H)))  (reset! direction :left)
    (and (not= @direction :left)
         (or (= k VK_RIGHT) (= k VK_L))) (reset! direction :right)
    (and (not= @direction :up)
         (or (= k VK_DOWN) (= k VK_J)))  (reset! direction :down)
    (and (not= @direction :down)
         (or (= k VK_UP) (= k VK_K)))    (reset! direction :up)))

(defn move-snek
  []
  (let [body (if @grow-snek? snek-blocks (subvec snek-blocks 1))
        old-head (last body)
        old-x (first old-head)
        old-y (second old-head)
        new-head (case @direction
                   :right [(+ old-x block-size) old-y]
                   :left  [(- old-x block-size) old-y]
                   :down  [old-x (+ old-y block-size)]
                   :up    [old-x (- old-y block-size)])]
    (def snek-blocks (conj body new-head))
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
      (doseq [block snek-blocks]
        (draw-block g block Color/GREEN))
    (doseq [apple apple-blocks]
        (draw-apple g apple Color/RED)))
    (keyPressed [e]
      (handle-keypress (.getKeyCode e))
      (comment
      (let [keycode (.getKeyCode e)]
        (cond
          ;; TODO: make a function for changing directions
          (= keycode VK_LEFT) (reset! direction :left)
          (= keycode VK_H) (reset! direction :left)
          (= keycode VK_RIGHT) (reset! direction :right)
          (= keycode VK_L) (reset! direction :right)
          (= keycode VK_DOWN) (reset! direction :down)
          (= keycode VK_J) (reset! direction :down)
          (= keycode VK_UP) (reset! direction :up)
          (= keycode VK_K) (reset! direction :up)
          ;(= keycode VK_SPACE) (swap! speed * 1.1))
          (= keycode VK_SPACE) (move-snek)
          (= keycode VK_Q) (reset! grow-snek? true))
        (.repaint this))))
    (keyReleased [e]
      nil)
    (keyTyped [e]
      nil)))

(defn game-loop [snek]
  (loop []
    (move-snek)
    ;; check for overlaps with apples / body
    (when (apple-blocks (last snek-blocks))
      (reset! grow-snek? true)
      (def apple-blocks (disj apple-blocks (last snek-blocks))))
    (when ((set (subvec snek-blocks 0 (dec (count snek-blocks)))) (last snek-blocks))
      (println "he ded"))
    (.repaint snek)
    (Thread/sleep @speed)
    (recur)))

(defn -main
  [& args]
  (let [snek (draw-snek)
        frame (JFrame. "no step on snek")]
    (doto snek
      (.setFocusable true)
      (.setBounds 40 80 300 300)
      (.addKeyListener snek))
    (doto frame
      (.add snek)
      (.setSize 400 500)
      (.setVisible true))
      (game-loop snek)
    ))

