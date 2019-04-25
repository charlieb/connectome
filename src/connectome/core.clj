(ns connectome.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.string :refer [join]]))

(defn mk-node [] {:parent nil :kids [] :call ""})

(defn take-word [lst] (take-while #(not (some (partial = %) [\space \( \)])) lst))
(defn drop-word [lst] (rest (drop-while #(not (some (partial = %) [\space \( \)])) lst)))
(defn drop-until-fn [lst] (drop-while #(not (some (partial = %) [\( \)])) lst))

(defn code-to-nodes [code]
  "Walks the code creating a node for every expression and putting them in tree.
  node holds the current node"
  (loop [code code
         tree {0 (assoc (mk-node) :id 0)}
         node (assoc (mk-node) :id 0)]
    (println 'top code (:id node))
    (if (empty? code)
      tree
    (case (first code)
      \( (let [fn-call (join (take-word (rest code)))
               next-fn (drop-until-fn (rest code))
               new-id (+ 1 (apply max (keys tree)))
               new-node (assoc (mk-node)
                                :id new-id
                                :parent (:id node)
                                :call fn-call
                                :label (when (= fn-call "defn") (join (take-word (drop-word (rest code))))))
               new-tree (assoc (update-in tree [(:id node) :kids] #(conj % new-id))
                               new-id new-node)]
           (println "(" fn-call new-node)
           (recur next-fn new-tree new-node))
      \) (do (println ")") (recur (rest code) tree (tree (:parent node))))
      (do (println 'else code) (recur (drop-while #(not (some (partial = %) [\( \)])) code) tree node))))))

(defn print-nodes 
  ([nodes] (print-nodes 0 (nodes 0) nodes))
  ([indent node nodes] 
   (println (str (join (take indent (repeat \space))) (:call node)))
   (when (not (empty? (:kids node)))
     (doseq [n (:kids node)] 
       (print-nodes (+ 1 indent) (nodes n) nodes))))
  )

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  (let [nodes (code-to-nodes "(defn test [one two] (first (second one)) (third two))")]
    (println nodes)
    (print-nodes nodes))
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:color (mod (+ (:color state) 0.7) 255)
   :angle (+ (:angle state) 0.1)})

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  (q/fill (:color state) 255 255)
  ; Calculate x and y coordinates of the circle.
  (let [angle (:angle state)
        x (* 150 (q/cos angle))
        y (* 150 (q/sin angle))]
    ; Move origin point to the center of the sketch.
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 2)]
      ; Draw the circle.
      (q/ellipse x y 100 100))))


(q/defsketch connectome
  :title "You spin my circle right round"
  :size [500 500]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
