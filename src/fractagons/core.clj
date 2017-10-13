;;;; Program:  Fractagons, vsn 1.0
;;;; Author:  John Lynch
;;;; Date:  August 2017

;;;; Use:  IFS fractal image generator.

(ns fractagons.core
  (:require [quil.core :as q])
  (:require [fractagons.dynamic :as dyn])
  (:require [quil.middleware :as m])
)
    
(q/defsketch fractagons
  :title "Fractagons"               
  :setup dyn/setup
  :update dyn/update-state           
  :draw dyn/draw
  :mouse-clicked dyn/mouse-clicked             
  :key-typed dyn/key-typed             
  ; :display 1
  :size [768 768]
  ; :features [:present]
  :features [:exit-on-close]
  :middleware [m/fun-mode])  