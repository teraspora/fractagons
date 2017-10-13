;;;; Program:  Fractagons, vsn 1.0
;;;; Author:  John Lynch
;;;; Date:  August 2017

;;;; Use:  IFS fractal image generator.

;;; TO DO: logging option/ separate window/jframe
;;;        enter params on invocation?? (Override defaults?)  
;;;        package:  uberjar??  webapp?   android?
;;;        more colour control: try RGB??  [curvature speed sector -> [r g b]?? Colour maps?


(ns fractagons.dynamic
  (:require [quil.core :as q]
            [clojure.string :as str]))
(import javax.swing.JFileChooser)

;; Constants etc.
(defn BLACK [] (q/color 0 0 0)) ; create a fn as a workaround, as can't 
                                ; invoke (q/color) outside of setup/draw/update
(def mBLACK (memoize BLACK))    ; memoize it to obviate repeated calls

(def PI Math/PI)
(def QUARTER-PI (* PI 0.25))
(def HALF-PI (* PI 0.5))
(def TWO-PI (* PI 2.0))
(def TWO-OVER-PI (/ 2.0 PI))
(def A-THIRD 0.333333333334)
(def e-1 (- Math/E 1.0))
(def ROOT2 (q/sqrt 2.0))
(def ROOT3 (q/sqrt 3.0))
(def HALF-ROOT2 (* ROOT2 0.5))
(def HALF-ROOT3 (* ROOT3 0.5))
(def PRE-TRANS-FUNC-COUNT 21)


(def vmult (/ 255.0 (q/sqrt 32)))
;(def vmult 46.0)
;(def cmult 32.0)
(def cmult 21.0)

; Complex constants
(def ZERO [0.0 0.0])
(def ONE [1.0 0.0])
(def I [0.0 1.0])
(def uvec45 [HALF-ROOT2 HALF-ROOT2])

;; ANSI colours for repl logging
(def CLR_LB "\u001B[94m")
(def CLR_LC "\u001B[96m")
(def CLR_LR "\u001B[91m")
(def CLR_LG "\u001B[92m")
(def CLR_LM "\u001B[95m")
(def RESET_ALL "\u001B[0m")
(def NEWLINE "\n")
(def SPACE " ")

;; Print fns
(defn print-in-colour [s ansi-colour]
  (print (str ansi-colour s RESET_ALL NEWLINE))
  (flush))

(defn print-map 
  "Print the supplied map neatly and return it unaltered"
  [m] 
  (-> (into (sorted-map) m) print-str (str/replace "," "\n") (print-in-colour CLR_LC))
  (flush) m) 

;; Pixel dimensions of window; just change these two defs if you want a different size image
(def width 768)
(def height 768)

;; Needed for transforming subset of complex plane (tetra-unit square) to applet window
(def scale-factor (/ height 4.0))
(def x-trans (/ (- width height) 2.0))

; The number of pixels to shift, when shifting; so, 12 pixels each way for a 768x768 canvas
(def x-delta (/ width 64))
(def y-delta (/ height 64))

;; some initial/default values for params
(def A 0.75)
(def B 0.0)
(def T 0.5)
(def U 0.5)
(def W 1.0)

(defn init-state [state]
  (assoc state :size 1 :x0 0.0 :y0 0.0 :x 0.0 :y 0.0
    :level 0 :image-num 0 :making-video-seq false :current-video-dir SPACE
    :hue-offset 0 :x-shift 0 :y-shift 0 :scale-not-shift true :x-scale 1.0 :y-scale 1.0 :mirror false
    :invert-colours false :param-delta 0.2 :colour-by-speed false :curvature 1.0 :pre-transform false
    :apply-ballfold false :swap-xy false :treat-as-polar false :polarise-vfunc false 
    :root-pre-trans false :sq-pre-trans false :sq-pre-trans-components false :root-pre-trans-components false 
    :rotate-by-half-pi false :rotate-by-quarter-pi false :rotate-by-half-a-sector false
    :reapply-vfunc false :reflectLR false :reflectUD false))

(defn set-default-params [state]
  (assoc state :t T :u U :w W :a A :b B))

(defn reset-state [state]
  (print-in-colour (str "Resetting state to default for order " 
      (:polygon-order state) " fractagons with variation " (:variation state)) CLR_LM)
    (q/background 0)
    (print-map (dissoc (set-default-params (init-state state)) :navigation-2d)))

;; Some utility fns

(defn rand-bool []
 (if (zero? (rand-int 2)) true false))

(defn inc2 [n]
  (+ n 2))

(defn dec2 [n]
  (- n 2))

(defn dec-by-x-delta [n]
  (- n x-delta))

(defn inc-by-x-delta [n]
  (+ n x-delta))

(defn dec-by-y-delta [n]
  (- n y-delta))

(defn inc-by-y-delta [n]
  (+ n y-delta))

(defn twice [x]
  (* 2.0 x))

(defn half [x]
  (* 0.5 x))

(defn cube [x]
  (* x x x))

(defn sin2 [x]
  (let [s (q/sin x)]
    (* s s)))

(defn cos2 [x]
  (let [c (q/cos x)]
    (* c c)))

(defn root-sin [x]
  (let [s (Math/sin x)]
    (-> s Math/abs Math/sqrt double (Math/copySign s))))

(defn root-cos [x]
  (let [c (Math/cos x)]
    (-> c Math/abs Math/sqrt double (Math/copySign c))))

; Hmmm... there are many ways to write this; tail recursion prob. not the most efficient
(defn space
  "Return a string of n spaces"
  [n]
  (if (= n 1) SPACE (str SPACE (space (dec n)))))

(defn dock-string
  "Return a copy of s with last n chars removed"
  [s n]
  (subs s 0 (- (count s) n)))

;;; ##################################################################################################

(defn setup []
  (q/no-stroke)
  (q/smooth)
  (q/frame-rate 512)
  (q/color-mode :hsb 255.0)
  (q/background 0)  
  (print-map (set-default-params (init-state {:polygon-order 3 :variation 0 :pre-trans-index 0}))))

;;; ##################################################################################################

; Display control fns
(defn display->xy 
  "Convert screen to Cartesian coords"
  [px py x-scale y-scale]
  [(/ (- (/ (- px x-trans) scale-factor) 2.0) x-scale) (/ (- (/ py scale-factor) 2.0) y-scale)]) 

(defn xy->display
  "Convert Cartesian to screen coords"
  [x y x-scale y-scale]
  [(+ x-trans (int (* (+ (* x x-scale) 2.0) scale-factor))) (int (* (+ (* y y-scale) 2.0) scale-factor))])

(defn mirror-pixel 
  "Given a pixel x and y coords, return a list of mirrored coord vectors"
  [i j width height]
  (if (or (odd? (int i)) (odd? (int j)))
    []
    (let [p (/ i 2)
          q (/ j 2)
          r (- width p 1)
          s (- height q 1)]
      (list [p q] [r q] [p s] [r s]))))   ; return list of 4 2-element vectors defining the 4 mirror-image points


(defn draw-dot [[i j] size]
  (if (= size 1) (q/set-pixel i j (q/current-fill))
                 (q/ellipse i j size size)))

(defn draw-dots 
  "Draw four dots with the current fill"
  [points size]
  (when (= (count points) 4)
    (do
      (draw-dot (nth points 0) size)
      (draw-dot (nth points 1) size)
      (draw-dot (nth points 2) size)
      (draw-dot (nth points 3) size))))

;;; ##################################################################################################

;;; Saving & reverting 

(defn save-state-map
  "Save the current state map in a .frm file"
  [state fname]
  (spit (str fname ".frm") (dissoc state :level :last-fname :image-num 
                                            :making-video-seq :current-video-dir :param-delta )))    

(defn revert-to-state
  "Revert to a state saved in a .frm file, if one exists"
  [state fname]
  (let [fgon-state (if (.exists (java.io.File. fname))
                       (read-string (slurp fname))
                       nil)]
    (if fgon-state 
        (do (q/background 0)
            (print-map (dissoc (merge state fgon-state {:level 0 :param-delta (:param-delta state) 
              :scale-not-shift (:scale-not-shift state) :last-fname (dock-string fname 4)}) :navigation-2d)))
        (do (print-in-colour "No state-map file to which to revert.   Ignoring." CLR_LR)
            (flush)
             state))))

(defn revert-to-last-image
  "Revert to last saved image if one exists"
  [state]
  (let [img (str (:last-fname state) ".png")]
    (if (> (count img) 4) 
      (q/set-image 0 0 (q/load-image img)) (print-in-colour "No image to which to revert.   Ignoring." CLR_LR))
    state))

;;; ##################################################################################################















;;; ##################################################################################################

;; Complex and vector fns
(defn sqrt-copy-sign
  "Take the square root of the absolute value of a real number and give it the 
     sign of the original number"
  [x]
  (Math/copySign (Math/sqrt (Math/abs x)) x))

(defn arg
  "Return the angle subtended by a vector with x=0, i.e. the argument of x+iy"
  [[x y]]
  (Math/atan2 y x))

(defn bar-cx
  "Complement of complex number"
  [[x y]]
  [x (- y)])

(defn swap-xy-cx
  "Switch the real and imaginary parts of a complex number"
  [[x y]]
  [y x])

(defn neg-cx
  "Negative of complex number"
  [[x y]]
  [(- x) (- y)])

(defn neg-real-cx
  "Negative of real part of complex number - same as 'minus x bar'"
  [[x y]]
  [(- x) y])

(defn neg-imag-cx
  "Negative of imaginary part of complex number - same as 'x bar'"
  [[x y]]
  [x (- y)])

(defn mod-cx
  "Complex modulus"
  [[x y]]
  (Math/hypot x y))

(defn mod2-cx
  "Complex modulus squared"
  [[x y]]
  (+ (* x x) (* y y)))

(defn mult-cx 
  "Complex multiplication"
  [[x y] [u v]]
  [(- (* x u) (* y v)) (+ (* x v) (* y u))])

(defn add-cx 
  "Complex addition"
  [[x y] [u v]]
  [(+ x u) (+ y v)])

(defn sub-cx 
  "Complex subtraction"
  [[x y] [u v]]
  [(- x u) (- y v)])

(defn recip-cx
  "Complex reciprocal"
  [[x y]]
  (let [denom (+ (* x x) (* y y))]
        (if-not (= denom 0.0) [(/ x denom) (- (/ y denom))]
                              [Double/MAX_VALUE Double/MAX_VALUE])))

(defn div-cx
  "Complex division"
  [z w]
  (mult-cx z (recip-cx w)))

(defn real->cx
  "Convert a double to a two-element vector representing a complex number"
  [x]
  [x 0.0])

(defn sq-cx
  "Square a complex number"
  [[x y]]
    [(- (q/sq x) (q/sq y)) (* 2 x y)])

(defn sq-components-signed-cx
  "Square the real and imaginary parts of a complex number, and adjust signs to preserve quadrant"
  [[x y]]
    [(Math/copySign (* x x) x) (Math/copySign (* y y) y)])

(defn root-components-signed-cx
  "Apply (sqrt-copy-sign) to both real and imaginary parts of a complex number"
  [[x y]]
    [(sqrt-copy-sign x) (sqrt-copy-sign y)])

(defn root-cx
  "Square root of a complex number"
  [z]
  (let [rr          (-> z mod-cx q/sqrt)
        half-theta  (half (arg z))]
    [(* rr (q/cos half-theta)) (* rr (q/sin half-theta))]))

(defn polar-cx
  "Given a complex number in polar form, convert to Cartesian [x y]"
  [[r theta]]
  [(* r (q/cos theta)) (* r (q/sin theta))])

(defn uvec 
  "Return the unit vector with the specified angle"
  [theta]
  [(q/cos theta) (q/sin theta)])

(defn rotate
  "Rotate a vector by a specified angle"
  [z psi]
  (mult-cx z (uvec psi)))

(defn scale
  "Scale a vector by a real factor"
  [[x y] k]
  [(* x k) (* y k)])

(defn vec-dot
  "Given 2 Cartesian 2-D vectors, return the dot product"
  [[x y] [u v]]
  (+ (* x u) (* y v)))

(defn sep
  "Return the distance separating two points"
  [[x y] [u v]]
    (q/sqrt (+ (q/sq (- x u)) (q/sq (- y v)))))

;;; ##########################################################################################

(defn ball-fold 
    "DEPRECATED!!!"
    [z r R]
  (let [zabs (mod-cx z)
        rabs (q/abs r)
        Rabs (q/abs R)]
        (cond (< zabs rabs) (div-cx z (real->cx (q/sq rabs)))
              (< zabs Rabs) (div-cx z (real->cx (q/sq zabs)))
              :else         z)))

;;; ##########################################################################################

;; Variation fns
(defn vari0
  "Variation function 0 - linear; the identity fn"
  [z]
  z)

(defn vari1
  "Variation function 1 - sinusoidal"
  [[x y]]
  [(twice (q/sin x)) (twice (q/sin y))])  ; added multiplier to Draves' vsn.

(defmulti vari2 (fn [[x y]] (if (and (= x 0.0) (= y 0.0)) 0 1)))
  
  (defmethod vari2 0 [[x y :as z]] 
    (print-in-colour (str "::::::: Invoking vari2 dsp 1 with \nx = " x 
      "\ny = " y) CLR_LC)
    ZERO)
  
  (defmethod vari2 1 [[x y :as z]] 
    (let [r2 (mod2-cx z)]
      [(/ x r2) (/ y r2)]))
  
(defn vari3
  "Variation function 3 - swirl"
  [[x y :as z]]
  (let [r2  (mod2-cx z)
        sr2 (q/sin r2)
        cr2 (q/cos r2)]
    [(- (* x sr2) (* y cr2)) (+ (* x cr2) (* y sr2))]))

(defn vari4
  "Variation function 4 - horseshoe"
  [[x y :as z]]
    (div-cx (sq-cx z) [(mod-cx z) 0.0]))

(defn vari5
  "Variation function 5 - polar / comb"
  [[x y :as z]]
  (let [r (mod-cx z)
        theta (arg [y x])
        thop (/ theta PI)]
    [thop (- r 1.0)]))

(defn vari6
  "Variation function 6 - handkerchief / egg"
  [[x y :as z]]
  (let [r (mod-cx z)
        theta (arg [y x])]
    [(* r (q/sin (+ theta r))) (* r (q/cos (- theta r)))]))

(defn vari7
  "Variation function 7 - heart"
  [[x y :as z]]
  (let [r (mod-cx z)
        theta (arg [y x])
        thr (* r theta)]
    [(* r (q/sin thr)) (* (- r) (q/cos thr))]))

(defn vari8
  "Variation function 8 - disc"
  [[x y :as z]]
  (let [r (mod-cx z)
        theta (arg [y x])
        pir (* r PI)
        thopi (/ theta PI)]
    [(* thopi (q/sin pir)) (* thopi (q/cos pir))]))

(defn vari9
  "Variation function 9 - spiral"
  [[x y :as z]]
  (let [r (mod-cx z)
        theta (arg [y x])]
    (div-cx [(+ (q/cos theta) (q/sin r)) (- (q/sin theta) (q/cos r))] [r 0.0])))

(defn vari10
  "Variation function 10 - hyperbolic"
  [[x y :as z]]
  (let [r (mod-cx z)
        theta (arg [y x])]
    [(/ (q/sin theta) r) (* r (q/cos theta))]))

(defn vari11
  "Variation function 11 - diamond"
  [[x y :as z]]
  (let [r (mod-cx z)
        theta (arg [y x])]
    [(* (q/sin theta) (q/cos r)) (* (q/cos theta) (q/sin r))]))

(defn vari12
  "Variation function 12 - ex"
  [[x y :as z]]
  (let [r (mod-cx z)
        theta (arg [y x])
        p0c (cube (q/sin (+ theta r)))
        p1c (cube (q/cos (- theta r)))]
    [(* r (+ p0c p1c)) (* r (- p0c p1c))]))

(defn vari16    ; #13
  "Variation function 16 - fisheye"
  [[x y :as z]]
  (let [t (half (inc (mod-cx z)))]
    [(* t y) (* t x)]))

(defn vari18    ; #14
  "Variation function 18 - exponential"
  [[x y :as z]]
  (let [t (Math/exp (dec x))
        u (* PI y)]
    [(* t (q/cos u)) (* t (q/sin u))]))

(defn vari19    ; #15
  "Variation function 19 - power"
  [[x y :as z]]
  (let [theta (arg [y x])
        sin-theta (q/sin theta)
        t (Math/pow (mod-cx z) sin-theta)]
    [(* t (q/cos theta)) (* t sin-theta)]))

(defn vari27    ; #16
  "Variation function 27 - eyefish"
  [[x y :as z]]
  (let [t (half (inc (mod-cx z)))]
    [(* t x) (* t y)]))

(defn vari28    ; #17
  "Variation function 28 - bubble"  
  [[x y :as z]]
  (let [t (inc (/ 4.0 (mod2-cx z)))]
    [(* t x) (* t y)]))

(defn vari29    ; #18
  "Variation function 29 - cylinder"
  [[x y]]
  [(q/sin x) y])

(defn vari42    ; #19
  "Variation function 42 - tangent"
  [[x y]]
  [(/ (q/sin x) (q/cos y)) (Math/tan y)])

(defn vari48    ; #20
  "Variation function 48 - cross"
  [[x y]]
  (let [t   (- (* x x) (* y y))
        u   (q/sqrt (/ 1.0 (* t t)))] 
  [(* u x) (* u y)]))

(defn variJ0    ; #21
  "Variation function J0 - duck"
  [[x y]]
    [(+ (q/sin x) (q/cos y)) (- (q/cos x) (q/sin y))])

(defn variJ1    ; #22
  "Variation function J1 - Minkowski variant"
  [[x y :as z]]
  (let [c     (q/cos x)
        s     (q/sin y)]
    [(twice (- (* x c) (* y s))) (twice (+ (* x s) (* y c)))]))

;;; #      #      #      #      #      #      #      #      #      #      #      #      #      #      #      

(defn d-fn [[x y :as z]]  ; #23
  [(* PI (q/cos x) (q/sin y)) (arg z)])

(defn d-fn2  ; #24
  "ts277"
  [z]
  (-> z d-fn polar-cx))

(defn ts390 ; #25
  [[x y :as z]]
  [(* Math/E (q/sqrt (Math/abs (max (q/cos x) (q/cos y))))) (arg z)])

(defn ts323 [[x y :as z]]  ; #26
  (polar-cx [(* 2.0 ROOT2 (q/sin x) (q/cos y)) (arg z)]))

(defn fg010 [[x y :as z]]  ; #27
  [(twice (+ (q/sin x) (q/cos y))) (arg z)])

(defn fg011 [[x y :as z]]  ; #28
  [(* 4.0 (q/sin x) (q/cos y)) (arg z)])

(defn fg012 [[x y :as z]]  ; #29
  [(twice (+ (q/cos x) (q/sin y))) (arg z)])

(defn fg013 [[x y :as z]]  ; #30
  [(* 4.0 (q/cos x) (q/sin y)) (arg z)])

(defn e-fn [[x y :as z]]  ; #31
  "vfts53"
  [(* 2.0 (q/sin x) (q/cos y)) (arg z)])

(defn e-fn2  ; #32
  "vfts53"
  [z]
  (-> z e-fn polar-cx))

(defn fg014 [[x y :as z]]  ; #33
  [(* 4.0 (q/cos (q/sin y))) (* 4.0 (q/sin (q/sin x)))])

(defn fg015 [[x y]]  ; #34
   [(* x y) (+ x y)])

(defn fg016 ; #35
  "vv25"
  [[x y :as z]]
  [(* Math/E (q/sqrt (Math/abs (min (q/cos x) (q/cos y))))) (arg z)])

(defn fg017 ; #36
  "vv25"
  [[x y :as z]]
  [(* Math/E (q/sqrt (Math/abs (max (q/sin x) (q/sin y))))) (arg z)])

(defn fg018 ; #37
  "vv25"
  [[x y :as z]]
  [(* Math/E (Math/cbrt (Math/abs (min (q/cos x) (q/cos y))))) (arg z)])

(defn fg019 ; #38
  "vv25"
  [[x y :as z]]
  [(* Math/E (Math/sqrt (Math/abs (max (q/cos x) (q/cos y)))))
   (* Math/E (Math/sqrt (Math/abs (min (q/sin x) (q/sin y)))))])

(defn fg020 ; #39
  "new idea"
  [[x y :as z]]
  (let [r (mod-cx z)
        theta (arg z)]
    [(- r (q/sin theta)) (+ r (q/cos theta))]))

(defn fg021 ; #40
  "new idea"
  [[x y :as z]]
  (let [r (mod-cx z)
        theta (arg z)]
    (mult-cx [(* theta (q/cos r)) (* theta (q/sin r))] [TWO-OVER-PI 0.0])))

(defn fg022 ; #41
  "new idea"
  [[x y :as z]]
  (let [r (mod-cx z)
        theta (arg z)]
    (mult-cx [(* theta (q/cos r)) (* theta (q/sin r))] [TWO-OVER-PI 0.0])))

(defn fg023 ; #42
  "try & see!"
  [[x y :as z]]
  (let [r (mod-cx z)
        theta (arg z)]
    [(twice (q/cos (* r theta))) (twice (q/sin (+ r theta)))]))

(defn fg024    ; #43
  "vf17"
  [[x y]]
  [(q/cos x) y])

;; Put the variation functions we we want to use 
;  into a vector so we can index them; not the best solution, I know...
(def vfuncs [vari0 vari1 vari2 vari3 vari4 vari5 vari6 
             vari7 vari8 vari9 vari10 vari11 vari12 vari16 vari18 vari19 vari27
             vari28 vari29 vari42 vari48 variJ0 variJ1 d-fn d-fn2 ts390 ts323
             fg010 fg011 fg012 fg013 e-fn e-fn2 fg014 fg015 fg016 fg017 fg018
             fg019 fg020 fg021 fg022 fg023 fg024])

;;; ##########################################################################################

;; A new heart for the program!
(defn polygon-j
  "John's final affine n-gon fn"
  [z t u a b n spoke vfunc]  
                     ; t and u = scaling parameters
                     ; a and b = translation parameters
                     ; n = order of polygpn; 
                     ; spoke = rotation parameter: 
                         ; which of the n sector unit vectors (0 to (n-1)) to multiply by.
  (let [[x y] (vfunc z)
        temp  [(+ (* t x) a) (+ (* u y) b)]
        [x1 y1]  (rotate temp (/ (* TWO-PI spoke) n))]
        [x1 y1]))

;;; ##########################################################################################

(defn create-random-state 
  "Pot-luck image; randomise salient parameters but retain polygon-order, colour settings etc."
  [state symmetrical? preserve-vfunc?]
  (let 
    [{:keys [sq-pre-trans root-pre-trans sq-pre-trans-components root-pre-trans-components variation]} state
     rs   (into state       ; map these keys to the values below
             (zipmap [:variation :pre-trans-index
               :x-scale :y-scale :x-shift :y-shift :level :drawn-frames :making-video-seq
               :t :u :a :b
               :pre-transform :treat-as-polar :polarise-vfunc :swap-xy :apply-ballfold
               :root-pre-trans :sq-pre-trans :root-pre-trans-components :sq-pre-trans-components :reapply-vfunc]
               
              (concat 
                [(if preserve-vfunc? variation (rand-int (count vfuncs))) (rand-int PRE-TRANS-FUNC-COUNT)
                1.0 1.0 0 0 0 0 false
                (dec2 (rand 4)) (dec2 (rand 4)) (dec2 (rand 4)) (dec2 (rand 4))]
                (take 9 (repeatedly rand-bool)) [(if symmetrical? false (rand-bool))])))
      rs   (if sq-pre-trans 
                 (assoc rs :root-pre-trans false) 
                 rs)
      rs   (if sq-pre-trans-components
                 (assoc rs :root-pre-trans-components false) 
                 rs)]
     rs))

(defn create-video-dir
  "Create a dedicated directory in which to begin saving images regularly as img00000.png, img00001.png,...;
   and return its name as a string"
  []
  (loop [dir-num 0
         dir-name "fgonvid-000"]
    (if-not (.exists (java.io.File. dir-name)) 
      (do (.mkdir (java.io.File. dir-name)) dir-name)
      (recur (inc dir-num) (str "fgonvid-" (format "%03d" dir-num))))))

(defn pre-transform
  "Function to transform [x y] before applying (polygon-h) function"
  [[x y :as z] index]
  (case index
    0 [(-> x q/cos twice) (-> y q/sin twice)]
    1 [(-> x q/sin twice) (-> y q/cos twice)]
    2 [(-> x q/cos twice) (-> y q/cos twice)]
    3 [(-> x q/sin twice) (-> y q/sin twice)]
    4 [(-> x cos2 twice) (-> y sin2 twice)]
    5 [(-> x sin2 twice) (-> y cos2 twice)]
    6 [(-> x cos2 twice) (-> y cos2 twice)]
    7 [(-> x sin2 twice) (-> y sin2 twice)]
    8 [(-> x root-cos twice) (-> y root-sin twice)]
    9 [(-> x root-sin twice) (-> y root-cos twice)]
    10 [(-> x root-cos twice) (-> y root-cos twice)]
    11 [(-> x root-sin twice) (-> y root-sin twice)]
    12 [(-> x root-cos q/sin twice) (-> y root-sin q/cos twice)]
    13 [(+ (q/cos x) (q/sin x)) (- (q/sin y) (q/cos y))]
    14 [(+ (q/cos x) (q/sin y)) (- (q/sin x) (q/cos y))]
    15 (recip-cx z)
    16 (polar-cx [(/ 1.0 (mod-cx z)) (arg z)])
    17 [(-> x Math/exp q/cos (* PI)) (-> y Math/exp q/sin (* PI))]
    18 [(-> x Math/expm1 q/cos (* PI)) (-> y Math/expm1 q/sin (* PI))]
    19 [(-> x Math/abs Math/log q/cos (* PI)) (-> y Math/abs Math/log q/sin (* PI))]
    20 [(-> x Math/abs Math/log1p q/cos (* PI)) (-> y Math/abs Math/log1p q/sin (* PI))]))
    
;;; ##########################################################################################

(defn update-state [state]
  (let [{:keys [x y x0 y0 t u w a b size level polygon-order colour-by-speed
                pre-trans-index root-pre-trans sq-pre-trans 
                root-pre-trans-components sq-pre-trans-components image-num]} state
        n               (rand-int polygon-order)  ; choose a quadrant of the polygon
        vfunc           (get vfuncs (:variation state))
        vfunc           (if (:polarise-vfunc state) (comp polar-cx vfunc) vfunc)
        [p q]               (if (:pre-transform state)
                          (let [z-pre      (pre-transform [x y] pre-trans-index)
                                z-pre      (cond sq-pre-trans-components (sq-components-signed-cx z-pre)
                                                 root-pre-trans-components (root-components-signed-cx z-pre)
                                                 :else                     z-pre)
                                z-pre      (cond root-pre-trans   (root-cx z-pre)
                                                 sq-pre-trans     (sq-cx z-pre)
                                                 :else            z-pre)]
                                z-pre)
                          [x y])
        z               (if (:swap-xy state) [q p] [p q])
        z               (if (:treat-as-polar state) (polar-cx z) z)
        z               (if (:apply-ballfold state) (ball-fold z a b) z) 
        z               (polygon-j z t u a b polygon-order n vfunc)
        [x4 y4]         (if (:reapply-vfunc state) (vfunc z) z)
        curv            (if colour-by-speed 0.0 (+ TWO-PI (- (arg (sub-cx [x4 y4] [x y])) (arg (sub-cx [x y] [x0 y0])))))
        shoot-frame?    (and (:making-video-seq state) (pos? size) (zero? (mod level (int (* 20 (Math/log level))))))]

    (when shoot-frame?
      (let [fpath (str (:current-video-dir state) "/img" (format "%05d" (inc image-num)) ".png")]
        (print-in-colour (str "Saving video frame as " fpath) CLR_LR)
        (q/save fpath)))

    (assoc state :x0 x :y0 y :x x4 :y y4 :curvature curv :level (inc level)
      :image-num (if-not shoot-frame? image-num (inc image-num))))) ; return updated map

(defn draw [state]
  (try
    (let [{:keys [x y x0 y0 size level x-shift y-shift 
                  colour-by-speed polygon-order invert-colours hue-offset]} state 
        col-shift (if colour-by-speed (* (sep [x0 y0] [x y]) vmult) (* (:curvature state) cmult))
        kl        (/ level 4096)  
        log       (= (int kl) kl) 
        rot90     (:rotate-by-half-pi state) 
        rot45     (:rotate-by-quarter-pi state) 
        roths     (:rotate-by-half-a-sector state)
        xs        (:x-scale state)
        ys        (:y-scale state)
        [x y]     (if rot45 (rotate [x y] QUARTER-PI) [x y])
        [x y]     (if roths (rotate [x y] (/ PI polygon-order)) [x y])        
        [px py]   (xy->display (if rot90 (- y) x) (if rot90 x y) xs ys)
        pxs       (let [pxr (+ px x-shift)]
                    (if (:reflectLR state) (- width pxr 1) pxr))
        pys       (let [pyr (+ py y-shift)]
                    (if (:reflectUD state) (- height pyr 1) pyr))
        old-col   (q/get-pixel pxs pys)
        new-col   (let [temp (mod (+ col-shift hue-offset) 256)
                        hue  (if invert-colours (mod (+ temp 128) 256) temp)
                        hue  (if (< 64 hue 85) (- 255 hue) hue)]
                  (q/color hue 255 255))
        lerp-frac (if (= old-col (mBLACK)) 0.75 0.5)
        mean-col  (q/lerp-color old-col new-col lerp-frac)]
      (q/fill mean-col)

      ; print state every 4096 iterations
      (when log (do (println (str level " iterations, framerate = " (q/current-frame-rate)))
                    (flush)
                    (print-map 
                      (dissoc (into {} (map #(when (second %) %) state)) nil :x0 :y0 :x :y :curvature :level
                        (when-not (:pre-transform state) :pre-trans-index)))))
            ;; or: (reduce #(if-not (second %2) (dissoc %1 (first %2)) %1) state state)

      (if (:mirror state) 
        (draw-dots (mirror-pixel pxs pys width height) size)
        (draw-dot [pxs pys] size)))
   (catch Exception e (print-in-colour "Exception encountered, dec-ing vfunc & resetting..." CLR_LM)
                      (.printStackTrace e)
                      (reset! (q/state-atom) 
                        (-> (assoc state :variation (mod (dec (:variation state)) (count vfuncs))) reset-state)))))

(defn mouse-clicked 
  "Clear the display and log state"
  [state event]
  (println (str "Iteration count = " (:level state)))
  (q/background 0)
  (let [px    (:x event)
        py    (:y event)
        xs    (:x-scale state)
        ys    (:y-scale state)
        [x y] (display->xy px py xs ys)]
    (-> (assoc state :x x :y y :level 0) print-map)))

;; The main user input controller
(defn key-typed [state event]
  (let [k       (:key event)
        size    (:size state)
        delta   (:param-delta state)
        delta+1 (inc delta)
        sns     (not (:scale-not-shift state))
        vx      (:variation state)
        n       (:polygon-order state)
        ptx     (:pre-trans-index state)
        rpt     (:root-pre-trans state)
        spt     (:sq-pre-trans state)
        mvs     (:making-video-seq state)]
    (print-in-colour (dissoc event :key-code :raw-key) CLR_LB)

    (cond 
          ; Quit program unconditionally
          (= k :Q)                   (do (println "Ciao!") (q/exit))
          
          ; Save the image as .png & state map as .frm file
          (= k :s)                   (do  
                                        (let [fname     (str "images/fgon" n "V" vx "-" (:level state))
                                              fname-png (str fname ".png")]
                                          (print-in-colour (str ":: Saving image as " fname-png) CLR_LM)
                                          (flush)
                                          (q/save fname-png)
                                          (save-state-map state fname)
                                          (assoc state :last-fname fname)))

          ; Load a saved state map from a .frm file and crank the starter
          (= k :_)                   (let [fo-dialog (JFileChooser. "images/")
                                          response (.showOpenDialog fo-dialog nil)
                                          fname (if (= response JFileChooser/APPROVE_OPTION)
                                                    (.getCanonicalPath (.getSelectedFile fo-dialog))
                                                    nil)]
                                        (if (nil? fname)
                                          (do (print-in-colour "No file selected." CLR_LR) (flush) state)
                                          (do (print-in-colour "Reverting to selected state" CLR_LM)
                                              (revert-to-state state fname))))
          
          ; Create a random state
          (= k :g)                   (do (print-in-colour "Creating random state..." CLR_LM)
                                         (q/background 0)
                                         (print-map (create-random-state state false 
                                            (if (contains? (q/key-modifiers) :alt) true false))))

          ; Create a symmetrical random state
          (= k :G)                   (do (print-in-colour "Creating symmetrical random state..." CLR_LM)
                                         (q/background 0)
                                         (print-map (create-random-state state true 
                                            (if (contains? (q/key-modifiers) :alt) true false))))

          ; Toggle flag to save images as img<nnnnn>.png, to make a video sequence
          (= k :M)                   (do (when-not mvs (q/background 0))
                                         (assoc state 
                                              :making-video-seq (not mvs)
                                              :current-video-dir (if-not mvs
                                                                   (let [dir (create-video-dir)
                                                                         fname (str dir "/" dir)]
                                                                      (save-state-map state fname)
                                                                      dir)
                                                                    nil)
                                              :image-num -1))

          ; Print the iteration count 
          (= k :j)                   (do (print-in-colour (str "Iteration count: " (:level state))  CLR_LM) (flush) state) 
          
          ; Display last saved image but don't change state
          (= k :R)                   (do 
                                         (print-in-colour "Displaying last saved image but preserving current state" CLR_LM)
                                         (revert-to-last-image state))

          ; Reset the a, b, t, u, w params to default
          (= k :D)                   (do 
                                         (print-in-colour "Resetting a, b, t, u, w params to default" CLR_LM)
                                         (print-map (set-default-params state)))

          ; Reset the scale, x- and y- offsets, and cancel any mirroring
          (= k :S)                   (do
                                         (print-in-colour "Resetting scale, x- and y- offsets, and cancelling any mirroring" CLR_LM)
                                         (assoc state :x-scale 1.0 :y-scale 1.0 :x-shift 0 :y-shift 0 :mirror false))
          
          ; Reset to initial state, except that the polygon-order & variation fns remain the same
          (= k :Z)                   (reset-state state)    
          
          ; Wipe the blackboard clean ;)
          (= k :z)                   (do
                                        (q/background 0)
                                        (print-map (assoc state :level 0)))
          
          ; Vary :param-delta, used to vary the step 
          ; by which a parameter is incremented or decremented
          (= k :+)                   (assoc state :param-delta (* delta ROOT2))
          (= k :-)                   (assoc state :param-delta (/ delta ROOT2))

          ; Toggle whether the x, X, y, Y, e and E keys scale or shift the image
          (= k :%)                   (do 
                                        (print-in-colour (str (if sns "*Scal" "Shift") "ing* mode on.") CLR_LR)
                                        (assoc state :scale-not-shift sns))          
          
          ; Modify a, b, t, u, w parameters...
          (= k :a)                   (if (contains? (q/key-modifiers) :alt)
                                        (assoc state :a (- (:a state)))                  ; negate a
                                        (assoc state :a (- (:a state) delta)))
          (= k :A)                   (if (contains? (q/key-modifiers) :alt)
                                        (assoc state :a (if (zero? (:a state)) A 0.0))   ; toggle zeroise/reset a
                                        (assoc state :a (+ (:a state) delta)))
          (= k :b)                   (if (contains? (q/key-modifiers) :alt)
                                        (assoc state :b (- (:b state)))                  ; negate b
                                        (assoc state :b (- (:b state) delta)))
          (= k :B)                   (if (contains? (q/key-modifiers) :alt)
                                        (assoc state :b (if (zero? (:b state)) B 0.0))   ; toggle zeroise/reset b
                                        (assoc state :b (+ (:b state) delta)))
          (= k :t)                   (if (contains? (q/key-modifiers) :alt)
                                        (assoc state :t (- (:t state)))                  ; negate t
                                        (assoc state :t (/ (:t state) delta+1)))
          (= k :T)                   (if (contains? (q/key-modifiers) :alt)
                                        (assoc state :t (if (zero? (:t state)) T 0.0))   ; toggle zeroise/reset t
                                        (assoc state :t (* (:t state) delta+1)))
          (= k :u)                   (if (contains? (q/key-modifiers) :alt)
                                        (assoc state :u (- (:u state)))                  ; negate u
                                        (assoc state :u (/ (:u state) delta+1)))
          (= k :U)                   (if (contains? (q/key-modifiers) :alt)
                                        (assoc state :u (if (zero? (:u state)) U 0.0))   ; toggle zeroise/reset u
                                        (assoc state :u (* (:u state) delta+1)))
          (= k :w)                   (if (contains? (q/key-modifiers) :alt)
                                        (assoc state :w (- (:w state)))                  ; negate w
                                        (assoc state :w (- (:w state) (* delta 2.5))))
          (= k :W)                   (if (contains? (q/key-modifiers) :alt)
                                        (assoc state :w (if (zero? (:w state)) W 0.0))   ; toggle zeroise/reset w
                                        (assoc state :w (+ (:w state) (* delta 2.5))))

          ; Transforms / mirroring of the final image
            ; Translation & scaling...
                                      ; (note: if-not used as we set sns to the inverse of :scale-not-shift)
          (= k :x)                   (if-not sns (assoc state :x-scale (/ (:x-scale state) delta+1))
                                             (assoc state :x-shift (dec-by-x-delta (:x-shift state))))
          (= k :X)                   (if-not sns (assoc state :x-scale (* (:x-scale state) delta+1))
                                             (assoc state :x-shift (inc-by-x-delta (:x-shift state))))
          (= k :y)                   (if-not sns (assoc state :y-scale (/ (:y-scale state) delta+1))
                                             (assoc state :y-shift (dec-by-y-delta (:y-shift state))))
          (= k :Y)                   (if-not sns (assoc state :y-scale (* (:y-scale state) delta+1))
                                             (assoc state :y-shift (inc-by-y-delta (:y-shift state))))
          (= k :e)                   (if-not sns (assoc state :x-scale (/ (:x-scale state) delta+1)
                                                          :y-scale (/ (:y-scale state) delta+1))
                                             (assoc state :x-shift (dec-by-x-delta (:x-shift state))
                                                          :y-shift (dec-by-y-delta (:y-shift state))))
          (= k :E)                   (if-not sns (assoc state :x-scale (* (:x-scale state) delta+1)
                                                          :y-scale (* (:y-scale state) delta+1))
                                             (assoc state :x-shift (inc-by-x-delta (:x-shift state))
                                                          :y-shift (inc-by-y-delta (:y-shift state))))
            ; Reflection & mirroring...          
          (= k :r)                   (assoc state :reflectLR (not (:reflectLR state)))
          (= k :p)                   (assoc state :reflectUD (not (:reflectUD state)))
          (= k :m)                   (assoc state :mirror (not (:mirror state)))
            ; Rotation...
          (= k :O)                   (assoc state :rotate-by-half-pi (not (:rotate-by-half-pi state)))
          (= k :o)                   (assoc state :rotate-by-quarter-pi (not (:rotate-by-quarter-pi state)))
          (= k :*)                   (assoc state :rotate-by-half-a-sector (not (:rotate-by-half-a-sector state)))
          
          ; Colour stuff
          (= k :c)                   (let [cbs (not (:colour-by-speed state))]
                                       (print-in-colour 
                                        (str "Setting colours by " (if cbs "*speed*" "*curvature*")) CLR_LM)
                                       (assoc state :colour-by-speed cbs))
          (= k :i)                   (let [invcols (not (:invert-colours state))]
                                       (print-in-colour 
                                        (str "Colour inversion *O" (if invcols "N*" "FF*")) CLR_LM)
                                       (assoc state :invert-colours invcols))
          (= k :h)                   (let [nho (dec (:hue-offset state))]
                                       (println (str "Hue offset = " nho))
                                       (assoc state :hue-offset nho))
          (= k :H)                   (let [nho (inc (:hue-offset state))]
                                       (println (str "Hue offset = " nho))
                                       (assoc state :hue-offset nho))

          ; Change size of dots 
          (and (= k :<) (> size 0))  (assoc state :size (dec size))
          (and (= k :>) (< size 8))  (assoc state :size (inc size))

          ; Change variation fn, pre-transform and polygon order
          (= k :v)                   (let [vx (mod (dec vx) (count vfuncs))]
                                       (print-in-colour 
                                        (str "Variation function *" vx "* set.") CLR_LM) 
                                        (assoc state :variation vx))
          (= k :V)                   (let [vx (mod (inc vx) (count vfuncs))]
                                       (print-in-colour 
                                        (str "Variation function *" vx "* set.") CLR_LM) 
                                        (assoc state :variation vx))
          (= k :n)                   (assoc state :polygon-order (if (> n 63) (/ n 2) (if (> n 2) (dec n) n)))
          (= k :N)                   (assoc state :polygon-order (if (< n 64) (inc n) (* 2 n)))
          (= k :k)                   (let [ptx (mod (dec ptx) PRE-TRANS-FUNC-COUNT)]
                                       (print-in-colour 
                                        (str "Pre-transform *" ptx "* set.") CLR_LM) 
                                        (assoc state :pre-trans-index ptx))
          (= k :K)                   (let [ptx (mod (inc ptx) PRE-TRANS-FUNC-COUNT)]
                                       (print-in-colour 
                                        (str "Pre-transform *" ptx "* set.") CLR_LM) 
                                        (assoc state :pre-trans-index ptx))
          
          ; Boolean flags specifying transformation functions to be applied at various stages
          (= k :=)                   (let [pt (not (:pre-transform state))]
                                       (print-in-colour (str "Pre-transform *O" (if pt "N*" "FF*")) CLR_LM)
                                       (assoc state :pre-transform pt))
          (= k :!)                   (assoc state :reapply-vfunc (not (:reapply-vfunc state)))         
          (= k :f)                   (assoc state :apply-ballfold (not (:apply-ballfold state)))         
          (= k :?)                   (assoc state :swap-xy (not (:swap-xy state)))         
          (= k :P)                   (assoc state :treat-as-polar (not (:treat-as-polar state)))         
          (= k :$)                   (assoc state :polarise-vfunc (not (:polarise-vfunc state)))         
          (= k :.)                   (assoc state :root-pre-trans (not rpt) :sq-pre-trans false)
          (= k :/)                   (assoc state :sq-pre-trans (not spt) :root-pre-trans false)
          (= k :#)                   (assoc state :sq-pre-trans-components (not (:sq-pre-trans-components state))
                                                  :root-pre-trans-components false)
          (= k :')                   (assoc state :root-pre-trans-components (not (:root-pre-trans-components state))
                                                  :sq-pre-trans-components false)
          (= k :L)                   (print-map state)  ; print full state map
          
          ; For any other key, print a decluttered subset of the state map
          :else                      (do (print-map 
                                         (dissoc (into {} (map #(when (second %) %) state)) 
                                         nil :x0 :y0 :x :y :curvature :level 
                                         (when-not (:pre-transform state) :pre-trans-index :sq-pre-trans
                                          :root-pre-trans :sq-pre-trans-components :root-pre-trans-components)))
                                         state))))
