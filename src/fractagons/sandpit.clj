;;; ##################################################################################################
;; Playground...

(def intern-ns (partial intern *ns*))

(defn defvar 
  "Define a var named by a runtime-created string"
  [str val]
  (intern-ns (symbol str) val))

(defn valof [x] x)

(defmacro getval [str] `(list valof ~(symbol str)))

;;; ##################################################################################################


(defn fix-maps
"Read in all old maps in a directory, modify and resave"
   [dir]
   (let [frm-list (drop 1 (file-seq fdir))]      ; first "file object" is the directory itself
      (map #(-> % slurp read-string fix-map (save-state-map (str "/home/john/--scratch/" (dock-string (.getName %) 4)))) frm-list)))

(defn fix-map
"Read in an old frm map, and return an updated version that works with (polygon-j)"
   [m]
   (let [{:keys [t u w]} m
         a               (- w (* t u))
         b               0.0
         u               t]
      (-> m (assoc :a a :b b :u u) (dissoc :w))))
;;; ********* OR (-> m (dissoc :navigation-2d)))

(def fdir (clojure.java.io/file "/home/john/-scratch/"))
(def fdir2 (clojure.java.io/file "/home/john/--scratch/"))
