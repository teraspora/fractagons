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
