(require 'dash)
(require 'advent-utils)

(defconst day25/debug-buffer "* Day 25 *")

(defun day25/symbol-to-string (symbol)
  (or (case symbol
     (:> ">")
     (:v "v"))
      "."))

(defun day25/to-string (grid)
  (let ((result ""))
    (advent/-each-grid grid
      (when (and (zerop (cdr it-coord))
               (not (equal it-coord '(0 . 0))))
        (setq result (concat result "\n")))
      (setq result (concat result (day25/symbol-to-string it-value))))
    result))

(defun day25/debug--print (grid)
  (with-current-buffer (get-buffer-create day25/debug-buffer)
    (erase-buffer)
    (goto-char (point-min)) ;overkill probably
    (insert (day25/to-string grid))))

(defun day25/read-map (grid)
  (advent/-update-grid! grid
    (cond
     ((string= it ">") :>)
     ((string= it "v") :v)
     ((string= it ".") nil)
     ((t (error "Unexpected value!"))))))

(defun day25/evolve-down (grid))

(defun day25/evolve-right (grid))

(defun day25/evolve(grid)
  ()
  )


(defun day25/part-1 (lines)
  (error "Not yet implemented"))

(defun day25/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day25)

(setq example (day25/read-map (advent/read-grid 25 :example #'identity)))
(setq problem (day25/read-map (advent/read-grid 25 :problem #'identity)))
