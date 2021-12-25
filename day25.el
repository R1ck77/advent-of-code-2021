(require 'dash)
(require 'advent-utils)

(defun day25/read-map (grid)
  (advent/-update-grid! grid
    (cond
     ((string= it ">") :>)
     ((string= it "v") :v)
     ((string= it ".") nil)
     ((t (error "Unexpected value!"))))))

(defun day25/part-1 (lines)
  (error "Not yet implemented"))

(defun day25/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day25)

(setq example (day25/read-map (advent/read-grid 25 :example #'identity)))
(setq problem (day25/read-map (advent/read-grid 25 :problem #'identity)))
