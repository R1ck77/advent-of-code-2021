(require 'dash)
(require 'advent-utils)

(defun day9/read-line (line)
  (apply #'vector (-map #'string-to-number (split-string line "" t))))

(defun day9/read-map (lines)
  (apply #'vector (-map #'day9/read-line lines)))

(defun day9/get (map row column)
  (condition-case nil
      (aref (aref map row) column)
    (error nil)))

(defun day9/get-neighbors (map row column)
  "Return a list with: center value, N, E, S,W values, possibly nil"
  (list (day9/get map row column)
        (day9/get map (1- row) column )
        (day9/get map row (1+ column) )
        (day9/get map (1+ row) column )
        (day9/get map row (1- column) )))

(defun day9/is-minimum? (map row column)
  "Return the value at position if it's a minimum, or nil otherwise"
  (let* ((neighbors (day9/get-neighbors map row column))
        (center (car neighbors)))
    (unless (--any (<= it center) (-non-nil (rest neighbors)))
      center)))

(defun day9/get-map-size (map)
  (list (length map)
        (length (elt map 0))))

(defun day9/sum-minima-risk (map)
  (let ((size ))
    (apply #'+ (-map #'1+
           (-non-nil
            (--map (day9/is-minimum? map (car it) (cdr it))
                   (apply #'advent/create-coordinates
                          (day9/get-map-size map))))))))

(defun day9/part-1 (lines)
  (day9/sum-minima-risk (day9/read-map lines)))

(defun day9/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day9)

;;(defvar example (day9/read-map (advent/read-problem-lines 9 :example)))
;;(defvar problem (day9/read-map (advent/read-problem-lines 9 :problem)))
