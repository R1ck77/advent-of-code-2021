(require 'dash)
(require 'advent-utils)

(defconst day19/transforms (list
                            (lambda (x y z) (list x y z))
                            (lambda (x y z) (list x z (- y)))
                            (lambda (x y z) (list x (- z) y))
                            (lambda (x y z) (list x (- y) (- z)))
                            (lambda (x y z) (list (- x) y (- z)))
                            (lambda (x y z) (list (- x) (- y) z))
                            (lambda (x y z) (list (- x) (- z) (- y)))
                            (lambda (x y z) (list (- x) z y))
                            (lambda (x y z) (list z y (- x)))
                            (lambda (x y z) (list z (- y) x))
                            (lambda (x y z) (list z x y))
                            (lambda (x y z) (list z (- x) (- y)))
                            (lambda (x y z) (list (- z) y x))
                            (lambda (x y z) (list (- z) (- y) (- x)))
                            (lambda (x y z) (list (- z) (- x) y))
                            (lambda (x y z) (list (- z) x (- y)))
                            (lambda (x y z) (list y (- x) z))
                            (lambda (x y z) (list y x (- z)))
                            (lambda (x y z) (list y (- z) (- x)))
                            (lambda (x y z) (list y z x))
                            (lambda (x y z) (list (- y) (- x) (- z)))
                            (lambda (x y z) (list (- y) x z))
                            (lambda (x y z) (list (- y) z (- x)))
                            (lambda (x y z) (list (- y) (- z) x)))
  "List of all spatial transformations, identities included")

(defun day19/read-coordinate (line)
  (-map #'string-to-number
        (split-string line "," t)))

(defun day19/read-title (line)
  (string-to-number (elt (split-string line " " t) 2)))

(defun day19/read-scan (lines)
  ;;(print (format "Reading probe '%d'" (day19/read-title (car lines))))
  (-map #'day19/read-coordinate (cdr lines)))

(defun day19/read-scans (blocks)
  (-map #'day19/read-scan blocks))

(defun day19/build-list (scans))

(defun day19/part-1 (lines)
  (error "Not yet implemented"))

(defun day19/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day19)
