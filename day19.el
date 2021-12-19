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

(defun day19/visible-beacon? (point)
;;; TODO/FIXME bland optimization possible
  (let ((x (car point))
        (y (elt point 1))
        (z (elt point 2)))
    (and (>= x -1000)
         (<= x 1000)
         (>= y -1000)
         (<= y 1000)
         (>= z 1000)
         (<= z 1000))))

(defun day19/two-point-match-for-translation? (ref other translation))

(defun day19/create-translation (dest src)
  "Creates a transform that turns src point into dst points

Can be used to move a point from the 'other' set to the 'reference' set"
  (lexical-let ((dx (- (car dest) (car src)))
                (dy (- (elt dest 1) (elt src 1)))
                (dz (- (elt dest 2) (elt src 2))))
    (lambda (x y z)
      (list (+ x dx)
            (+ y dy)
            (+ z dz)))))

(defun day19/two--point--match? (ref other)
  "Given the two groups of beacons in the same orientation, return the list of transforms with *at least* a two point match

This is a preliminary check, if a transform works here, it's worth re-checking more thoroughly"
  (let ((destination-points ref)
        (candidates)))
  ;; for each point of the reference set
  (while destination-points
    (let ((dest-point (pop translations))
          (source-points other))
      ;; for each point of the other set
      (while source-points
        (let ((translation (day19/create-translation dest-point (pop source-point))))
          ;; if there is at least a 2 point match, add it to the 
          (when (day19/two-point-match-for-translation? ref other translation)
            (push translation candidates))))))
  candidates)

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
