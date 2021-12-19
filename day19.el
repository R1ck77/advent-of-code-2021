(require 'dash)
(require 'advent-utils)

(defconst day19/spatial-transforms (list (lambda (x y z) (list x y z))
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
  "List of all spatial transformations, identity included")

(defun day19/count-matches (group1 group2)
  (if (> (length group1) (length group2))
      (day19/count-matches group2 group1)
    (let ((set (advent/table)))
      ;; let's hope that insertion costs more than extraction
      (--each group2 (advent/put set it t))
      (length (--filter (advent/get set it) group1)))))

(defun day19/visible-beacon? (point)
;;; TODO/FIXME bland optimization possible
  (let ((x (car point))
        (y (elt point 1))
        (z (elt point 2)))
    (and (>= x -1000)
         (<= x 1000)
         (>= y -1000)
         (<= y 1000)
         (>= z -1000)
         (<= z 1000)
         point)))

(defun day19/two-beacons-match-for-translation? (ref other translation)
  "Returns the translation if it results in at least 12 matches between ref and other or more"
  (let ((groups (-split-at 10 (-filter #'day19/visible-beacon? (--map (apply translation it) other)))))
    (let ((primary-group (cadr groups))
          (secondary-group (car groups)))
      (if (>= (length primary-group) 2)
       (let ((primary-matches (day19/count-matches ref primary-group)))
         (unless (or (< primary-matches 2)
                     ;; don't bother if the secondary group + 2 can't make to 12
                     (< (+ primary-matches (length secondary-group)) 12))
           (let ((all-matches (+ primary-matches (day19/count-matches ref secondary-group))))
             (and (>= all-matches 12) translation))))))))

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

(defun day19/create-valid-translation (dest src)
  "Creates a transform with 'create-translation' but returns nil if the point is not valid"
  (lexical-let ((transform (day19/create-translation dest src)))
    (lambda (x y z)
      (let ((result (funcall transform x y z)))
        (day19/visible-beacon? result)))))

(defun day19/oriented-beacons--match? (ref other)
  "Given the two groups of beacons in the same orientation, return the transform if there is a match"
  (let ((destination-points ref)
        (valid-translation))
    ;; for each point of the reference set
    (while (and (not valid-translation) destination-points)
      (let ((dst-point (pop destination-points))
            (source-points other))
        ;; for each point of the other set
        (while (and (not valid-translation) source-points)
          (let* ((src-point (pop source-points))
                 (translation (day19/create-translation dst-point src-point)))
            ;; if there is at least a 2 point match, add it to the 
            (when (day19/two-beacons-match-for-translation? ref other translation)
              (setq valid-translation translation))))))
    valid-translation))

(defun day19/two-scanners-match? (ref other)
  "Returns a couple of transforms, the rotation and the translation, or nil"
  (let ((remaining-transforms day19/spatial-transforms)
        (matching-transforms))
    (while (and (not matching-transforms) remaining-transforms)
      (let* ((current-rotation (pop remaining-transforms))
             (rotated-other (--map (apply current-rotation it) other)))
        (when-let ((translation (day19/oriented-beacons--match? ref rotated-other)))
          (setq matching-transforms (cons current-rotation translation)))))
    matching-transforms))

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
