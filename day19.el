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

(defun day19/debug-print (value)
  (print value)
  (redisplay))

(defun day19/count-matches (group1 group2)
  (if (> (length group1) (length group2))
      (day19/count-matches group2 group1)
    (let ((set (advent/table)))
      ;; let's hope that insertion costs more than extraction
      (--each group2 (advent/put set it t))
      (length (--filter (advent/get set it) group1)))))

(defun day19/visible-beacon? (point)
  point)

(defun day19/two-beacons-match-for-translation? (ref other translation)
  "Returns the translation if it results in at least 12 matches between ref and other or more"
  (let ((transformed-points (--map (apply translation it) other)))
    (if (>= (length ref) 12) ;; if I have reduce ref so much, forget about it!
        (let ((groups (-split-at 10 (-filter #'day19/visible-beacon? transformed-points))))
          (let ((primary-group (cadr groups))
                (secondary-group (car groups)))
            (if (>= (length primary-group) 2)
                (let ((primary-matches (day19/count-matches ref primary-group)))
                  (unless (or (< primary-matches 2)
                              ;; don't bother if the secondary group + 2 can't make to 12
                              (< (+ primary-matches (length secondary-group)) 12))
                    (let ((all-matches (+ primary-matches (day19/count-matches ref secondary-group))))
                      (and (>= all-matches 12) transformed-points))))))))))

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

(defun day19/filter-ref (ref inverse-translation)
  (--filter (day19/visible-beacon? (apply inverse-translation it)) ref))

(defun day19/oriented-beacons--match? (ref other)
  "Given the two groups of beacons in the same orientation, return the transform if there is a match"
  (let ((destination-points ref)
        (result))
    ;; for each point of the reference set
    (while (and (not result) destination-points)
      (let ((dst-point (pop destination-points))
            (source-points other))
        ;; for each point of the other set
        (while (and (not result) source-points)
          (let* ((src-point (pop source-points))
                 (translation (day19/create-translation dst-point src-point))
                 (validated-reference (day19/filter-ref ref (day19/create-translation src-point dst-point))))
            (setq result (day19/two-beacons-match-for-translation? validated-reference other translation))))))
    result))

(defun day19/two-scanners-match? (ref other)
  "Returns the other points in the new reference if there is a match"
  (let ((remaining-transforms day19/spatial-transforms)
        (result))
    (while (and (not result) remaining-transforms)
      (let* ((current-rotation (pop remaining-transforms))
             (rotated-other (--map (apply current-rotation it) other)))
        (setq result (day19/oriented-beacons--match? ref rotated-other))))
    result))

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

(defun day19/accumulate-points (all-points indexed-scan)
  (let ((initial-size (hash-table-count all-points)))
    (--each (cdr indexed-scan) (advent/put all-points it t))
    (let ((new-size (hash-table-count all-points)))
      (day19/debug-print (format "Accumulated %d: %d -> %d (+%d)"
                                 (car indexed-scan)
                                 initial-size
                                 new-size
                                 (- new-size initial-size))))))

(defun day19/add-converted-points (all-points ref-scan other-scans)
  (day19/debug-print (format "Checking %s vs %s" (car ref-scan) (-map #'car other-scans)))
  (if other-scans
      (let ((branches)
         (remainders))
     (while other-scans
       (let* ((current-element (pop other-scans))
              (current-index (car current-element))
              (current-data (cdr current-element)))
         (let ((new-set-or-nil (day19/two-scanners-match? (cdr ref-scan) current-data)))
           (if (not new-set-or-nil)
               (push current-element remainders)
             (progn
               (day19/accumulate-points all-points (cons current-index new-set-or-nil))
               (push (cons current-index new-set-or-nil) branches))))))
     (day19/debug-print (format "Branches: %s" (-map #'car branches)))
     (day19/debug-print (format "To Check: %s" (-map #'car remainders)))
     ;; Remove
     (setq other-scans :invalid)
     (let ((new-remainders (--reduce-from (day19/add-converted-points all-points it acc) remainders  branches)))
       new-remainders))))

(defun day19/find-all-connections (scans)
  "Returns a list of pairs of transforms"
  (let* ((n-scans (length scans))
         (indexed-scans (--map-indexed (cons it-index it) scans))
        (all-points (advent/table))
        (0-scan (car indexed-scans)))
    (day19/accumulate-points all-points 0-scan)
    (day19/add-converted-points all-points 0-scan (rest indexed-scans))
    (print (format "found %d points"(hash-table-count all-points)))
    (sort (advent/-map-hash all-points it-key) (lambda (a b) (< (car a) (car b))))))

(defun day19/part-1 (blocks)
  (length
   (day19/find-all-connections (day19/read-scans blocks))))

(defun day19/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day19)

(setq example (day19/read-scans (advent/read-blocks-of-lines 19 :example)))
(setq problem (day19/read-scans (advent/read-blocks-of-lines 19 :problem)))
