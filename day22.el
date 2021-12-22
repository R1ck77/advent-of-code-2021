(require 'dash)
(require 'advent-utils)

(defun day22/read-interval (token)
  (let ((as-list (-map #'string-to-number
                       (split-string (cadr (split-string token "=" t)) "[.]+" t))))
    (cons (car as-list)
          (cadr as-list))))

(defun day22/read-state (token)
  (cond
   ((string= token "on") 1)
   ((string= token "off") 0)
   (t (error "Unexpected token"))))

(defun day22/read-step (line)
  (let* ((state-coords (split-string line " " t))
         (coords (-map #'day22/read-interval (split-string (cadr state-coords) "," t))))
    (list coords (day22/read-state (car state-coords)))))

(defun day22/read-steps (lines)
  (-map #'day22/read-step lines))

;; TODO/FIXME not sure it's correct. What if a cube is partially outside?
(defun day22/bound-in-50-region? (min-max)
  (and (>= (car min-max) -50)
       (<= (cdr min-max) 50)))

(defun day22/in-restricted-50-region? (step)
  (let ((bounds (car step)))
    (and (day22/bound-in-50-region? (elt bounds 0))
         (day22/bound-in-50-region? (elt bounds 1))
         (day22/bound-in-50-region? (elt bounds 2)))))

(defun day22/restrict-steps (steps)
  (-filter #'day22/in-restricted-50-region? steps))

(defun day22/split-coordinate (base step)
  "Returns lists of coordinates, or nil if there is nothing to do"
  (cond
   ;; step completely outside: no intersection
   ((or (> (car base) (cdr step))
        (< (cdr base) (car step)))
    nil) ;; special code for "nothing to do"
   ;; step dips in the cube from the left
   ((and (>= (cdr step) (car base))
         (< (car step) (car base)))
    (if (>= (cdr step) (cdr base))
        ;; step contains the base
        (list (cons (car base) (cdr base)))
      ;; step leaves a sliver out
      (list (cons (car base) (cdr step))
            (cons (1+ (cdr step)) (cdr base)))))
   ;; step trails out of the cube to the right
   ((and (> (cdr step) (cdr base))
         (<= (car step) (cdr base)))
    (if (<= (car step) (car base))
        ;; step contains base
        (list (cons (car base) (cdr base)))
      ;; step leaves a sliver out
      (list (cons (car base) (1- (car step)))
            (cons (car step) (cdr base)))))
   ;; step contains/overlaps the base completely
   ((and (<= (car step) (car base))
         (>= (cdr step) (cdr base)))
    (list base))
   ;; step properly contained in the base
   ((and (<= (car base) (car step))
         (>= (cdr base) (cdr step)))
    (cond
     ;; complete overlap
     ((and (= (car base) (car step))
           (= (cdr base) (cdr step)))
      (list base))
     ;; left overlap, 1 split
     ((= (car base) (car step))
      (list (cons (car base) (cdr step))
            (cons (1+ (cdr step)) (cdr base))))
     ;; right overlap, 1 split
     ((= (cdr base) (cdr step))
      (list (cons (car base) (1- (car step)))
            (cons (car step) (cdr base))))
     ;; no overlap, 2 splits
     (t (list (cons (car base) (1- (car step)))
              (cons (car step) (cdr step))
              (cons (1+ (cdr step)) (cdr base))))))
   (t (error "Unsupported case"))))

(defun day22/count-on (cube)
  (if (zerop (cadr cube))
      0
    (let ((coords (car cube)))
     (* (let ((x-coords (elt coords 0)))
          (1+ (- (cdr x-coords) (car x-coords))))
        (let ((y-coords (elt coords 1)))
          (1+ (- (cdr y-coords) (car y-coords))))
        (let ((z-coords (elt coords 2)))
          (1+ (- (cdr z-coords) (car z-coords))))))))

(defun day22/count-all-cubes (cubes)
  (apply #'+ (-map #'day22/count-on cubes)))

(defun day22/slice-cube (base step)
  "Slice a cube using the step"
  (let ((x-slices (day22/split-coordinate (elt base 0)
                                          (elt step 0)))
        (y-slices (day22/split-coordinate (elt base 1)
                                          (elt step 1)))
        (z-slices (day22/split-coordinate (elt base 2)
                                          (elt step 2)))
        (result))
    (and x-slices y-slices z-slices
         (loop for i in x-slices do
               (loop for j in y-slices do
                     (loop for k in z-slices do
                           (push (list i j k) result)))))
    result))

(defun day22/coordinate-contained? (base step)
      (and (>= (car base) (car step))
           (<= (cdr base) (cdr step))))

(defun day22/properly-contained? (base step)
  (and (day22/coordinate-contained? (elt base 0)
                                    (elt step 0))
       (day22/coordinate-contained? (elt base 1)
                                    (elt step 1))
       (day22/coordinate-contained? (elt base 2)
                                    (elt step 2))))

(defun day22/split-cube (base step)
  "Returns a list of cubes (or nil if there is no overlap) which are either completely contained or not overlapping with step."
  (if (day22/properly-contained? base step)
      (list base)
    (day22/slice-cube base step)))

(defun day22/within? (cube rule)
  "Returns t if the x coordinate of the cube is within the x coordinate of the rule"
  (and (let ((cube-x (elt cube 0))
             (rule-x (elt rule 0)))
         (and (<= (cdr cube-x) (cdr rule-x))
              (>= (car cube-x) (car rule-x))))
       (let ((cube-y (elt cube 1))
             (rule-y (elt rule 1)))
         (and (<= (cdr cube-y) (cdr rule-y))
              (>= (car cube-y) (car rule-y))))
       (let ((cube-z (elt cube 2))
             (rule-z (elt rule 2)))
         (and (<= (cdr cube-z) (cdr rule-z))
              (>= (car cube-z) (car rule-z))))))

(defun day22/flip (value)
  (- 1 value))

(defun day22/apply-rule-on-cube (cube rule)
  "Apply the rule to cube, returning an (always non null) list of cubes"
  (let ((cube-coords (car cube))
        (cube-state (cadr cube))
        (rule-coords (car rule))
        (rule-state (cadr rule)))
    ;; already turned on: nothing to do
    (if (= rule-state cube-state)
        (list cube)
      (let ((sliced (day22/split-cube cube-coords rule-coords))
            (new-state (day22/flip cube-state)))
        (if (not sliced)
            ;; no overlap!
            (list cube)
          ;;otherwise the remaining cubes have either complete overlap or none at all
          ;; I can check the first coordinate only          
          (let ((result (--map (list it (if (day22/within? it rule-coords)
                                            rule-state
                                          cube-state))
                               sliced)))
            result))))))

(defun day22/apply-rule-to-all (cubes rule)
  (-reduce #'append (--map (day22/apply-rule-on-cube it rule) cubes)))

(defun day22/evolve-reactor (cubes rules)
  (while rules
    (print (length rules))
    (let* ((rule (pop rules))
           (new-cubes (day22/apply-rule-to-all cubes rule)))
      (setq cubes new-cubes)))
  cubes)

(defun day22/part-1 (lines)
  (let ((rules (day22/restrict-steps (day22/read-steps lines)))
        (region '((-50 . 50) (-50 . 50) (-50 . 50))))
    (day22/count-all-cubes
           (day22/evolve-reactor (list (list region 0))
                                 rules))))

(defun day22/union--c (pair-1 pair-2)
  (cons (min (car pair-1) (car pair-2))
        (max (cdr pair-1) (cdr pair-2))))

(defun day22/union (cube-a cube-b)
  (list
   (day22/union--c (elt cube-a 0)
                   (elt cube-b 0))
   (day22/union--c (elt cube-a 1)
                   (elt cube-b 1))
   (day22/union--c (elt cube-a 2)
                   (elt cube-b 2))))

(defun day22/get-region-extremes (rules)
  (-reduce #'day22/union (-map #'car rules)))

(defun day22/part-2 (lines)
  (let* ((rules (day22/read-steps lines))
         (region (day22/get-region-extremes rules)))
    (day22/count-all-cubes
     (day22/evolve-reactor (list (list region 0))
                           rules))))

(provide 'day22)

(setq example (day22/read-steps (advent/read-problem-lines 22 :example)))
(setq problem (day22/read-steps (advent/read-problem-lines 22 :problem)))


