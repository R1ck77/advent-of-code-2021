(require 'dash)
(require 'advent-utils)

;;; Arbitrary! I have no idea whether a solution exists for vy = 1e39. Is there a mathematical proof?
(defconst max-vy-search 201)

(defun day17/read-target (line)
  (string-match "target area: x=\\([-0-9]+\\)\.\.\\([-0-9]+\\), y=\\([-0-9]+\\)\.\.\\([-0-9]+\\)" line)
  (list :x-min (string-to-number (match-string 1 line))
        :x-max (string-to-number (match-string 2 line))
        :y-min (string-to-number (match-string 3 line))
        :y-max (string-to-number (match-string 4 line))))


(defun day17/vertical-hit? (target vy)
  "Returns the maximum-position if the probe hits the mark, nil otherwise"
  (let ((overshot)
        (hit)
        (pos 0)
        (max-pos 0))
    (while (not (or overshot hit))
      (setq pos (+ pos vy))
      (setq max-pos (max max-pos pos))
      (setq vy (1- vy))
      (setq overshot (< pos (plist-get target :y-min)))
      (setq hit (and (not overshot)
                     (<= pos (plist-get target :y-max))))
      (when hit
        (print
         (format "Hit at %d!" vy))
        (redisplay)))
    (and (not overshot) (list vy max-pos))))

(defun day17/compute-max-vy (target top-vy)
  (let ((result (car (nreverse (-non-nil 
                   (--map (day17/vertical-hit? target it)
                          (number-sequence 0 (or top-vy 1001))))))))
   (cadr result)))

(defun day17/part-1 (line)
  (day17/compute-max-vy (day17/read-target line) max-vy-search))

(defun day17/max-x-speed (target)
  "Maximum x speed that *will* overshot the target"
  (1+ (plist-get target :x-max)))

(defun day17/step-x (x vx)
  (list (+ x vx) (max 0 (- vx 1))))

(defun day17/evolve-x-until-stop (vx)
  "Returns a list with all x positions reached by the probe, in inverted order (from 0 included)"
  (let ((state (list 0 vx))
        (positions (list 0)))
    (while (not (zerop (cadr state)))
      (setq state (apply #'day17/step-x state))
      (setq positions (cons (car state) positions)))
    positions))

(defun day17/min-y-speed (target)
  "Minimum y speed that *will* overshot the target"
  (1- (plist-get target :y-min)))

(defun day17/compute-min-x-speed (target)
  "Minimum x speed that won't stop short of the target"
  (car
   (--filter (>= (car (day17/evolve-x-until-stop it))
                 (plist-get target :x-min))
             (number-sequence 1 (plist-get target :x-min)))))

(defun day17/compute-top (vy)
  (assert (> vy 0))
  (/ (* vy (1+ vy)) 2))

(defun day17/compute-max-y-speed (target))

(defun day17/overshot? (target x-y)
  (or (> (car x-y) (plist-get target :x-max))
      (< (cadr x-y) (plist-get target :y-min))))

(defun day17/hit? (target x-y)
  (let ((x (car x-y))
        (y (cadr x-y)))
   (and (>= x (plist-get target :x-min))
        (<= x (plist-get target :x-max))
        (>= y (plist-get target :y-min))
        (<= y (plist-get target :y-max)))))

(defun day17/step (p v)
  (let ((new-p (list (+ (car p) (car v))
                     (+ (cadr p) (cadr v))))
        (new-v (list (max 0 (- (car v) 1))
                     (1- (cadr v)))))
    (list new-p new-v)))

(defun day17/will-hit? (target vx-vy)
  "Returns nil if the target doesn't hit the mark or if the maximum height doesn't top the record"
  (let ((pos (list 0 0))
        (v vx-vy)
        (target-hit nil)
        (stop nil))
    (while (not stop)
      (let ((newpos-newv (day17/step pos v)))
        (setq pos (car newpos-newv))
        (setq target-hit (or target-hit (day17/hit? target pos)))
        (setq v (cadr newpos-newv))
        (setq stop (or target-hit (day17/overshot? target pos)) )))
    target-hit))


(defun day17/compute-all-hit (target vx-min vx-max vy-min vy-max)
  (let ((vx vx-min)
        (hits))
    (loop for vx from vx-min upto vx-max do
          (loop for vy from vy-min upto vy-max do
                (if (day17/will-hit? target (list vx vy))
                    (push (list vx vy) hits))))
    hits))

(defun day17/part-2 (line)
  (let ((target (day17/read-target line)))
    (let ((vx-min (day17/compute-min-x-speed target))
          (vx-max (day17/max-x-speed target))
          (vy-min (day17/min-y-speed target))
          (vy-max (day17/compute-max-vy target max-vy-search)))
      (length (day17/compute-all-hit target vx-min vx-max vy-min vy-max)))))

(provide 'day17)
