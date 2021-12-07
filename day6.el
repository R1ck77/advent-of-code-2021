(require 'dash)
(require 'advent-utils)

(defun day6/read-fishes (line)
  (-map #'string-to-number (split-string line "," t)))

(defun day6/create-empty-counter ()
  "Create a counter with all slots reset to 0"
  (make-vector 9 0))

(defun day6/bin-fishes (fishes)
  "Create a counter with the starting status of the fishes"
  (let ((counter (day6/create-empty-counter)))
    (--each fishes (aset counter it (1+ (elt counter it))))
    counter))

(defun day6/format-counter (counter)
  (let ((message ""))
   (--each (number-sequence 0 8)
     (setq message (concat message (format "%d -> %d\n" it (elt counter it)))))
   message))

(defun day6/increment-fishes (_ added base)
  (+ added base))

(defun day6/evolve (counter)
  "Return a new table with the structure"
  (let ((new-counter (day6/create-empty-counter)))
    ;; Reduce all "safe" timers, from 8 to 1 included
    (--each (number-sequence 1 8)
      (aset new-counter (1- it) (elt counter it)))
    ;; Generate new fishes
    (let ((0-fishes (elt counter 0)))
      ;; Add newly born fishes to 8
      (aset new-counter 8 (+ 0-fishes (elt new-counter 8)))
      ;; Reset the mother to 6
      (aset new-counter 6 (+ 0-fishes (elt new-counter 6))))
    new-counter))

(defun day6/count-fishes (counter)
  (apply #'+ (append counter nil)))

(defun day6/part-1 (line)
  (day6/count-fishes
   (advent/iterate #'day6/evolve
                   (day6/bin-fishes (day6/read-fishes line))
                   80)))

(defun day6/part-2 (line)
  (day6/count-fishes
   (advent/iterate #'day6/evolve
                   (day6/bin-fishes (day6/read-fishes line))
                   256)))

(provide 'day6)
