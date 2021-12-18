(require 'dash)
(require 's)
(require 'advent-utils)

(defun day18/read-number (line)
  (let ((clean-text (s-replace "," " "
                               (s-replace "]" ")"
                                          (s-replace "[" "(" line)))))
    (eval (car (read-from-string (concat "'" clean-text))))))

(defun day18/recur-until-equal (value f)
  (let ((transformed value))
    (setq value nil)
    (while (not (equal transformed value))
      (setq value transformed)
      (setq transformed (funcall f value)))
    transformed))

(defun day18/intermission (value &optional level)
  "Find the leftmost 4th level"
  (unless (numberp value)
    (let ((level (or level 0)))
      (when (<= level 4)       
          (or (day18/intermission (car value) (1+ level))
              (day18/intermission (cadr value) (1+ level)))
          value))))

(defun day18/acc--parentship (value level left right)
  (if (numberp value)
      (vector number)
    (let ((next-level (1+ level))))
    (list (vector level left right)
          (let ((new-left (day18/acc--parentship (car value) next-level left wrong-right))
                (new-right(day18/acc--parentship (cadr value) next-level wrong-left right)))
            (aset new-left))
          (list new-left new-right))))

(defun day18/get--leftmost-value (value)
  (if (numberp value)
      value
    (day18/get--leftmost-value (car value))))

(defun day18/explode-left (value)
  value)

(defun day18/reverse (value)
  (if (numberp value)
      value
    (list (day18/reverse (cadr value))
          (day18/reverse (car value)))))

(defun day18/explode-right (value)
  (day18/reverse (day18/explode-left (day18/reverse value))))

(defun day18/explode-1 (value)
  (day18/explode-right (day18/explode-left value)))

(defun day18/explode (value)
  (day18/recur-until-equal value #'day18-explode-1))

(defun day18/split-number (number)
  (let ((remainder (mod number 2))
        (division (/ number 2)))
    (list division (+ division remainder))))

(defun day18/split (value)
  (if (listp value)
      (-map #'day18/split value)
    (if (< value 10)
        value
      (day18/split-number value))))

(defun day18/reduce-1 (number)
  (day18/split (day18/explode number)))

(defun day18/reduce (number)
  (day18/recur-until-equal number #'day18/reduce-1))

(defun day18/sum-all (numbers)
  (day18/reduce (--reduce (list acc it) numbers)))

(defun day18/magnitude (number)
  (if (numberp number)
      number
    (+ (* 3 (day18/magnitude (car number)))
       (* 2 (day18/magnitude (cadr number))))))

(defun day18/part-1 (lines)
  (day18/magnidue
   (day18/sum-all (-map #'day18/read-number lines))))

(defun day18/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day18)
