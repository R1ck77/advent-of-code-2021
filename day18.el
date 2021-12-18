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

(defun day18/explode-left (number)
  number)

(defun day18/reverse (number)
  (if (numberp number)
      number
    (list (day18/reverse (cadr number))
          (day18/reverse (car number)))))

(defun day18/explode-right (number)
  (day18/reverse (day18/explode-left (day18/reverse number))))

(defun day18/explode-1 (number)
  (day18/explode-right (day18/explode-left number)))

(defun day18/explode (number)
  (day18/recur-until-equal number #'day18-explode-1))

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
