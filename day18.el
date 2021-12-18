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

(defun day18/add-to-vector! (n value)
  (aset value 0 (+ n (aref value 0))))

(defun day18/add-left-to-pair! (n value)
  (if (vectorp value)
      (day18/add-to-vector! n value)
    (day18/add-to-vector! n (cadr value))))

(defun day18/add-right-to-pair! (n value)
  (if (vectorp value)
      (day18/add-to-vector! n value)
    (day18/add-to-vector! n (car value))))

(defun day18/explode-wrapped! (wrapped)
  "Explode the first pair, returns the original list if nothing to do"
  (let* ((flattened (-flatten-n 3 wrapped))
        (index-of-list (-find-index #'listp flattened)))
    (when index-of-list
      (let* ((pair (elt flattened index-of-list))
             (left (aref (car pair) 0))
             (right (aref (cadr pair) 0)))
        (setcar pair :boom)
        (if (> index-of-list 0) (day18/add-left-to-pair! left (elt flattened (1- index-of-list))))
        (if-let ((right-hand (elt flattened (1+ index-of-list))))
            (day18/add-right-to-pair! right right-hand))))
    wrapped))

(defun day18/unwrap (wrapped)
  (cond
   ((vectorp wrapped) (aref wrapped 0))
   ((and (listp wrapped) (eq (car wrapped) :boom)) 0)
   ((listp wrapped) (list (day18/unwrap (car wrapped)) (day18/unwrap (cadr wrapped))))
   (t (error (format "Unexpected element: '%s'" wrapped)))))

(defun day18/wrap-numbers (value)
  (if (numberp value)
      (vector value)
    (list (day18/wrap-numbers (car value))
          (day18/wrap-numbers (cadr value)))))

(defun day18/explode-1 (value)
  (day18/unwrap (day18/explode-wrapped! (day18/wrap-numbers value))))

(defun day18/explode (value)
  (day18/recur-until-equal value #'day18/explode-1))

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

(defun day18/sum (a b)
  (day18/reduce (list a b)))

(defun day18/sum-all (numbers)
  (--reduce-from (day18/sum acc it) (car numbers) (rest numbers)))

(defun day18/magnitude (number)
  (if (numberp number)
      number
    (+ (* 3 (day18/magnitude (car number)))
       (* 2 (day18/magnitude (cadr number))))))

(defun day18/part-1 (lines)
  (day18/magnitude
   (day18/sum-all (-map #'day18/read-number lines))))

(defun day18/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day18)

(setq v (day18/read-number "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"))
