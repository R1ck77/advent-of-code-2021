(require 'dash)
(require 'advent-utils)

(defun day10/read-blocks (line)
  (--map (case it
           (?\( :+r)
           (?\) :-r)
           (?\[ :+s)
           (?\] :-s)
           (?\{ :+c)
           (?\} :-c)
           (?\< :+a)
           (?\> :-a))
         (-map #'string-to-char
               (split-string line "" t))))

(defun day10/read-instructions (lines)
  (-map #'day10/read-blocks lines))

(defun day10/get-matching-bracket (bracket)
  (case bracket
    (:-r :+r )
    (:+r :-r)
    (:-s :+s )
    (:+s :-s)
    (:-c :+c )
    (:+c :-c)
    (:-a :+a )
    (:+a :-a)))

(defun day10/reduce--accumulator (acc closing)
  (let ((expected (day10/get-matching-bracket closing)))
    (if (eq (car acc) expected)
        (rest acc)
      closing)))

(defun day10/reduce-element (acc char)
  (if (not (listp acc))
      acc ; non-list accumulator -> error occurred, so pipe that through the reduce
    (case char
      ((:+r :+s :+c :+a) (cons char acc))
      ((:-r :-s :-c :-a)  (day10/reduce--accumulator acc char))
      (t (error "Unexpected character received!")))))

(defun day10/reduce-blocks (blocks)
  (-reduce-from #'day10/reduce-element (list (car blocks)) (rest blocks)))

(defun day10/corrupted? (blocks)
  (let ((result (day10/reduce-blocks blocks)))
    (if (listp result)
        nil
      result)))

(defun day10/score-corruption (error-code)
  (if error-code
   (case error-code
     (:-r 3)
     (:-s 57)
     (:-c 1197)
     (:-a 25137)
     (t (error (format "Unexpected error code: %s" error-code))))
   0))

(defun day10/part-1 (lines)
  (apply #'+ (-map #'day10/score-corruption
                   (-map #'day10/corrupted?
                         (day10/read-instructions lines)))))

(defun day10/missing-brackets (blocks)
  (let ((reduced (day10/reduce-blocks blocks)))
    (if (listp reduced)
        (-map #'day10/get-matching-bracket reduced))))

(defun day10/tool-score (bracket)
  (case bracket
    (:-r 1)
    (:-s 2)
    (:-c 3)
    (:-a 4)
    (t (error (format "Unexpected bracket: %s" bracket)))))

(defun day10/score-missing-brackets (brackets-list)
  (--reduce-from (+ (* 5 acc) (day10/tool-score it)) 0 brackets-list))

(defun day10/get-middle-value (scores)
  (elt (sort (copy-sequence scores) #'<) (/ (length scores) 2)))

(defun day10/part-2 (lines)
  (day10/get-middle-value
   (-map #'day10/score-missing-brackets
         (-non-nil
                  (-map #'day10/missing-brackets
                        (day10/read-instructions lines))))))

(provide 'day10)
