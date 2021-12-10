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

(defun day10/check--counter (counter)
  "Returns the counter if correct or the offending bracket otherwise"
  (if-let ((offending-index (seq-position counter -1)))
      (elt '(:-r :-s :-c :-a) offending-index)
    counter))

(defun day10/sum--counters (counter added)
  (let ((result (--map (+ (car it) (cdr it))
                       (-zip counter added))))
    (print result)
    result))

(defun day10/update--counters (acc char)
  (day10/check--counter
   (day10/sum--counters
    acc
    (case char
      (:-r '(-1 0 0 0))
      (:+r '(1 0 0 0))
      (:-s '(0 -1 0 0))
      (:+s '(0 1 0 0))
      (:-c '(0 0 -1 0))
      (:+c '(0 0 1 0))
      (:-a '(0 0 0 -1))
      (:+a '(0 0 0 +1))))))

(defun day10/check--next (acc char)
  (if (not (listp acc))
      acc
    (day10/update--counters acc char)))

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

(defun day10/corrupted? (blocks)
  (let ((result (-reduce-from #'day10/reduce-element (list (car blocks)) (rest blocks))))
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

(defun day10/part-2 (lines)
  (error "Not yet implemented"))

(defvar example (day10/read-instructions (advent/read-problem-lines 10 :example)))
(defvar problem (day10/read-instructions (advent/read-problem-lines 10 :problem)))

(provide 'day10)
