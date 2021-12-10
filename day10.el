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

(defun day10/corrupted? (blocks))

(defun day10/part-1 (lines)
  (error "Not yet implemented"))

(defun day10/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day10)

