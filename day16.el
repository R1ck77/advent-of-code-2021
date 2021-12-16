(require 'dash)
(require 'advent-utils)

(defun day16/char-to-bin (char)
  (case char
    (?0  (list 0 0 0 0))
    (?1  (list 0 0 0 1))
    (?2  (list 0 0 1 0))
    (?3  (list 0 0 1 1))
    (?4  (list 0 1 0 0))
    (?5  (list 0 1 0 1))
    (?6  (list 0 1 1 0))
    (?7  (list 0 1 1 1))
    (?8  (list 1 0 0 0))
    (?9  (list 1 0 0 1))
    (?A  (list 1 0 1 0))
    (?B  (list 1 0 1 1))
    (?C  (list 1 1 0 0))
    (?D  (list 1 1 0 1))
    (?E  (list 1 1 1 0))
    (?F  (list 1 1 1 1))))

(defun day16/hex2bin (line)
  (--reduce-from (append acc (day16/char-to-bin it))
                 '()
                 (-map #'string-to-char (split-string line "" t))))

(defun day16/part-1 (lines)
  (error "Not yet implemented"))

(defun day16/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day16)
