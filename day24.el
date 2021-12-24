(require 'dash)
(require 's)
(require 'advent-utils)

(defun day24/tokenize (string)
  (intern (format ":%s" string)))

(defun day24/read-operand (string)
  (if (s-numeric? string)
      (string-to-number string)
    (day24/tokenize string)))

(defun day24/read-instruction (string)
  (day24/tokenize string))

(defun day24/read-opcode (line)
  (let ((strings (split-string line " " t)))
    (cons (day24/read-instruction (pop strings))
          (-map #'day24/read-operand strings))))

(defun day24/read-opcodes (lines)
  (-map #'day24/read-opcode lines))

(defun day24/part-1 (lines)
  (error "Not yet implemented"))

(defun day24/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day24)

(setq example (day24/read-opcodes (advent/read-problem-lines 24 :problem)))
