(require 'dash)
(require 'advent-utils)

(defun day24/read-operand (token)
  ())

(defun day24/read-instruction (token)
  ())

(defun day24/read-opcode (line)
  (let ((tokens (split-string line " " t)))
    (list (day24/read-instruction (elt tokens 0))
          (day21/read-operand (elt tokens 1))
          (day21/read-operand (elt tokens 2)))))

(defun day24/read-opcodes (lines)
  (-map #'day24/read-opcode lines))

(defun day24/part-1 (lines)
  (error "Not yet implemented"))

(defun day24/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day24)
