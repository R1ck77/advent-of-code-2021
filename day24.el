(require 'dash)
(require 's)
(require 'advent-utils)

(defconst day24/registers '(:w :x :y :z) "All ALU's registers")

(defun day24/tokenize (string)
  (intern (format ":%s" string)))

(defun day24/read-operand (string)
  (let ((as-token (day24/tokenize string)))
    (if (memq as-token day24/registers)
        as-token
      (string-to-number string))))

(defun day24/read-instruction (string)
  (day24/tokenize string))

(defun day24/read-opcode (line)
  (let ((strings (split-string line " " t)))
    (cons (day24/read-instruction (pop strings))
          (-map #'day24/read-operand strings))))

(defun day24/read-opcodes (lines)
  (-map #'day24/read-opcode lines))

(defun day24/create-alu ()
  (list :x 0
        :z 0
        :y 0
        :w 0))

(defun day24/operate (operation a b)
  (if (and (a )))
  (list operation a b))

(defun day24/binary-operation (alu function op1 op2)
  "Returns a new alu with the content changed"
  (let ((old-value (plist-get alu op1))
        (other-value (or (and (numberp op2) op2)
                         (plist-get alu op2))))
    (plist-put alu op1 (funcall function old-value other-value))))

(defun day24/mul--a-b (op1 op2)
  (if (numberp op1)
        (cond
         ((zerop op1) 0)
         ((= op1 1) op2)
         ((numberp op2) (* op1 op2))
         (t (list #'* op1 op2)))
    (if (numberp op2)
        (cond
         ((zerop op2) 0)
         ((= op2 1) op1)
         (t (list #'* op2 op1)))
      (list #'* op2 op1))))

(defun day24/mul (alu op1 op2)
  (day24/binary-operation alu #'day24/mul--a-b op1 op2))

;; TODO Maybe you can check for - equality?
(defun day24/add--a-b (op1 op2)
  (if (numberp op1)
        (cond
         ((zerop op1) op2)
         ((numberp op2) (+ op1 op2))
         (t (list #'+ op1 op2)))
    (if (numberp op2)
        (cond
         ((zerop op2) op1)
         ((= op2 1) op1)
         (t (list #'+ op2 op1)))
      (list #'+ op2 op1))))

(defun day24/add (alu op1 op2)
  (day24/binary-operation alu #'day24/add--a-b op1 op2))

(defun day24/mod--a-b (op1 op2)
  (when (and (numberp op1) (< op1 0))
    (error "Negative mod dividend"))
  (when (and (numberp op2) (<= op2 0))
    (error "Invalid mod divisor"))
  (if (numberp op1)
      (cond
       ((zerop op1) 0)
       ((numberp op2) (mod op1 op2))
       (t (list #'mod op1 op2)))
    (if (numberp op2)
        (cond
         ((= op2 1) 0)
;         ((equal op1 op2) 0) ;; there is a corner case where the dividend could be 0, this could result in an error! :(
         ((numberp op2) (mod op1 op2))
         (t (list #'mod op1 op2)))
      (list #'mod op1 op2))))

(defun day24/mod (alu op1 op2)
  (day24/binary-operation alu #'day24/mod--a-b op1 op2)  )

(defun day24/div--a-b (op1 op2)
  (when (and (numberp op2) (== op2 0))
    (error "Zero divisor"))
  (if (numberp op1)
      (cond
       ((zerop op1) 0)
       ((numberp op2) (truncate (/ op1 op2)))
       (t (list #'truncate (list #'/ op1 op2))))
    (if (numberp op2)
        (cond
         ((= op2 1) op1)
;         ((equal op1 op2) 1) ;; there is a corner case where the dividend could be 0, this could result in an error :(
         ((numberp op2) (truncate (/ op1 op2)))
         (t (list #'truncate (list #'/ op1 op2))))
      (list #'truncate (list #'/ op1 op2)))))

(defun day24/div (alu op1 op2)
  (day24/binary-operation alu #'day24/div--a-b op1 op2))

(defun day24/eql--a-b (op1 op2)
  (if (and (numberp op1) (numberp op2))
      (if (= op1 op2) 1 0)
    (if (equal op1 op2)
        ;; same expression. Must be equal
        1
      ;; different expression. Could be equal
      (list #'equal op1 op2))))

(defun day24/eql (alu op1 op2)
  (day24/binary-operation alu #'day24/eql--a-b op1 op2))

(defun day24/inp (alu instruction input)
  (assert input)
  (plist-put alu instruction input))

(defun day24/simplify (alu instruction inputs)
  "Returns a new cons (ALU . remaining-inputs) with an ALU with a new symbolic state"
  (if (eq (car instruction) :inp)
      ;; inp is special
      (list (day24/inp alu (elt instruction 1) (pop inputs)) inputs)    
    (list (funcall (case (car instruction)
                (:mul #'day24/mul)
                (:add #'day24/add)
                (:mod #'day24/mod)
                (:div #'day24/div)
                (:eql #'day24/eql))
              alu
              (elt instruction 1)
              (elt instruction 2))
          inputs)))


(defun day24/part-1 (lines)
  (day24/read-opcodes lines))

(defun day24/part-2 (lines)
  (error "Not yet implemented"))


(provide 'day24)
