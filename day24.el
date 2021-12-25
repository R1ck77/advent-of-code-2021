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

(defun day24/is-input? (value)
  (and (listp value)
       (stringp (car value))))

(defun day24/input--mul (input value)
  (day24/input--operation input (lambda (x) (* x value))))

(defun day24/mul--a-b (op1 op2)
  (if (numberp op1)
      (cond
       ((zerop op1) 0)
       ((= op1 1) op2)
       ((numberp op2) (* op1 op2))
       ((day24/is-input? op2) (day24/input--mul op2 op1))       
       (t (list #'* op1 op2)))
    (if (numberp op2)
        (cond
         ((zerop op2) 0)
         ((= op2 1) op1)
         ((day24/is-input? op1) (day24/input--mul op1 op2))                
         (t (list #'* op2 op1)))
      (list #'* op2 op1))))

(defun day24/mul (alu op1 op2)
  (day24/binary-operation alu #'day24/mul--a-b op1 op2))

(defun day24/merge-indices (list1 list2)
  (-uniq (append list1 list2)))

(defun day24/reduce-values (values)
  "merge the indices together"
  (let ((set (advent/table)))
    (--each (-partition 2 values)
      (let ((value (car it))
            (indices (cadr it)))
        (advent/put set value (day24/merge-indices indices (advent/get set value '())))))
    (let ((new-list))
      (maphash (lambda (value indices)
                 (setq new-list (append new-list (list value indices))))
               set)
      new-list)))

(defun day24/reduce-input (input)
  (let ((new-values (day24/reduce-values (cadr input))))
    (if (= (length new-values) 2)
        (car new-values)
      (list (car input) new-values))))

(defun day24/operate--vector (values cell-operation)
  "Returns a new vector"
  (--reduce-from (append acc
                         (let ((value (car it))
                               (indices (cadr it)))
                           (list (funcall cell-operation value)
                                 indices)))
                 ()
                 (-partition 2 values)))

(defun day24/input--operation (input f)
  (day24/reduce-input (list (car input)
                            (day24/operate--vector (cadr input) f))))

(defun day24/input--add (input value)
  (day24/input--operation input (lambda (x) (+ x value))))

;; TODO Maybe you can check for - equality?
(defun day24/add--a-b (op1 op2)
  (if (numberp op1)
        (cond
         ((zerop op1) op2)
         ((numberp op2) (+ op1 op2))
         ((day24/is-input? op2) (day24/input--add op2 op1))
         (t (list #'+ op1 op2)))
    (if (numberp op2)
        (cond
         ((zerop op2) op1)
         ((= op2 1) op1)
         ((day24/is-input? op1) (day24/input--add op1 op2))
         (t (list #'+ op2 op1)))
      (list #'+ op2 op1))))

(defun day24/add (alu op1 op2)
  (day24/binary-operation alu #'day24/add--a-b op1 op2))

(defun day24/input--mod (input value)
  (day24/input--operation input
                          (lambda (x)
                            (if (< x 0)
                                nil
                              (mod x value)))))

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
         ((day24/is-input? op1) (day24/input--mod op1 op2))
;         ((equal op1 op2) 0) ;; there is a corner case where the dividend could be 0, this could result in an error! :(
         ((numberp op1) (mod op1 op2))
         (t (list #'mod op1 op2)))
      (list #'mod op1 op2))))

(defun day24/mod (alu op1 op2)
  (day24/binary-operation alu #'day24/mod--a-b op1 op2)  )

(defun day24/div--a-b (op1 op2)
  (when (and (numberp op2) (= op2 0))
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
         ((numberp op1) (truncate (/ op1 op2)))
         (t (list #'truncate (list #'/ op1 op2))))
      (list #'truncate (list #'/ op1 op2)))))

(defun day24/div (alu op1 op2)
  (day24/binary-operation alu #'day24/div--a-b op1 op2))

(defun day24/illegal-input-value? (op)
  "True if op is a number but it's outside [1,9]"
  (and (numberp op)
       (not (and (> op 0)
                 (< op 10)))))

;; TODO/FIXME wrong
(defun day24/input--eql-inputs (op1 op2)
  (let ((v1 (cadr op1))
        (v2 (cadr op2)))
    (let ((set (advent/table)))
      (--each (advent/v->l v1)
        (advent/put set it t))
      (if (not (--filter (advent/get set it) (advent/v->l v2 )))
          0
        (list #'equal op1 op2)))))

(defun day24/eql--a-b (op1 op2)
  (if (and (numberp op1) (numberp op2))
      ;; both are number
      (if (= op1 op2) 1 0)
    (if (and (day24/is-input? op1) (day24/is-input? op2))
        (day24/input--eql-inputs op1 op2)
     (if (or (and (day24/illegal-input-value? op1) (day24/is-input? op2))
             (and (day24/illegal-input-value? op2) (day24/is-input? op1)))
         ;; an input value can't be like that!
         0
       (if (equal op1 op2)
           ;; same expression. Must be equal
           1
         ;; different expression. Could be equal
         (list #'equal op1 op2))))))

(defun day24/eql (alu op1 op2)
  (day24/binary-operation alu #'day24/eql--a-b op1 op2))

(defun day24/inp (alu instruction input)
  (assert input)
  (plist-put alu instruction input))

(defun day24/simplify (alu-inputs instruction)
  "Returns a new cons (ALU . remaining-inputs) with an ALU with a new symbolic state"
  (let ((alu (car alu-inputs))
        (inputs (cadr alu-inputs)))
    (print instruction)
    (print inputs)
    (redisplay)
   (if (eq (car instruction) :inp)
       ;; inp is special
       (let ((next-input (pop inputs)))
        (list (day24/inp alu (elt instruction 1) next-input) inputs))    
     (list (funcall (case (car instruction)
                      (:mul #'day24/mul)
                      (:add #'day24/add)
                      (:mod #'day24/mod)
                      (:div #'day24/div)
                      (:eql #'day24/eql))
                    alu
                    (elt instruction 1)
                    (elt instruction 2))
           inputs))))

(defun day24/starting-input-data ()
  (--reduce-from (append acc (list it (list it))) '() (number-sequence 1 9)))

(defun day24/create-inputs (count)
  (nreverse
   (--map (list (format ":i%d" it)
                (day24/starting-input-data))
          (number-sequence 0 (1- count)))))

(defun day24/simplify-all (input-count instructions)
  "Returns the z value of the processor"
  (let ((alu-inputs (--reduce-from (day24/simplify acc it)
                        (list (day24/create-alu)
                              (day24/create-inputs input-count))
                        instructions)))
    ;; I expect all inputs to be consumed
    (plist-get (car alu-inputs) :z)))

(defun day24/part-1 (lines)
  (day24/simplify-all 14 (day24/read-opcodes lines)))

(defun day24/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day24)

(setq example (day24/read-opcodes (advent/read-problem-lines 24 :problem)))

