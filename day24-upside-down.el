;; -*- lexical-binding: t -*-
(require 'dash)
(require 's)
(require 'advent-utils)

(setq lexical-binding t)

(defconst day24-upside-down/registers '(:w :x :y :z) "All ALU's registers")


(defun day24-upside-down/create-alu ()
  (list :x 0
        :z 0
        :y 0
        :w 0))

(defun day24-upside-down/operate (operation a b)
  (if (and (a )))
  (list operation a b))

(defun day24-upside-down/binary-operation (alu function op1 op2)
  "Returns a new alu with the content changed"
  (let ((old-value (plist-get alu op1))
        (other-value (or (and (numberp op2) op2)
                         (plist-get alu op2))))
    (plist-put alu op1 (funcall function old-value other-value))))

(defun day24-upside-down/is-input? (value)
  (and (listp value)
       (plist-get value :name)))

(defun day24-upside-down/input--mul (input value)
  (lexical-let ((value value))
    (day24-upside-down/input--operation input (lambda (x) (* x value)))))

(defun day24-upside-down/input--mul-inputs (op1 op2)
  (day24-upside-down/input--binary-operation op1 op2 #'*))

(defun day24-upside-down/mul--a-b (op1 op2)
  (cond
   ((and (day24-upside-down/is-input? op1) (day24-upside-down/is-input? op2))
    (day24-upside-down/input--mul-inputs op1 op2))
   ((and (numberp op1) (numberp op2)) (* op1 op2))
   ((and (day24-upside-down/is-input? op1) (numberp op2))
    (cond
     ((zerop op2) 0) ; null element
     ((= 1 op2) op1) ;identity element
     (t (day24-upside-down/input--mul op1 op2))))
   ((and (day24-upside-down/is-input? op2) (numberp op1))
    (day24-upside-down/mul--a-b op2 op1))
   (t (error "Unresolved operation: * %s %s" op1 op2))))

(defun day24-upside-down/mul (alu op1 op2)
  (day24-upside-down/binary-operation alu #'day24-upside-down/mul--a-b op1 op2))

(defun day24-upside-down/index-to-magnitude (index)
  (car
   (--reduce-from (cons (+ (car acc) (if it (* it (cdr acc)) 0))                  
                        (* (cdr acc) 8))
                  '(0 . 1)
                  (advent/v->l index))))

(defun day24-upside-down/merge-indices (list1 list2)
  (-uniq (append list1 list2))
   )

(defun day24-upside-down/reduce-values (values)
  "merge the indices together"
  (let ((set (advent/table)))
    (--each (-partition 2 values)
      (let ((value (car it))
            (indices (cadr it)))
        (when value
         (advent/put set value (day24-upside-down/merge-indices indices (advent/get set value '()))))))
    (let ((new-list))
      (maphash (lambda (value indices)
                 (setq new-list (append new-list (list value indices))))
               set)
      new-list)))

(defun day24-upside-down/reduce-input (input)
  (let ((new-values (day24-upside-down/reduce-values (plist-get input :value))))
    (if (= (length new-values) 2)
        (car new-values)
      (list :name (plist-get input :name)
            :value new-values))))

;;; TODO/FIXME remove nil?
(defun day24-upside-down/operate--vector (values cell-operation)
  "Returns a new vector"
  (--reduce-from (append acc
                         (let ((value (car it))
                               (indices (cadr it)))
                           (if-let (new-value (funcall cell-operation value))
                               (list new-value
                                     indices))))
                 ()
                 (-partition 2 values)))

(defun day24-upside-down/input--operation (input f)
  (print (format "1-op %d" (length (plist-get input :value))))
  (day24-upside-down/debug-output-size
   (lexical-let ((f f))
     (day24-upside-down/reduce-input (list :name (plist-get input :name)
                               :value (day24-upside-down/operate--vector (plist-get input :value) f))))))

(defun day24-upside-down/input--add (input value)
  (lexical-let ((value value))
    (day24-upside-down/input--operation input (lambda (x) (+ x value)))))

(defun day24-upside-down/input--add-inputs (op1 op2)
  (day24-upside-down/input--binary-operation op1 op2 (lambda (a b) (+ a b))))


(defun day24-upside-down/add--a-b (op1 op2)
  (cond
   ((and (numberp op1) (numberp op2))
    (+ op1 op2))
   ((and (day24-upside-down/is-input? op1) (day24-upside-down/is-input? op2))
    (day24-upside-down/input--add-inputs op1 op2))
   ((and (day24-upside-down/is-input? op1) (numberp op2))
    (if (zerop op2)
        op1
      (day24-upside-down/input--add op1 op2)))
   ((and (day24-upside-down/is-input? op2) (numberp op1))
    (day24-upside-down/add--a-b op2 op1))
   (t (error (format "Unhandled: eql %s %s" op1 op2)))))

(defun day24-upside-down/add (alu op1 op2)
  (day24-upside-down/binary-operation alu #'day24-upside-down/add--a-b op1 op2))

(defun day24-upside-down/input--mod (input value)
  (lexical-let ((value value))
    (day24-upside-down/input--operation input
                            (lambda (x)
                              (mod x value)))))

(defun day24-upside-down/mod--a-b (op1 op2)    
  (assert (numberp op2))
  (cond
   ((numberp op1) (mod op1 op2))
   ((day24-upside-down/is-input? op1) (day24-upside-down/input--mod op1 op2))
   (t (error (format "Unhandled: mod %s %s" op1 op2)))))

(defun day24-upside-down/mod (alu op1 op2)
  (day24-upside-down/binary-operation alu #'day24-upside-down/mod--a-b op1 op2)  )

(defun day24-upside-down/input--div (input value)
  (lexical-let ((value value))
    (day24-upside-down/input--operation input (lambda (x) (truncate (/ x value))))))

(defun day24-upside-down/safe-division-2 (x y)
  (truncate (/ x y)))

(defun day24-upside-down/input--div-inputs (op1 op2)
  (day24-upside-down/input--binary-operation op1 op2 #'day24-upside-down/safe-division-2))

(defun day24-upside-down/div--a-b (op1 op2)
  (assert (numberp op2))
  (assert (not (zerop op2)))
  (if (and (day24-upside-down/is-input? op1) (day24-upside-down/is-input? op2))
      (progn
        (error "This shouldn't happen!")
        (day24-upside-down/input--div-inputs op1 op2))
    (if (numberp op1)
        (cond
         ((zerop op1) 0)
         ((numberp op2) (truncate (/ op1 op2)))
         (t (list #'truncate (list #'/ op1 op2))))
      (cond
       ((= op2 1) op1)
       ((numberp op1) (truncate (/ op1 op2)))
       ((day24-upside-down/is-input? op1) (day24-upside-down/input--div op1 op2))
       (t (list #'truncate (list #'/ op1 op2)))))))

(defun day24-upside-down/div (alu op1 op2)
  (day24-upside-down/binary-operation alu #'day24-upside-down/div--a-b op1 op2))

(defun day24-upside-down/illegal-input-value? (op)
  "True if op is a number but it's outside [1,9]"
  (and (numberp op)
       (not (and (> op 0)
                 (< op 10)))))

(defun day24-upside-down/unroll--value (value)
  "Split a value in a repeated list of (value [index])"
  (--reduce-from (let ((value (car it))
                       (indices (cadr it)))
                   (append acc (-reduce-from (lambda (pairs pair)
                                               (cons pair pairs))                                 
                                             ()
                                             (-map (lambda (index)
                                                     (list value index))
                                                   indices)))
                   )
                 '()
                 (-partition 2 value)))

(defun day24-upside-down/roll--value (pairs)
  (let ((merged (advent/table)))
    (--each pairs
      (let ((value (car it))
            (index (cadr it)))
        (advent/put merged value (cons index (advent/get merged value)))))
    (let ((result))
      (maphash (lambda (value indices)
                 (setq result (append result (list value indices))))
               merged)
      result)))

(defun day24-upside-down/any--conflicting-indices? (idx1 idx2)
  (--any (and
          ;; both indices for the variable are present
          (car it) (cdr it)
          ;;; but they are not the same
          (/= (car it) (cdr it)))
         (-zip (advent/v->l idx1) (advent/v->l idx2))))

(defun day24-upside-down/merge--indices (idx1 idx2)
  (apply #'vector
         (--map (or (car it) (cdr it))
                (-zip (advent/v->l idx1) (advent/v->l idx2)))))

(defun day24-upside-down/debug-output-size (op)
  (if (numberp op)
      (print (format "Got a number (%d)" op))
    (print (format "Got %d distinct values" (length (plist-get op :value)))))
  (redisplay)
  op)

(defun day24-upside-down/input--binary-operation (op1 op2 f)
  (print (format "2-op %d vs %d "
                 (length (plist-get op1 :value))
                 (length (plist-get op2 :value))))
  (redisplay)
  (day24-upside-down/debug-output-size
   (let ((values1 (day24-upside-down/unroll--value (plist-get op1 :value)))
         (values2 (day24-upside-down/unroll--value (plist-get op2 :value)))
         (results))
     (loop for i in values1 do
           (let ((v1 (car i))
                 (i1 (cadr i)))
             (loop for j in values2 do
                   (let ((v2 (car j))
                         (i2 (cadr j)))
                     (unless (day24-upside-down/any--conflicting-indices? i1 i2)
                       (let ((new-index (day24-upside-down/merge--indices i1 i2))
                             (new-value (funcall f v1 v2)))
                         (if new-value  ; discard nil values!
                             (push (list new-value new-index) results))))))))
     (day24-upside-down/reduce-input
      (list :name (s-truncate 10 (concat (plist-get op1 :name)
                                         ":"
                                         (plist-get op2 :name)) "???")
            :value (day24-upside-down/roll--value results))))))

(defun day24-upside-down/input--eql (input value)
  (lexical-let ((value value))
    (day24-upside-down/input--operation input (lambda (x) (if (= x value) 1 0)))))

(defun day24-upside-down/input--eql-inputs (op1 op2)
  (day24-upside-down/input--binary-operation op1 op2 (lambda (a b) (if (= a b) 1 0))))

(defun day24-upside-down/eql--a-b (op1 op2)
  (cond
   ((and (numberp op1) (numberp op2)) (if (= op1 op2) 1 0))
   ((and (day24-upside-down/is-input? op1) (day24-upside-down/is-input? op2))
    (day24-upside-down/input--eql-inputs op1 op2))
   ((and (day24-upside-down/is-input? op1) (numberp op2)) (day24-upside-down/input--eql op1 op2))
   ((and (day24-upside-down/is-input? op2) (numberp op1)) (day24-upside-down/input--eql op2 op1))
   (t (error (format "Unhandled: eql %s %s" op1 op2)))))

(defun day24-upside-down/eql (alu op1 op2)
  (day24-upside-down/binary-operation alu #'day24-upside-down/eql--a-b op1 op2))

(defun day24-upside-down/inp (alu instruction input)
  (assert input)
  (plist-put alu instruction input))

(defun day24-upside-down/simplify (alu-inputs instruction)
  "Returns a new cons (ALU . remaining-inputs) with an ALU with a new symbolic state"
  (let ((alu (car alu-inputs))
        (inputs (cadr alu-inputs)))
    (print instruction)
    (print inputs)
    (redisplay)
   (if (eq (car instruction) :inp)
       ;; inp is special
       (let ((next-input (pop inputs)))
        (list (day24-upside-down/inp alu (elt instruction 1) next-input) inputs))    
     (list (funcall (case (car instruction)
                      (:mul #'day24-upside-down/mul)
                      (:add #'day24-upside-down/add)
                      (:mod #'day24-upside-down/mod)
                      (:div #'day24-upside-down/div)
                      (:eql #'day24-upside-down/eql))
                    alu
                    (elt instruction 1)
                    (elt instruction 2))
           inputs))))

(defun day24-upside-down/create-i2v (count var-index start-value)
  (let ((indices (make-vector count nil)))
    (aset indices var-index (list start-value))
    indices))

(defun day24-upside-down/starting-input-data (count var-index)
  (--reduce-from (append acc (list it (list (day24-upside-down/create-i2v count var-index it))))
                 '()
                 (number-sequence 1 9)))

(defun day24-upside-down/create-inputs (count)
  (nreverse
   (--map (list :name (format ":i%d" it)
                :value (day24-upside-down/starting-input-data count it))
          (number-sequence 0 (1- count)))))

(defun day24-upside-down/simplify-all (input-count instructions)
  "Returns the z value of the processor"
  (let ((alu-inputs (--reduce-from (day24-upside-down/simplify acc it)
                        (list (day24-upside-down/create-alu)
                              (day24-upside-down/create-inputs input-count))
                        instructions)))
    ;; I expect all inputs to be consumed
    (print "RESULT ready!")
    (redisplay)
    (car alu-inputs)))

(defun day24-upside-down/simplify-all-z (input-count instructions)
  (plist-get (day24-upside-down/simplify-all input-count instructions) :z))

(defun day24-upside-down/part-1 (lines)
  (day24-upside-down/simplify-all-z 14 (day24-upside-down/read-opcodes lines)))

(defun day24-upside-down/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day24-upside-down)

(defun read-programX (text)
  (day24-upside-down/read-opcodes (split-string text "\n" t)))

(defun simplify-program (inputs p)
  (day24-upside-down/simplify-all inputs (read-programX p)))

(setq example (day24-upside-down/read-opcodes (advent/read-problem-lines 24 :problem)))
(setq negate (read-programX "inp x
mul x -1"))
(setq triple (read-programX "inp z
inp x
mul z 3
eql z x"))
(setq binary (read-programX "inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2"))
