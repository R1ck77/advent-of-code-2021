(require 'dash)
(require 's)
(require 'advent-utils)

(defconst day24/index--range (number-sequence 0 8))
(defconst day24/registers '(:w :x :y :z))

(defun day24/debug--assert-alu-ok (alu)
  (assert (and
           (vectorp (plist-get alu :x))
           (vectorp (plist-get alu :y))
           (vectorp (plist-get alu :z))
           (vectorp (plist-get alu :w))))
  alu)

(defun day24/tokenize (string)
  (intern (format ":%s" string)))

(defun day24/read-operand (string)
  (let ((as-token (day24/tokenize string)))
    (if (memq as-token day24/registers)
        as-token
      (make-vector 9 (string-to-number string)))))

(defun day24/read-instruction (string)
  (day24/tokenize string))

(defun day24/read-opcode (line)
  (let ((strings (split-string line " " t)))
    (cons (day24/read-instruction (pop strings))
          (-map #'day24/read-operand strings))))

(defun day24/read-opcodes (lines)
  (-map #'day24/read-opcode lines))

(defun day24/create-alu ()
  (list :x (make-vector 9 0)
        :z (make-vector 9 0)
        :y (make-vector 9 0)
        :w (make-vector 9 0)))

(defun day24/pair--operation (f dst src)
  (let ((result (make-vector 9 0)))
   (--each day24/index--range
     (let ((vsrc (aref src it))
           (vdst (aref dst it)))
       (aset result it (funcall f vdst vsrc))))
   result))

;;; TODO/FIXME unused
(defun day24/copy-alu (alu)
  (let ((new-alu))
    (--each day24/registers
      (setq new-alu (plist-put new-alu it (copy-sequence (plist-get alu it)))))
    new-alu))

(defun day24/binary--update (alu f a b)
  (let ((new-alu (copy-sequence alu))
        (dst (plist-get alu a))
        (src (if (vectorp b) b (plist-get alu b))))
    (plist-put new-alu 
     a
     (when (and dst src)
       (day24/pair--operation f dst src)))))

(defun day24/add (alu a b)
  (day24/binary--update alu #'+ a b))

(defun day24/mul (alu a b)
  (day24/binary--update alu #'* a b))

(defun day24/div (alu a b)
  (day24/binary--update alu
                        (lambda (x y)
                          (unless (zerop y)
                            (truncate (/ x y))))
                        a b))

(defun day24/mod (alu a b)
  (day24/binary--update alu
                        (lambda (x y)
                          (unless (or (< x 0)
                                      (<= y 0))
                            (mod x y)))
                        a b))

(defun day24/eql (alu a b)
    (day24/binary--update alu
                        (lambda (x y)
                          (if (= x y) 1 0))
                        a b))

(defun day24/get--raw-indexed-columns (alu)
  "Return the transposed values of the alu in the form ((w values) (x values) ... (z values))

Columns containing nil's are set to nil"
  (let ((results))
    (loop for i from 0 upto 8 do
          (let ((column))
            (loop for var in day24/registers do
                  (push (aref (plist-get alu var) i) column))
            (push (when (= (length (-non-nil column)) 4)
                    (nreverse column))
                  results)))
    (nreverse results)))

;;; TODO/FIXME THIS DOESN'T WORK???
;;;
(comment
 (advent/-map-hash set
   (progn 
     (push (list it-value it-key) result))))

(defun day24/sort--alu-state (alu)
  "Return a list of lists, each one in the form:

((input values) (w x y z values))

e.g.:

((1 3 4) (0 1 3 9))
((2 5))

SOME INDICES MAY BE MISSING (if they are nil)"
  (let ((transposed (day24/get--raw-indexed-columns alu))
        (set (advent/table)))
    (--each-indexed transposed
      (when it ; unless the value is invalid
        (advent/put set it (cons (1+ it-index) (advent/get set it '())))))
    (let ((result)) 
      (maphash (lambda (key value) 
                 (push (list value key) result))
               set)
      result)))

(defun day24/make-alu (seed inp-var)
  "Create a new alu from the specified seed and new set variable"
  (let ((alu))
    (loop for i from 0 below 4 do
          ;; TODO/FIXME use a vector for the registers for faster access?
          (let ((var (elt day24/registers i)))
            (if (eq var inp-var)
                ;; Set the variable with numbers from 1 to 9
                (setq alu (plist-put alu var (apply #'vector (number-sequence 1 9))))
              (setq alu (plist-put alu var (make-vector 9 (elt seed i)))))))
    alu))

(defun day24/make-alu-from-pair (indices-state inp-var)
  (list (nreverse (car indices-state))
        (day24/debug--assert-alu-ok
         (day24/make-alu (cadr indices-state) inp-var))))

;; TODO/FIXME you need to merge the identical values!!!!
(defun day24/reset (alu inp-var)
  "Return a list of alus in the form:

((1 3 4) (alu1 alu2 alu3)
 (7 9) (alu4 alu5))"
  (let (results)
    (let ((sorted-states (day24/sort--alu-state alu)))
      (--map (day24/make-alu-from-pair it inp-var) sorted-states))))

(defun day24/evolve-alu (alu instruction)
  "Accepts an alu and an instruction and returns a list of evolved ALUs with the relevant indices"
  (day24/debug--assert-alu-ok alu)
  (print (format "Executing   %s" instruction))
  (let ((op (car instruction))
        (op1 (elt instruction 1)))
    (if (eq op :inp)
        ;; Split the ALU
        (progn
          (day24/reset alu op1))
      ;; Operations that doesn't split the ALU, the variables are the same ('nil')
      (list (list nil
                  (day24/debug--assert-alu-ok
                   (let ((op2 (elt instruction 2)))
                     (case op
                       (:add (day24/add alu op1 op2))
                       (:mul (day24/mul alu op1 op2))
                       (:div (day24/div alu op1 op2))
                       (:mod (day24/mod alu op1 op2))
                       (:eql (day24/eql alu op1 op2))        
                       (t (error (format "Unsupported opcode '%s'" car)))))))))))

(defun day24/evolve-all (variables-alu instructions)
  (let ((tree)
        (variables (car variables-alu))
        (alu (day24/debug--assert-alu-ok (cadr variables-alu))))
    (if instructions
      (let ((new-variables-alus (day24/evolve-alu alu (pop instructions))))
        (if (and (= (length new-variables-alus) 1) (not (caar new-variables-alus)))
            ;; no split, just evolve this one with the remaining instructions
            (let ((single-evolution (car new-variables-alus)))
              (day24/evolve-all (list variables (cadr single-evolution)) instructions))
          ;; split: evolve each one with his own index
          (list variables (--map (day24/evolve-all it instructions) new-variables-alus))))
      ;; TODO: return an index in the correct form
      (format "LAST ALU: %s" alu))))

(defun day24/part-1 (lines)
  (error "Not yet implemented"))

(defun day24/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day24)

(setq example (day24/read-opcodes (advent/read-problem-lines 24 :problem)))

(setq max-specpdl-size 10000)
(setq max-lisp-eval-depth 10000)

;; TODO/FIXME debug stuff
(defun mr () (apply #'vector (number-sequence 1 9)))
(defun mv (v) (make-vector 9 v))
