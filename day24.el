(require 'dash)
(require 's)
(require 'advent-utils)

(defconst day24/index--range (number-sequence 0 8))
(defconst day24/registers '(:w :x :y :z))

(defun day24/debug--assert-alu-ok (alu)
  (assert (and
           (numberp (plist-get alu :x))
           (numberp (plist-get alu :y))
           (numberp (plist-get alu :z))
           (numberp (plist-get alu :w))))
  alu)

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

(defun day24/binary--update! (alu f a b)
  (let ((dst (plist-get alu a))
        (src (if (numberp b) b (plist-get alu b))))
    (plist-put alu
     a
     (when (and dst src)
       (funcall f dst src)))))

(defun day24/add! (alu a b)
  (day24/binary--update! alu #'+ a b))

(defun day24/mul! (alu a b)
  (day24/binary--update! alu #'* a b))

(defun day24/div! (alu a b)
  (day24/binary--update! alu
                        (lambda (x y)
                          (unless (zerop y)
                            (truncate (/ x y))))
                        a b))

(defun day24/mod! (alu a b)
  (day24/binary--update! alu
                        (lambda (x y)
                          (unless (or (< x 0)
                                      (<= y 0))
                            (mod x y)))
                        a b))

(defun day24/eql! (alu a b)
    (day24/binary--update! alu
                        (lambda (x y)
                          (if (= x y) 1 0))
                        a b))

(defvar day24/debug--verbose t)

(defun day24/debug--print (value)
  (when day24/debug--verbose
    (print value)
    (redisplay)))

(defun day24/evolve-alu! (alu instruction)
  "Accepts an alu and an instruction and returns a new ALU"
;  (day24/debug--print (format "Executing   %s" instruction))
  ;; Operations that doesn't split the ALU, the variables list are the same
  (let ((op (car instruction))
        (op1 (elt instruction 1))
        (op2 (elt instruction 2)))
    (case op
      (:add (day24/add! alu op1 op2))
      (:mul (day24/mul! alu op1 op2))
      (:div (day24/div! alu op1 op2))
      (:mod (day24/mod! alu op1 op2))
      (:eql (day24/eql! alu op1 op2))        
      (t (error (format "Unsupported opcode '%s'" car))))))

(defun day24/validate-alu (alu)
  (and
   (numberp (plist-get alu :x))
   (numberp (plist-get alu :y))
   (numberp (plist-get alu :z))
   (numberp (plist-get alu :w))
   alu))

(defun day24/number-to-input (number &optional size)
  (let ((raw-input (-map #'string-to-number (split-string (number-to-string number) "" t))))
    (append (-repeat (- (or size 14) (length raw-input)) 1) raw-input)))

(defun day24/validate-input (instructions lnumber)
  (let ((evolved (--reduce-from (let ((next-instruction it)
                              (alu (car acc))
                              (inputs (cdr acc)))
                          (if alu
                              (if (eq (car next-instruction) :inp)                       
                                  (cons (plist-put alu (cadr next-instruction) (pop inputs))
                                        inputs)
                                (cons (day24/validate-alu
                                       (day24/evolve-alu! alu next-instruction))
                                      inputs))
                            (cons nil nil)))
                        (cons (day24/create-alu) lnumber)
                        instructions)))
    (and (car evolved) (= (plist-get (car evolved) :z) 0))))

(defun day24/part-1 (lines)
  (day24/evolve-all nil
                    (list nil (day24/create-alu))
                    (day24/read-opcodes lines)))

(defun day24/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day24)

(setq example (day24/read-opcodes (advent/read-problem-lines 24 :problem)))
(setq negate (day24/read-opcodes (list "inp x" "mul x -1")))
(setq is-trice? (day24/read-opcodes (list "inp z" "inp x" "mul z 3" "eql z x")))
(setq binary-conversion (day24/read-opcodes (list "inp w" "add z w" "mod z 2" "div w 2" "add y w" "mod y 2" "div w 2" "add x w" "mod x 2" "div w 2" "mod w 2")))

(setq first-batch (day24/read-opcodes (list
"inp w"
"mul x 0"
"add x z"
"mod x 26"
"div z 1"
"add x 14"
"eql x w"
"eql x 0"
"mul y 0"
"add y 25"
"mul y x"
"add y 1"
"mul z y"
"mul y 0"
"add y w"
"add y 12"
"mul y x"
"add z y")))
(setq second-batch (day24/read-opcodes (list
"inp w"
"mul x 0"
"add x z"
"mod x 26"
"div z 1"
"add x 14"
"eql x w"
"eql x 0"
"mul y 0"
"add y 25"
"mul y x"
"add y 1"
"mul z y"
"mul y 0"
"add y w"
"add y 12"
"mul y x"
"add z y"
"inp w"
"mul x 0"
"add x z"
"mod x 26"
"div z 1"
"add x 11"
"eql x w"
"eql x 0"
"mul y 0"
"add y 25"
"mul y x"
"add y 1"
"mul z y"
"mul y 0"
"add y w"
"add y 8"
"mul y x"
"add z y")))

(setq max-specpdl-size 10000)
(setq max-lisp-eval-depth 10000)

;; TODO/FIXME debug stuff
(defun mr () (apply #'vector (number-sequence 1 9)))
(defun mv (v) (make-vector 9 v))
