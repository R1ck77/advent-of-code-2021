;; -*- lexical-binding: t -*-
(require 'dash)
(require 's)
(require 'advent-utils)

(defconst day24/registers '(:w :x :y :z) "All ALU's registers")
(defconst day24/registers-base (list :w "w"
                                     :x "x"
                                     :y "y"
                                     :z "z"))

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

(defun day24/build-symbol (register index)
  (intern (concat ":" (plist-get day24/registers-base register)
                  (number-to-string index))))

(defun day24/current-input (state)
  (intern (format ":i%d" (plist-get state :input))))

(defun day24/current-symbol (state register)
  (day24/build-symbol register
                      (plist-get (plist-get state :indices) register)))

;; TODO/FIXME duplicated code
(defun day24/next-symbol (state register)
  (day24/build-symbol register
                       (1+ (plist-get (plist-get state :indices) register))))

(defun day24/increase-index (state register)
  (let* ((indices (plist-get state :indices))
         (old-index (plist-get indices register)))
    (plist-put (copy-sequence state)
               :indices (plist-put (copy-sequence indices)
                                   register (1+ old-index)))))

(defun day24/increase-input (state)
  (let ((new-state (day24/copy-state state)))
    (plist-put new-state :input (1+ (plist-get state :input)))))

(defun day24/create-state ()
  (list :indices (list :w 0 :x 0 :y 0 :z 0)
        :expressions (advent/table)
        :input 0)) ;; you start delivering the least significant digit

(defun day24/copy-state (state)
  (list :indices (copy-sequence (plist-get state :indices))
        :expressions (copy-hash-table (plist-get state :expressions))
        :input (plist-get state :input)))

(defun day24/to-string (state)
  (let ((result ""))
    (setq result (concat result (format "* INDICES: %s\n" (plist-get state :indices))))
    (setq result (concat result (format "* NEXT SERVED: %d\n" (plist-get state :input))))
    (setq result (concat result "* DICTIONARY:\n"))
    (maphash (lambda (symbol value)
               (setq result (concat result (format "  %s := %s\n" symbol value))))
             (plist-get state :expressions))
    result))

(defun day24/add-symbol (state symbol expression)
  (let ((new-state (day24/copy-state state)))
    (advent/put (plist-get new-state :expressions) symbol expression)
    new-state))

(defun day24/add-binary (state op dst src)
  "Returns a new state with the information added"
  (assert (not (numberp dst)))
  (let ((current-dst (day24/current-symbol state dst))
        (next-dst (day24/next-symbol state dst))
        (next-state (day24/increase-index state dst)))
    (cond
     ;; op
     ((numberp src)
      (day24/add-symbol next-state current-dst (list op next-dst src)))
     ;; symbol
     ((plist-get day24/registers-base src)
      (day24/add-symbol next-state current-dst (list op next-dst (day24/current-symbol next-state src))))
     ;; tertio non datur
     (t (error (format "Unexpected src: %s" src))))))

(defun day24/add-inp (state dst)
  "Returns a new state with the information set"
  (assert (not (numberp dst)))
  (let ((current (day24/current-symbol state dst))
        (current-input (day24/current-input state)))
    (day24/add-symbol (day24/increase-index (day24/increase-input state)
                                            dst)
                      current
                      current-input)))

(defun day24/add-instruction (state instruction)
  (let ((op (elt instruction 0)))
    (if (eq op :inp)
        (day24/add-inp state
                       (elt instruction 1))
      (day24/add-binary state
                        op
                        (elt instruction 1)
                        (elt instruction 2)))))

(defun day24/record-program (instructions)
  (--reduce-from (day24/add-instruction acc it)
                 (day24/create-state)
                 (reverse instructions)))

(defun day24/update-instructions (state f)
  (let ((new-state (day24/copy-state state))
        (new-expressions))
    (maphash (lambda (symbol expression))
             (plist-get state :expressions))
    (plist-put new-state :expressions )))

(defun day24/update-selection (f state-sel)
  (let ((state (car state-sel))
        (selection (cadr state-sel)))
    (let* ((new-state (day24/copy-state state))
           (new-expressions (plist-get new-state :expressions)))
      (--each selection
        (let ((result (apply f selection)))
          (if result
              (advent/put new-expressions (car result) (cadr result)))))
      new-state)))

(defun day24/select-expr (pred state)
  (list 
   state
   (let ((selected))
    (maphash (lambda (symbol value)
               (if (funcall pred symbol value)
                   (push (list symbol value) selected)))
             (plist-get state :expressions))
    selected)))

(defun day24/resolve-zeroes (state)
  "Returns a new state where all operations with a 0 src are resolved"
  (day24/update-selection (lambda (symbol value)
                            (case (car value)
                              (:mul (list symbol 0))
                              (:add (list symbol (cadr-value)))
                              (t (error (format "Unexpected zero operation for %s" value)))))
                          (day24/select-expr (lambda (symbol value)
                                               (and (numberp (elt value 2)) (zerop (elt value 2))))
                                             state)))

(defun day24/part-1 (lines)
  (error "Not yet implemented"))

(defun day24/part-2 (lines)
  (error "Not yet implemented"))


(provide 'day24)


(defun read-programX (text)
  (day24/read-opcodes (split-string text "\n" t)))
(setq example (day24/read-opcodes (advent/read-problem-lines 24 :problem)))

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
