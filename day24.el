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

(defun day24/make-input (index)
  (intern (format ":i%d" index)))

(defun day24/current-input (state)
  (day24/make-input (plist-get state :input)))

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
  (let ((result "")
        (ranges (day24/compute-ranges state)))
    (setq result (concat result (format "* INDICES: %s\n" (plist-get state :indices))))
    (setq result (concat result (format "* NEXT SERVED: %d\n" (plist-get state :input))))
    (setq result (concat result "* DICTIONARY:\n"))
    (maphash (lambda (symbol value)
               (setq result (concat result
                                    (if-let ((range (advent/get ranges symbol)))
                                        (format "%-40s %s\n" (format "  %s := %s" symbol value) range)
                                      (format "  %s := %s\n" symbol value)))))
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

(defun day24/add--terminators (state)
  "Add a 0 for each variable at the start of the code"
  (--reduce-from (let ((new-zero-variable (day24/current-symbol acc it)))
                   (day24/add-symbol acc new-zero-variable 0))
                 state
                 day24/registers))

(defun day24/record-program (instructions)
  (day24/add--terminators
   (--reduce-from (day24/add-instruction acc it)
                  (day24/create-state)
                  (reverse instructions))))

(defun day24/update-selection (f state-sel)
  (let ((state (car state-sel))
        (selection (cadr state-sel)))
    (let* ((new-state (day24/copy-state state))
           (new-expressions (plist-get new-state :expressions)))
      (--each selection
        (let ((result (apply f it)))
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

(defun day24/is-input? (value)
  (and (symbolp value)
       (s-starts-with? ":i" (symbol-name value))))

(defun day24/resolve-zeroes (state)
  "Returns a new state where all obvious operations with a 0 are resolved"
  (let ((right-zero-resolved (day24/update-selection (lambda (symbol value)
                                                       (case (car value)
                                                         (:mul (list symbol 0))
                                                         (:add (list symbol (elt value 1)))
                                                         (:eql (if (day24/is-input? (elt value 1))
                                                                   0
                                                                 (list symbol value)))
                                                         (t (error (format "Unexpected zero operation for %s" value)))))
                                                     (day24/select-expr (lambda (symbol value)
                                                                          (and (listp value)
                                                                               (numberp (elt value 2))
                                                                               (zerop (elt value 2))))
                                                                        state))))
    (day24/update-selection (lambda (symbol value)
                              (case (car value)
                                (:mul (list symbol 0))
                                (:add (list symbol (elt  value 2)))
                                (:div (list symbol 0))
                                (:mod (list symbol 0))
                                (:eql (if (day24/is-input? (elt value 2))
                                          0
                                        (list symbol value)))
                                (t (error (format "Unexpected zero operation for %s" value)))))
                            (day24/select-expr (lambda (symbol value)
                                                 (and (listp value)
                                                      (numberp (elt value 1))
                                                      (zerop (elt value 1))))
                                               right-zero-resolved))))

(defun day24/resolve-ones (state)
  "Returns a new state where all obvious operations with a 1 are resolved"
  (let ((right-one-resolved (day24/update-selection (lambda (symbol value)
                                                       (case (car value)
                                                         (:mul (list symbol (elt value 1)))
                                                         (:mod (error "A (mod x 1) is unexpected…"))
                                                         (:div (list symbol (elt value 1)))
                                                         (:add (list symbol value))
                                                         (:eql (list symbol value))
                                                         (t (error (format "Unexpected one operation for %s" value)))))
                                                     (day24/select-expr (lambda (symbol value)
                                                                          (and (listp value)
                                                                               (numberp (elt value 2))
                                                                               (= (elt value 2) 1)))
                                                                        state))))
    (day24/update-selection (lambda (symbol value)
                              (case (car value)
                                (:mul (list symbol (elt value 2)))
                                (:add (list symbol value))
                                (:mod (list symbol 1))
                                (:div (list symbol value)) ; this one will be automatically resolved
                                (:eql (list symbol value))
                                (t (error (format "Unexpected one operation for %s" value)))))
                            (day24/select-expr (lambda (symbol value)
                                                 (and (listp value)
                                                      (numberp (elt value 1))
                                                      (= (elt value 1) 1)))
                                               right-one-resolved))))

(defun day24/resolve-obvious-input-opertions (state)
  "Returns a new state where all obvious eql with inputs are resolved"
  (let ((right-input-resolved (day24/update-selection (lambda (symbol value)
                                                        (case (car value)
                                                          ;; Boring, non resolvable stuff
                                                          ((:mul :add) (list symbol value))
                                                          ;; Equals are nore interesting
                                                          (:eql (let ((other-value (elt value 1)))
                                                                  (if (and (numberp other-value)
                                                                           (or (<= other-value 0)
                                                                               (>= other-value 10)))
                                                                      (list symbol 0)
                                                                    (list symbol value))))                                                          
                                                          (:mod (error ("Unexpected 'mod x input' operation")))
                                                          (:div (error ("Unexpected 'div x input' operation")))
                                                          (t (error (format "Unexpected input operation for %s" value)))))
                                                     (day24/select-expr (lambda (symbol value)
                                                                          (and (listp value)
                                                                               (day24/is-input? (elt value 2))))
                                                                        state))))
    (day24/update-selection (lambda (symbol value)
                              (case (car value)
                                ;; Boring, non resolvable stuff
                                ((:add :mul) (list symbol value))
                                ;; Equals are nore interesting
                                (:eql (let ((other-value (elt value 2)))
                                        (if (and (numberp other-value)
                                                 (or (<= other-value 0)
                                                     (>= other-value 10)))
                                            (list symbol 0)
                                          (list symbol value))))
                                ;; mod by anything >= 10 is an identity
                                (:mod (let ((other-value (elt value 2)))
                                        (if (> other-value 9)
                                            (list symbol (elt value 1)))))
                                ;; (truncate (div input value)) by anyting greater than 9 will always yield 0
                                (:div (let ((other-value (elt value 2)))
                                        (if (> other-value 9)
                                            (list symbol 0))))                                
                                (t (error (format "Unexpected input operation for %s" value)))))
                            (day24/select-expr (lambda (symbol value)
                                                 (and (listp value)
                                                      (day24/is-input? (elt value 1))))
                                               right-input-resolved))))

(defun day24/resolve-special-values (state)
  (day24/resolve-ones (day24/resolve-zeroes state)))

(defun day24/map-expressions (f state)
  "Applies to each expression a (f old_expressions symbol value)

f should *not* modify the old expressions, returning nil will not write anything in the next db"
  (let* ((new-state (day24/copy-state state))
         (new-expressions (advent/table))
         (old-expressions (plist-get state :expressions)))
    (setq new-state (plist-put new-state :expressions new-expressions))
    (maphash (lambda (symbol value)
               (let ((result (funcall f old-expressions symbol value)))
                 (when result
                   (advent/put new-expressions (car result) (cadr result)))))
             old-expressions)
    new-state))

(defun day24/resolve--non-expression-symbol (db symbol)
  "Resolve the symbol to the next number/token/input if possible, return the original symbol otherwise"
  (assert symbol)
  (if (numberp symbol)
      symbol
    (let ((next-symbol (advent/get db symbol symbol)))
      (if (listp next-symbol)
          symbol
        next-symbol))))

(defun day24/resolve-numeric-expression (op a b)
  (case op
    (:add (+ a b))
    (:mul (* a b))
    (:div (truncate (/ a b)))
    (:mod (mod a b))
    (:eql (if (= a b) 1 0))
    (t (error (format "Unhandled operator: %s" op)))))

(defun day24/resolve--list-value (db value)
  "Resolve each member of a list, if both are number also compute the result"
  (let ((op (elt value 0))
        (op1 (day24/resolve--non-expression-symbol db (elt value 1)))
        (op2 (day24/resolve--non-expression-symbol db (elt value 2))))
    (if (not (and (numberp op1) (numberp op2)))
        (list op op1 op2)
      (day24/resolve-numeric-expression op op1 op2))))

(defun day24/replace-once (state)
  "Returns a new state where all variables that can be resolved to a single symbol or number are replaced once

Possibly replace expressions with their numerical value"
  (day24/map-expressions (lambda (db symbol value)
                           (cond
                            ;; numeric values are of no interest
                            ((numberp value)
                             (list symbol value))
                            ;; symbolic values may be
                            ((symbolp value)
                             (list symbol (day24/resolve--non-expression-symbol db value)))
                            ;; inp by definition is not interesting
                            ((eq (car value) :inp)
                             (list symbol value))
                            ;; Lists are where the action is
                            ((listp value)
                             (list symbol (day24/resolve--list-value db value )))
                            ;; There should be nothing else
                            (t (error (format "Unhandled value: %s" value)))))
                         state))

(defun day24/resolve--indirect-range (state ranges symbol)  
  "If a symbol can be resolved through indirection return that, otherwise nil"
  (if-let ((range (advent/get ranges symbol)))
      range
    (if-let ((next-symbol (advent/get (plist-get state :expressions) symbol)))
        (day24/resolve--indirect-range state ranges next-symbol))))

(defun day24/tdiv (a b)
  (truncate (/ a b)))

(defun day24/resolve-range (value)
  "Returns a range for a value, or nil if the value can't be immediately resolved"
  (cond
   ((numberp value) (vector value value))
   ((day24/is-input? value) (vector 1 9))
   ((and (listp value) (eq (car value) :eql)) (vector 0 1))
   ((and (listp value) (eq (car value) :mod)) (vector 0 (1- (elt value 2))))))

(defun day24/try--hard-for-range (state ranges unspec-value)
  (or (day24/resolve-range unspec-value)
      (day24/resolve--indirect-range state ranges unspec-value)))

(defun day24/resolve--add-range (state ranges value)
  (assert (eq (car value) :add))
  (if-let ((op1-range (day24/try--hard-for-range state ranges (elt value 1)))
           (op2-range (day24/try--hard-for-range state ranges (elt value 2))))
      (vector (+ (aref op1-range 0)
                 (aref op2-range 0))
              (+ (aref op1-range 1)
                 (aref op2-range 1)))))

(defun day24/resolve--mul-range (state ranges value)
  (assert (eq (car value) :mul))
  (if-let ((op1-range (day24/try--hard-for-range state ranges (elt value 1)))
           (op2-range (day24/try--hard-for-range state ranges (elt value 2))))
      (apply #'vector
             (sort (values  (* (aref op1-range 0)
                               (aref op2-range 0))
                            (* (aref op1-range 1)
                               (aref op2-range 1)))
                   #'<))))

(defun day24/step-ranges (state ranges)
  (let ((ranges (copy-hash-table ranges)))
   (maphash (lambda (symbol value)
              (cond
               ;; numbers are a 1 value range
               ((numberp value)
                (advent/put ranges symbol (day24/resolve-range value)))
               ;; inputs are between 1 and 9 included
               ((day24/is-input? value)
                (advent/put ranges symbol (day24/resolve-range value)))
               ;; symbols that are not inputs could be in theory resolved from the reference
               ;; since the connection is not necessarily resolved in the expressions
               ((symbolp value)
                (if-let ((range (day24/resolve--indirect-range state ranges value)))
                    (advent/put ranges symbol range)))
               ;; equals are always 0 or 1
               ((and (listp value) (eq (car value) :eql))
                (advent/put ranges symbol (day24/resolve-range value)))
               ;; mod compresses the results between 0 and the (divisor - 1)
               ((and (listp value) (eq (car value) :mod))
                (advent/put ranges symbol (day24/resolve-range value)))
               ;; div has always a second numeric operator and reduces a previous range, if available
               ((and (listp value)
                     (eq (car value) :div))
                (if-let ((op1-range (day24/try--hard-for-range state ranges (elt value 1))))
                    (advent/put ranges symbol (vector (day24/tdiv (aref op1-range 0) (elt value 2))
                                                      (day24/tdiv (aref op1-range 1) (elt value 2))))))
               ;; add
               ((and (listp value) (eq (car value) :add))
                (if-let ((range (day24/resolve--add-range state ranges value)))
                    (advent/put ranges symbol range)))
               ;; mul
               ((and (listp value) (eq (car value) :mul))
                (if-let ((range (day24/resolve--mul-range state ranges value)))
                    (advent/put ranges symbol range)))))
            (plist-get state :expressions))
   ranges))

(defun day24/to-string-r (ranges)
  (let ((result ""))
    (setq result (concat result "* RANGES:\n"))
    (maphash (lambda (symbol value)
               (setq result (concat result (format "  %s := %s\n" symbol value))))
             ranges)
    result))

(defun day24/compute-ranges (state)
  "Create a db of obvious ranges for the expressions"
  (let* ((ranges (advent/table))
         (new-ranges (day24/step-ranges state ranges)))
    (while (not (day24/same-tables? new-ranges ranges))
      (setq ranges new-ranges)
      (setq new-ranges (day24/step-ranges state ranges)))
    new-ranges))

(defun day24/no-range-intersection? (range1 range2)
  (or (> (aref range1 0) (aref range2 1))
      (> (aref range2 0) (aref range1 1))))

(defun day24/simplify-by-range (state)
  "Compute the ranges and then try to resolve all equals"
  (let ((ranges (day24/compute-ranges state)))
    (day24/update-selection (lambda (symbol value)                              
                              (if-let ((op1-range (day24/try--hard-for-range state ranges (elt value 1)))
                                       (op2-range (day24/try--hard-for-range state ranges (elt value 2))))
                                  ;; I can calculate the ranges                                  
                                  (if (day24/no-range-intersection? op1-range op2-range)
                                      ;; and there is no way they match an eql
                                      (list symbol 0)
                                    ;; there is even a slim intersection: we can't resolve the eql
                                    (list symbol value))
                                ;; I can't calculate the range (yet)
                                (list symbol value)))
                            (day24/select-expr (lambda (symbol value)
                                                 (and (listp value) (eq (car value) :eql)))
                                               state))))

(defun day24/simplify-once (state)
  (day24/simplify-by-range (day24/replace-once (day24/resolve-obvious-input-opertions (day24/resolve-special-values state)))))

(defun day24/same-tables? (table-a table-b)
  (and (= (hash-table-count table-a) (hash-table-count table-b))
       (not (--any (let ((a-value (advent/get table-a it))
                         (b-value (advent/get table-b it)))
                     (not (equal a-value b-value)))
                   (hash-table-keys table-a)))))

(defun day24/same-expressions? (state-a state-b)
  (let ((expr-a (plist-get state-a :expressions))
        (expr-b (plist-get state-b :expressions)))
    (day24/same-tables? expr-a expr-b)))

(defun day24/simplify-until-converged (state)
  (let ((new-state (day24/simplify-once state)))
    (while (not (day24/same-expressions? state new-state))
      (setq state new-state)
      (setq new-state (day24/simplify-once state)))
    new-state))

(defun day24/replace-symbol (value input-symbol input-value)  
  (cond
   ;; plain assignment
   ((eq value input-symbol) input-value)
   ;; list
   ((listp value) (--map (day24/replace-symbol it input-symbol input-value) value))
   ;; anything else
   (t value)))

(defun day24/set-input (state index input-value)
  (let* ((new-state (day24/copy-state state))
         (input-symbol (day24/make-input index))
         (new-db (plist-get new-state :expressions)))
    (maphash (lambda (symbol value)
               (advent/put new-db symbol (day24/replace-symbol value input-symbol input-value)))
             new-db)
    new-state))

(defun day24/force-input (state index input-value)
  (day24/simplify-until-converged (day24/set-input state index input-value)))

(defun day24/objective-feasible? (state)
  (let* ((ranges (day24/compute-ranges state))
         (z-goal (advent/get ranges :z0)))
    (and (>= 0 (aref z-goal 0))
         (<= 0 (aref z-goal 1)))))

;; caching would be good
(defun day24/search (cache state variables)
  (print (format "Inside %s" variables))
  (redisplay)
  (if (= (length variables) 14)
      variables
    (let ((next 9)
          (solved))
      (while (and (not solved) (>= next 1))
        (let ((forced-state (or (advent/get cache (cons next variables))
                                (day24/force-input state (- 13 (length variables)) next))))
          (advent/put cache (cons next variables) forced-state) ; won't hurt
          (if (day24/objective-feasible? forced-state)
              (let ((solved (day24/search cache forced-state (cons next variables))))
                (unless solved
                  (setq next (1- next)))
                )
            (setq next (1- next)))))
      (print solved)
      solved)))




(defun day24/part-1 (lines)
  (day24/search (advent/table) (day24/record-program (day24/read-opcodes lines)) nil))

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
