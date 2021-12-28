;; -*- lexical-binding: t -*-
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

(defun day24/eql (a b)
  (if (= a b) 1 0))

(defun day24/next-instruction (op inputs registers)
  "Returns a list"
  (if (eq (car op) :inp)
      (progn 
        (setq registers (plist-put registers (cadr op) (pop inputs)))
        (list :inputs inputs :registers registers))
    (let* ((op1 (elt op 1))
           (op1-value (plist-get registers op1))
           (op2 (or (plist-get registers (elt op 2)) (elt op 2))))
      (list :inputs inputs
            :registers (plist-put registers op1 (case (car op)
                                                  (:add (+ op1-value op2))
                                                  (:mul (* op1-value op2))
                                                  (:eql (day24/eql op1-value op2))
                                                  (:div (truncate (/ op1-value op2)))
                                                  (:mod (mod op1-value op2))
                                                  (t (error "Unexpected operation"))))))))

(defun day24/evaluate-program (program inputs &optional w x y z)
  "Evaluate a list of instructions"
  (let ((registers (list :x (or x 0)
                         :y (or y 0)
                         :z (or z 0)
                         :w (or w 0))))
    (--reduce-from (day24/next-instruction it (plist-get acc :inputs) (plist-get acc :registers))
                   (list :inputs inputs
                         :registers registers)
                   program)))

(defun day24/evaluate-lines (lines inputs &optional x y z w)
  "Evaluates a program (as text lines). Useful to double check results."
  (day24/evaluate-program (day24/read-opcodes lines) inputs w x y z))

(defun day24/debug--read-as-blocks (lines)
  "Read the lines of the program as blocks of instructions from inp to inp"
  (nreverse
   (-map #'nreverse (--reduce-from (if (eq (car it) :inp)
                                       (cons (list it) acc)
                                     (cons (cons it (car acc)) (rest acc)))
                                   nil
                                   (day24/read-opcodes lines)))))

(defvar day24/debug-blocks (day24/debug--read-as-blocks (advent/read-problem-lines 24 :problem))
  "List of blocks of code. Useful when coupled with day24/evaulate-lines to double check the hand-crafted function results")

;; manual information extracted from the input:
;; it's a list of variable constants from each code block
(defvar day24/synthesized-blocks '((1 14 12)
                                   (1 11 8)
                                   (1 11 7)
                                   (1 14 4)
                                   (26 -11 4)  ; this can be =
                                   (1 12 1)
                                   (26 -1 10)  ; this can be =
                                   (1 10 8)
                                   (26 -3 12)  ; this can be = 
                                   (26 -4 10)  ; this can be =
                                   (26 -13 15) ; this can be = 
                                   (26 -8 4)   ; this can be =
                                   (1 13 10)
                                   (26 -11 9))) ; this can be =

(defun day24/block (input old-z A B C)
  "Returns a (triggered new-z) list. Created from reading my specific input"
  (let* ((inner-block (+ (mod old-z 26) B)))    
    (if (= inner-block input)
        (list t (/ old-z A))
      (list nil (+ (* (/ old-z A) 26) input C)))))

(defun day24/block-n (index input old-z)
  (let ((block (elt day24/synthesized-blocks index)))
    (day24/block input old-z
                 (elt block 0)
                 (elt block 1)
                 (elt block 2))))

(defun day24/execute-n-blocks (inputs &optional start start-z)
  (-reduce-from (lambda (t-oldz index-input)
                  (day24/block-n (+ (or start 0)
                                    (car index-input))
                                 (cdr index-input) (cadr t-oldz) ))
                (list nil (or start-z 0))
                (--map-indexed (cons it-index it) inputs)))

(defun day24/add-layer (coordinates)
  (let ((results ()))
    (loop for i from 9 downto 1 do
          (setq results (append results (--map (cons i it) coordinates))))
    results))

(defun day24/grid-n (n)
  (--reduce-from 
   (day24/add-layer acc)
   (-map #'list (number-sequence 9 1 -1))
   (number-sequence 1 (1- n)))  )

(defun day24/optimize-n (first-block n z)
  (print (format "Optimizing blocks %s" (number-sequence first-block (1- (+ first-block n)))))
  (redisplay)
  (let* ((all-points (day24/grid-n n)))
    (nreverse
     ;; first index is the first served and the largest digit
     (--filter (car (cadr it))
               (--map (list it (day24/execute-n-blocks it first-block z))
                      all-points)))))

(defun day24/optimize-from-previous-results (previous first-block n)
  (print (format "Optimizing blocks %s" (number-sequence first-block (1- (+ first-block n)))))
  (redisplay)
  (let* ((new-points (day24/grid-n n))
         (results))
    (--each previous
      (let* ((old-indices (car it))
             (old-z (cadr (cadr it))))
        ;; only the 2 points coordinates here!
        (let ((inner-results (--filter (or (car (cadr it))
;                                           (zerop (cadr (cadr it)))
                                           )
                                       (--map (list it (day24/execute-n-blocks it first-block old-z)) new-points))))
          (-each (nreverse inner-results)
            (lambda (inner-result)
              (push (list (append old-indices (car inner-result))
                          (cadr inner-result))
                    results))))))
    (nreverse results)))

(defun day24/optimize-it-all ()
  "Returns the smallest and largest solution"
  (let* ((first-5-blocks (day24/optimize-n 0 5 0))
         (first-7-blocks (day24/optimize-from-previous-results first-5-blocks 5 2))
         (first-9-blocks (day24/optimize-from-previous-results first-7-blocks 7 2))
         (first-10-blocks (day24/optimize-from-previous-results first-9-blocks 9 1))
         (first-11-blocks (day24/optimize-from-previous-results first-10-blocks 10 1))
         (first-12-blocks (day24/optimize-from-previous-results first-11-blocks 11 1))
         (first-13-blocks (day24/optimize-from-previous-results first-12-blocks 12 2)))
    (list (caar first-13-blocks)
          (caar (last first-13-blocks)))))

(defun day24/part-1 ()
  "This solution completely disregards the input: the relevant information was extracted manually"
  (string-to-number
   (apply #'concat
          (-map #'number-to-string (cadr (day24/optimize-it-all))))))

(defun day24/part-2 ()
  "This solution completely disregards the input: the relevant information was extracted manually"
  (string-to-number
   (apply #'concat
          (-map #'number-to-string (car (day24/optimize-it-all))))))

(provide 'day24)

