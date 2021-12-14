(require 'dash)
(require 'advent-utils)

(defun day14/letter-to-token (letter)
  (intern (concat ":" letter)))

(defun day14/token-to-letter (token)
  (substring (symbol-name token) 1))

(defun day14/read-polymer (line)
  (let ((all-tokens (-map #'day14/letter-to-token
                          (split-string line "" t))))
    (-partition-in-steps 2 1 all-tokens)))

(defun day14/read-rule (line)
  "Turns NC -> H into ((:N :C) ((:N :H) (:C :H))"
  (let* ((tokens (split-string line))
         (starting (car (day14/read-polymer (car tokens))))
         (new-token (day14/letter-to-token (elt tokens 2))))
    (list starting (list (list (car starting) new-token)
                         (list new-token (cadr starting))))))

(defun day14/debug--print-rules (template)  
  (let ((rules (cadr template))
        (result ""))
    (maphash (lambda (k v)
               (setq result
                     (concat result (format "%s -> %s\n" k v))))
             rules)
    result))

(defun day14/read-rules (lines)
  (let ((rules (advent/table)))
    (--each (-map #'day14/read-rule lines)
      (advent/put rules (car it) (cadr it)))
    rules))

(defun day14/read-template (blocks)
  (let ((polymer (day14/read-polymer (caar blocks)))
        (rules (day14/read-rules (cadr blocks))))
    (list polymer rules)))

(defun day14/get-replacement (rules e-e)
  "Returns a list with the new elements (either two new couples or e-e again)"
  (advent/get rules e-e (list e-e)))

;; DUMB approach in n steps
(defun day14/insert (polymer rules)
  "Return a new polymer expanded with the new rules"
  (--reduce-from (let ((replacement (day14/get-replacement rules it)))
                   (append acc replacement))
                 '()
                 polymer))

(defun day14/expand (template times)
  (if (zerop times)
      template
    (let ((new-polymer (apply #'day14/insert template)))
      (day14/expand (list new-polymer (cadr template))
                    (1- times)))))

(defun day14/get-polymer-chain (template)
  "Convert the chain of pairs into a chain of tokens"
  (let ((polymer (car template)))
    (nreverse (cons (cadr (car (last polymer)))
           (--reduce-from (cons (car it) acc)
                          '()
                          polymer)))))

(defun day14/format-polymer (template)
  (let ((chain (day14/get-polymer-chain template)))
    (apply #'concat (-map #'day14/token-to-letter chain))))

(defun day14/frequency (template)
  (let ((frequency (advent/table))
        (chain (day14/get-polymer-chain template)))
    (--each chain
      (advent/put frequency it (1+ (advent/get frequency it 0))))
    (let ((nice-frequency))
      (maphash (lambda (k v)
                 (setq nice-frequency (append nice-frequency (list k v)))
                 )
               frequency)
      nice-frequency)))

(defun day14/frequency-difference (template)
  (let ((frequencies (sort (-map #'cadr (-partition 2 (day14/frequency template))) #'<)))
    (- (car (last frequencies)) (car frequencies))))

(defun day14/part-1 (blocks)
  (day14/frequency-difference
   (day14/expand (day14/read-template blocks) 10)))

(defun day14/part-2 (blocks)
  (error "Not yet implemented"))

(provide 'day14)

