(require 'dash)
(require 'advent-utils)

(defun day14/letter-to-token (letter)
  (intern (concat ":" letter)))

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

(defun day14/add-pair-to-frequencies (freq-acc pair)
  (let ((token (car pair)))
    (advent/-update freq-acc token (1+ it-value) 0)))

(defun day14/create-empty-frequencies ()
  (advent/table))

(defun day14/count-pair (cache pair rules times)
  "Expands a pair n times and returns the accumulator with the frequencies"
  (if (zerop times)
      (day14/add-pair-to-frequencies (day14/create-empty-frequencies) pair)
    (let ((cache-key (list pair times)))
      (or (advent/get cache cache-key)
          (advent/cache cache cache-key (let ((evolved (day14/get-replacement rules pair)))
             (if (= (length evolved) 1)
                 (day14/add-pair-to-frequencies (day14/create-empty-frequencies) (car evolved))
               (day14/count-polymer cache evolved rules (1- times)))))))))

(defun day14/accumulate-frequencies (freq-acc-1 freq-acc-2)
  (advent/each-hash freq-acc-2 (lambda (k v)
             (advent/-update freq-acc-1 k (+ v it-value) 0)))
  freq-acc-1)

(defun day14/count-polymer (cache polymer rules times)
  "Return the frequencies accumulator for the polymer"
  (--reduce-from (day14/accumulate-frequencies acc (day14/count-pair cache it rules times))
                 (day14/create-empty-frequencies)
                 polymer))

(defun day14/expand (template times)
  (let ((polymer (car template)))
    (let ((frequencies (day14/count-polymer (advent/table) polymer (cadr template) times)))
      (advent/-update frequencies
                      (cadr (car (last polymer)))
                      (1+ it-value)
                      0))))

(defun day14/get-polymer-chain (template)
  "Convert the chain of pairs into a chain of tokens"
  (let ((polymer (car template)))
    (nreverse (cons (cadr (car (last polymer)))
           (--reduce-from (cons (car it) acc)
                          '()
                          polymer)))))

(defun day14/create-nice-frequency (frequency)
  (advent/-map-hash frequency
    (cons it-key it-value)))

(defun day14/frequency-difference (nice-frequency)
  (let ((frequencies (sort (-map #'cdr nice-frequency) #'<)))
    (- (car (last frequencies)) (car frequencies))))

(defun day14/solve-for  (blocks steps)
  (day14/frequency-difference
   (day14/create-nice-frequency
    (day14/expand (day14/read-template blocks) steps))))

(defun day14/part-1 (blocks)
  (day14/solve-for blocks 10))

(defun day14/part-2 (blocks)
  (day14/solve-for blocks 40))

(provide 'day14)

