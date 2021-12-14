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

(defun day14/create-empty-frequencies ()
  (advent/table))

(defun day14/add--to-frequencies (freq-acc token)
  (advent/put freq-acc token (1+ (advent/get freq-acc token 0)))
  freq-acc)

(defun day14/add-frequencies (freq-acc pair)
  (day14/add--to-frequencies freq-acc (car pair)))

;; TODO/FIXME this shouldn't exist
(defun day14/cache (cache cache-key result)
  (advent/put cache cache-key result)
  result)

(defun day14/count-pair (cache pair rules times)
  "Expands a pair n times and returns the accumulator with the frequencies"
  (if (zerop times)
      (day14/add-frequencies (day14/create-empty-frequencies) pair)
    (let ((cache-key (list pair times)))
      (or (advent/get cache cache-key)
          (day14/cache cache cache-key (let ((evolved (day14/get-replacement rules pair)))
             (if (= (length evolved) 1)
                 (day14/add-frequencies (day14/create-empty-frequencies) (car evolved))
               (day14/count-polymer cache evolved rules (1- times)))))))))

;; TODO/FIXME update function for tables and a decent maphash
(defun day14/accumulate-frequencies (freq-acc-1 freq-acc-2)
  (maphash (lambda (k v)
             (advent/put freq-acc-1 k (+ (advent/get freq-acc-1 k 0) v)))
           freq-acc-2)
  freq-acc-1)

(defun day14/count-polymer (cache polymer rules times)
  "Return the frequencies accumulator for the polymer"
  (--reduce-from (day14/accumulate-frequencies acc (day14/count-pair cache it rules times))
                 (day14/create-empty-frequencies)
                 polymer))

(defun day14/expand (template times)
  (let ((polymer (car template))
        (cache (advent/table)))    
    (day14/add--to-frequencies (day14/count-polymer cache polymer (cadr template) times)
                               (cadr (car (last polymer))))))

(defun day14/get-polymer-chain (template)
  "Convert the chain of pairs into a chain of tokens"
  (let ((polymer (car template)))
    (nreverse (cons (cadr (car (last polymer)))
           (--reduce-from (cons (car it) acc)
                          '()
                          polymer)))))

(defun day14/create-nice-frequency (frequency)
  (let ((nice-frequency))
    (maphash (lambda (k v)
               (setq nice-frequency (append nice-frequency (list k v))))
             frequency)
    nice-frequency))

(defun day14/frequency-difference (nice-frequency)
  (let ((frequencies (sort (-map #'cadr (-partition 2 nice-frequency)) #'<)))
    (- (car (last frequencies)) (car frequencies))))

(defun day14/solve-for  (blocks steps)
  (day14/frequency-difference
   (day14/create-nice-frequency
    (day14/expand (day14/read-template blocks) steps))))

(defmacro day14/time (&rest forms)
  "Time the forms and return a cons with the time in ms and the result"
  (declare (indent 0))
  (let ((start-time (make-symbol "start-time"))
        (result (make-symbol "result")))
    `(let ((,start-time (float-time))
           (,result))
       (setq ,result (progn ,@forms))
       (list (- (float-time) ,start-time)
             ,result))))

(defun day14/part-1 (blocks)
  (day14/solve-for blocks 10))

(defun day14/part-2 (blocks)
  (day14/solve-for blocks 40))

(provide 'day14)

(defun day14/test-solution ()
  (let ((input (advent/read-blocks-of-lines 14 :example))
        (steps 10)
        (time 0))
    (while (< time 30)
      (let ((time-result (day14/time (day14/solve-for input steps))))
        (setq time (car time-result))
        (print (format "%2d    %s\"   %d" steps time (cadr time-result)))
        (setq steps (1+ steps))
        (redisplay)))))

