(require 'dash)
(require 'advent-utils)

(defun day14/letter-to-token (letter)
  (intern (concat ":" letter)))

(defun day14/token-to-letter (token)
  (substring (symbol-name) 1))

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

(defun day14/expand (template times)
  template)

(defun day14/format-polymer (template)
  (let* ((polymer (car template))
         (all-but-last (apply #'day14/token-to-letter (-map #'car polymer)))
         (last-pair (car (cdr (car (last polymer))))))
   ))

(defun day14/part-1 (blocks)
  (defun exp(day14/read-template lines))
  (error "Not yet implemented"))

(defun day14/part-2 (blocks)
  (error "Not yet implemented"))

(provide 'day14)

