(require 'dash)
(require 'advent-utils)

(defun day8/read-code (line)
  (let ((raw-results (-split-on "|" (split-string line " " t))))
    (assert (= 10 (length (car raw-results))))
    (assert (= 4 (length (cadr raw-results))))
    (list :patterns (car raw-results)
          :output (cadr raw-results))))

(defun day8/read-codes (lines)
  (-map #'day8/read-code lines))

(defun day8/compute-special-digits-output-count (code)
  (length
   (--filter (case it
               ((2 4 3 7) t))
             (-map #'length code))))

(defun day8/part-1 (lines)
  (let ((codes (day8/read-codes lines)))
    (apply #'+
           (-map #'day8/compute-special-digits-output-count
                 (--map (plist-get it :output) codes)))))

(defun day8/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day8)
