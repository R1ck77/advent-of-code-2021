(require 'dash)
(require 'advent-utils)

(defun day3/bits-to-number (bits)
  "Convert a list of bits to a unsigned number.

The list has least significant bits first."
  (let ((base 1)
        (sum 0))
    (while bits
      (setq sum (+ sum (* base (car bits))))
      (setq base (* base 2))
      (setq bits (cdr bits))      
      )
    sum))

(defun day3/complement (bits)
  (--map (- 1 it) bits))

(defun day3/sum-lists (acc value)
  (--map (+ (car it) (cdr it))
         (-zip acc value)))

(defun day3/count-ones (bits-list)
  "Returns a list with the number of one in place"
  (-reduce-from #'day3/sum-lists
                 (-repeat (length (car bits-list)) 0)
                 bits-list))

(defun day3/get-moda-on-bits (bits-list)
  (let ((threshold (/ (length bits-list) 2)))
   (--map (if (> it threshold) 1 0)
          (day3/count-ones bits-list))))

(defun day3/get-bits (line)
  "Get a list of bits from a line in least significant bits order first"
  (reverse
   (-map #'string-to-number
         (split-string line "" t))))

(defun day3/read-bits-list (input)
  (-map #'day3/get-bits input))

(defun day3/part-1 (input)
  (let ((moda (day3/get-moda-on-bits (day3/read-bits-list input))))
    (let ((gamma-rate (day3/bits-to-number moda))
          (epsilon-rate (day3/bits-to-number (day3/complement moda))))
     (* gamma-rate epsilon-rate))))

(defun day3/part-2 (input)
  (error "Not yet implemented"))

(provide 'day3)
