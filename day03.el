(require 'dash)
(require 'advent-utils)

(defun day03/bits-to-number (bits)
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

(defun day03/complement (bits)
  (--map (if (eq it :tie)
             :tie
           (- 1 it))
         bits))

(defun day03/sum-lists (acc value)
  (--map (+ (car it) (cdr it))
         (-zip acc value)))

(defun day03/count-ones (bits-list)
  "Returns a list with the number of one in place"
  (-reduce-from #'day03/sum-lists
                 (-repeat (length (car bits-list)) 0)
                 bits-list))

(defun day03/get-moda-on-bits (bits-list)
  (let ((total (length bits-list) ))
    (--map (cond
            ((> it (- total it)) 1)
            ((< it (- total it)) 0)
            (t :tie)) 
           (day03/count-ones bits-list))))

(defun day03/get-bits (line)
  "Get a list of bits from a line in least significant bits order first"
  (reverse
   (-map #'string-to-number
         (split-string line "" t))))

(defun day03/read-bits-list (input)
  (-map #'day03/get-bits input))

(defun day03/part-1 (input)
  (let ((moda (day03/get-moda-on-bits (day03/read-bits-list input))))
    (let ((gamma-rate (day03/bits-to-number moda))
          (epsilon-rate (day03/bits-to-number (day03/complement moda))))
      (* gamma-rate epsilon-rate))))

(defun day03/compute-advanced-coefficient (bits-list selection-function)
  (let ((current-bit (length (car bits-list))))
    (while (> (length bits-list) 1)
      (setq current-bit (1- current-bit))
      (let* ((selection-value (funcall selection-function bits-list current-bit)))
        (setq bits-list (--filter (= (elt it current-bit) selection-value)
                                  bits-list))))
    (day03/bits-to-number (car bits-list))))

(defun day03/oxygen-generator-rating-bit-criteria (bits-list index)
  (let ((raw-value (elt (day03/get-moda-on-bits bits-list) index)))
    (if (eq :tie raw-value)
        1
      raw-value)))

(defun day03/compute-oxygen-generator-rating (bits-list)
  (day03/compute-advanced-coefficient bits-list #'day03/oxygen-generator-rating-bit-criteria))

(defun day03/co2-scrubber-bit-criteria (bits-list index)
  (let ((raw-value (elt (day03/complement (day03/get-moda-on-bits bits-list)) index)))
    (if (eq :tie raw-value) 0 raw-value)))

(defun day03/compute-co2-scrubber-rating (bits-list)
  (day03/compute-advanced-coefficient bits-list #'day03/co2-scrubber-bit-criteria))

(defun day03/part-2 (input)
  (let ((bits-list (day03/read-bits-list input)))
    (let ((oxygen-generator-rating (day03/compute-oxygen-generator-rating bits-list))
          (co2-scrubber-rating (day03/compute-co2-scrubber-rating bits-list)))
      (* oxygen-generator-rating co2-scrubber-rating))))

(provide 'day03)
