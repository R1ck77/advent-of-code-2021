(require 'dash)
(require 'advent-utils)

(defun day16/char-to-bin (char)
  (case char
    (?0  (list 0 0 0 0))
    (?1  (list 0 0 0 1))
    (?2  (list 0 0 1 0))
    (?3  (list 0 0 1 1))
    (?4  (list 0 1 0 0))
    (?5  (list 0 1 0 1))
    (?6  (list 0 1 1 0))
    (?7  (list 0 1 1 1))
    (?8  (list 1 0 0 0))
    (?9  (list 1 0 0 1))
    (?A  (list 1 0 1 0))
    (?B  (list 1 0 1 1))
    (?C  (list 1 1 0 0))
    (?D  (list 1 1 0 1))
    (?E  (list 1 1 1 0))
    (?F  (list 1 1 1 1))))

(defun day16/3-digits-to-number (bits)
  (cond
   ((equal bits '(0 0 0)) 0)
   ((equal bits '(0 0 1)) 1)
   ((equal bits '(0 1 0)) 2)
   ((equal bits '(0 1 1)) 3)
   ((equal bits '(1 0 0)) 4)
   ((equal bits '(1 0 1)) 5)   
   ((equal bits '(1 1 0)) 6)
   ((equal bits '(1 1 1)) 7)))

(defun day16/hex2bin (line)
  (--reduce-from (append acc (day16/char-to-bin it))
                 '()
                 (-map #'string-to-char (split-string line "" t))))

(defun day16/get-3-digits-number (packet)
  (let ((parts (-split-at 3 packet)))
    (list (cadr parts)
          (day16/3-digits-to-number (car parts)))))

(defun day16/get--version (packet)
  "Returns a list (rest-of-packet version)"
  (day16/get-3-digits-number packet))

(defun day16/get--type (v-packet)
  "Returns a list (rest-of-packet type version) from a (rest-of-packet version) list"
  (let ((rest-type (day16/get-3-digits-number (car v-packet))))
    (list (car rest-type) (cadr rest-type) (cadr v-packet))))

(defun day16/decode-packet (packet-t-v)
  (let ((payload (car packet-t-v))
        (type (elt packet-t-v 1))
        (version (elt packet-t-v 2)))))

(defun day16/decode-header (packet)
  "Returns a list (rest-of-packet type version) from a packet"
  (day16/get--type (day16/get--version packet)))

(defun day16/part-1 (lines)
  (error "Not yet implemented"))

(defun day16/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day16)
