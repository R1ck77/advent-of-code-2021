(require 'dash)
(require 'advent-utils)

(defconst header-size 6)

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

(defun day16/hex-to-bin (line)
  (--reduce-from (append acc (day16/char-to-bin it))
                 '()
                 (-map #'string-to-char (split-string line "" t))))

(defun day16/binary-data-to-number (bits)
  (string-to-number (apply #'concat (-map #'number-to-string bits)) 2))

(defun day16/read-num-subpackets (n data)
  "Return the remaining data, and the list of subpackets"
  (let ((remaining-data data)
        (packets))
    (loop for i below n do
          (let ((data-t-v-payload (day16/decode-packet remaining-data)))
            (setq remaining-data (car data-t-v-payload))
            (setq packets (cons (cons nil (rest data-t-v-payload)) packets))))
    (list remaining-data (nreverse packets))))

(defun day16/get--size-data (content bits)
  (let* ((sizebits-data (-split-at bits content))
         (size (day16/binary-data-to-number (car sizebits-data)))
         (data (cadr sizebits-data)))
    (list size data)))

(defun day16/decode-id-1 (content)
  "Returns the remaining data, if any, and the list of decodified packets"
  (let ((size-data (day16/get--size-data content 11)))
    (day16/read-num-subpackets (car size-data) (cadr size-data))))

(defun day16/read-fixed-length-subpackets (length data)
  "Return the remaining data, and the list of subpackets"
  (let* ((payload-rest (-split-at length data))
        (remaining-data (cadr payload-rest))
        (payload (car payload-rest))
        (packets))
    (while payload
      (let ((data-t-v-payload (day16/decode-packet payload)))
        (setq payload (car data-t-v-payload))
        (setq packets (cons (cons nil (rest data-t-v-payload)) packets))))
    (list remaining-data (nreverse packets))))

(defun day16/decode-id-1 (content)
  "Returns the remaining data, if any, and the list of decodified packets"
  (let ((size-data (day16/get--size-data content 11)))
    (day16/read-num-subpackets (car size-data) (cadr size-data))))

(defun day16/decode-id-0 (content)
  "Returns the remaining data, if any, and the list of decodified packets"
  (let ((size-data (day16/get--size-data content 15)))
    (day16/read-fixed-length-subpackets (car size-data) (cadr size-data))))

(defun day16/decode-operator (packet-t-v)
  "Returns the remaining data, the type, the version and a list of decodified packets"
  (let ((data (car packet-t-v))
        (type (elt packet-t-v 1))
        (version (elt packet-t-v 2)))
    (let* ((id (car data))
           (rest-list (if (zerop id)
                          (day16/decode-id-0 (rest data))
                        (day16/decode-id-1 (rest data)))))
      (list (car rest-list)
            type
            version
            (cadr rest-list)))))

(defun day16/debug--check-4 (digits)
  (let* ((rdigits (reverse digits))
        (last (car rdigits))
        (other (rest rdigits)))
    (assert (zerop (car last)))
    (--each other (assert (= 1 (car it))))))

(defun day16/pad-payload-data (data-bits)
  "Return the size of the payload in a way that the *complete* packet length is a multiple of 4"
  (comment (let ((remainder (mod (+ header-size data-bits) 4)))
     (if (zerop remainder)
         data-bits
       (+ data-bits (- 4 remainder)))))
  data-bits)

(defun day16/get-4-payload (data)
  "Returns two lists: the actual packet data, and the remaining data"  
  (let* ((blocks  (-partition 5 data))
         (1-blocks (length (--take-while (= (car it) 1) blocks))))
    ;; If I take all 1-blocks, the next block must start with a 0
    (assert (zerop (elt data (* 5 1-blocks))))
    ;; Compute the total unpadded *data* length
    (-split-at (day16/pad-payload-data (* 5 (1+ 1-blocks)))
               data)))

(defun day16/decode-4-payload (data)
  "Returns the value and the unprocessed data"
  (let ((payload-rest (day16/get-4-payload data)))
    (let ((digits (-partition 5 (car payload-rest))))
      (day16/debug--check-4 digits)
      (list (day16/binary-data-to-number (--reduce-from (append acc (rest it)) '() digits))
            (cadr payload-rest)))))

(defun day16/decode-4 (packet-t-v)
  "Returns the remaining data, the type, the version and the number"
  (let ((value-remaining (day16/decode-4-payload (car packet-t-v))))
    (list (cadr value-remaining)
          (elt packet-t-v 1)
          (elt packet-t-v 2)
          (car value-remaining))))

(defun day16/get-3-digits-number (packet)
  (let ((parts (-split-at 3 packet)))
    (list (cadr parts)
          (day16/3-digits-to-number (car parts)))))

(defun day16/decode-header (packet)
  "Returns a list (rest-of-packet type version) from a packet"
  (let* ((version-rest (day16/get--size-data packet 3))
         (type-rest (day16/get--size-data (cadr version-rest) 3)))
    (list (cadr type-rest) (car type-rest) (car version-rest))))

(defun day16/decode-packet (data)
  (let ((packet-t-v (day16/decode-header data)))
   (let ((payload (car packet-t-v))
         (type (elt packet-t-v 1))
         (version (elt packet-t-v 2)))
     (case type
       (4 (day16/decode-4 packet-t-v))
       (t (day16/decode-operator packet-t-v))))))

(defun day16/sum-versions (decoded-packet)
  (+ (elt decoded-packet 2)
     (let ((data (elt decoded-packet 3)))
       (if (listp data)
           (apply #'+ (-map #'day16/sum-versions data))
         0))))

(defun day16/part-1 (line)
  (day16/sum-versions
   (day16/decode-packet
    (day16/hex-to-bin line))))

(defun day16/id-to-operator (id)
  (case id
    (0 (lambda (&rest values) (apply #'+ values)))
    (1 (lambda (&rest values) (apply #'* values)))
    (2 (lambda (&rest values) (apply #'min values)))
    (3 (lambda (&rest values) (apply #'max values)))
    (4 (lambda (&rest values) (car values)))
    (5 (lambda (&rest values) (if (> (car values) (cadr values)) 1 0)))
    (6 (lambda (&rest values) (if (< (car values) (cadr values)) 1 0)))
    (7 (lambda (&rest values) (if (= (car values) (cadr values)) 1 0)))
    (t (error "Unsupported type!"))))

(defun day16/eval (decoded-packet)
  (let ((operator (day16/id-to-operator (elt decoded-packet 1)))
        (data (elt decoded-packet 3)))
    
    (apply operator (if (listp data)
                        (-map #'day16/eval data)
                      (list data)))))

(defun day16/part-2 (line)
  (day16/eval
   (day16/decode-packet
    (day16/hex-to-bin line))))

(provide 'day16)

