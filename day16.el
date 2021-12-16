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

(defun day16/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day16)

(defvar v6 "D2FE28")
(defvar v1 "38006F45291200")
(defvar v7 "EE00D40C823060")
(defvar sum12 "620080001611562C8802118E34")
(defvar problem "020D74FCE27E600A78020200DC298F1070401C8EF1F21A4D6394F9F48F4C1C00E3003500C74602F0080B1720298C400B7002540095003DC00F601B98806351003D004F66011148039450025C00B2007024717AFB5FBC11A7E73AF60F660094E5793A4E811C0123CECED79104ECED791380069D2522B96A53A81286B18263F75A300526246F60094A6651429ADB3B0068937BCF31A009ADB4C289C9C66526014CB33CB81CB3649B849911803B2EB1327F3CFC60094B01CBB4B80351E66E26B2DD0530070401C82D182080803D1C627C330004320C43789C40192D002F93566A9AFE5967372B378001F525DDDCF0C010A00D440010E84D10A2D0803D1761045C9EA9D9802FE00ACF1448844E9C30078723101912594FEE9C9A548D57A5B8B04012F6002092845284D3301A8951C8C008973D30046136001B705A79BD400B9ECCFD30E3004E62BD56B004E465D911C8CBB2258B06009D802C00087C628C71C4001088C113E27C6B10064C01E86F042181002131EE26C5D20043E34C798246009E80293F9E530052A4910A7E87240195CC7C6340129A967EF9352CFDF0802059210972C977094281007664E206CD57292201349AA4943554D91C9CCBADB80232C6927DE5E92D7A10463005A4657D4597002BC9AF51A24A54B7B33A73E2CE005CBFB3B4A30052801F69DB4B08F3B6961024AD4B43E6B319AA020020F15E4B46E40282CCDBF8CA56802600084C788CB088401A8911C20ECC436C2401CED0048325CC7A7F8CAA912AC72B7024007F24B1F789C0F9EC8810090D801AB8803D11E34C3B00043E27C6989B2C52A01348E24B53531291C4FF4884C9C2C10401B8C9D2D875A0072E6FB75E92AC205CA0154CE7398FB0053DAC3F43295519C9AE080250E657410600BC9EAD9CA56001BF3CEF07A5194C013E00542462332DA4295680")
