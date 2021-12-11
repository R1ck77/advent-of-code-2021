(require 'dash)
(require 'advent-utils)

(defconst codes '(((:a :b :c :e :f :g) . 0)
                  ((:c :f) . 1)
                  ((:a :c :d :e :g) . 2)
                  ((:a :c :d :f :g) . 3)
                  ((:b :c :d :f) . 4)
                  ((:a :b :d :f :g) . 5)
                  ((:a :b :d :e :f :g) . 6)
                  ((:a :c :f) . 7)
                  ((:a :b :c :d :e :f :g) . 8)
                  ((:a :b :c :d :f :g) . 9))
  "The digits encoded as plists")

(defconst day08/show-messages nil "Turn debug printouts on")

(defun day08/print (string)
  (if day08/show-messages 
      (print string)))

(defun day08/pattern-to-plist (pattern)
  "Turn a pattern into a list of keywords"
  (--map (intern (format ":%s" it))
         (-sort #'string< (split-string pattern "" t))))

(defun day08/read-code (line)
  (let ((raw-results (-split-on "|" (split-string line " " t))))
    (assert (= 10 (length (car raw-results))))
    (assert (= 4 (length (cadr raw-results))))
    (list :patterns (-map #'day08/pattern-to-plist (car raw-results))
          :output (-map #'day08/pattern-to-plist (cadr raw-results)))))

(defun day08/read-codes (lines)
  (-map #'day08/read-code lines))

(defun day08/compute-special-digits-output-count (code)
  (length
   (--filter (case it
               ((2 4 3 7) t))
             (-map #'length code))))

(defun day08/part-1 (lines)
  (let ((codes (day08/read-codes lines)))
    (apply #'+
           (-map #'day08/compute-special-digits-output-count
                 (--map (plist-get it :output) codes)))))

(defun day08/get-x-digits (patterns x)
  "Return all patterns with x digits"
  (--filter (= (length it) x) patterns))

(defun day08/decode-digit (conversion digit)
  (day08/print (format "Original digit: %s" digit))
  (let ((converted (--sort (string< (symbol-name it)
                                    (symbol-name other))
                           (--map (advent/get conversion it) digit))))
    (day08/print (format "Converted digit: %s -> '%s'" converted (alist-get converted codes nil nil 'equal)))
    (alist-get converted codes nil nil 'equal)))

(defun day08/decode-output (conversion output)
  (string-to-number
   (apply #'concat
          (-map #'number-to-string
                (--map (day08/decode-digit conversion it) output)))))

(defun day08/get-missing-lines (pattern)
  (--filter (not (memq it pattern)) '(:a :b :c :d :e :f :g)))

(defun day08/union (patterns)
  (delete-dups (copy-sequence (apply #'append patterns))))

(defun day08/get-missing-from-all (patterns)
  (day08/get-missing-lines (day08/union patterns)))

(defun day08/safe-remove (pattern value)
  (delq value (copy-sequence pattern)))

(defun day08/get--d-and-e (patterns c)
  (let ((missing-from-5-digit (-map #'day08/get-missing-lines (day08/get-x-digits patterns 5))))
    ;; Find the missing list that has the "c" -> the other is the "e"
    (let* ((5-value-missing (car (--filter (memq c it) missing-from-5-digit)))
           (e (car (day08/safe-remove 5-value-missing c))))
      ;; Find the missing lines from the 6: the one that's not a 'e' or a 'c' is a d
      (let ((6-digits-missing (-map #'day08/get-missing-lines
                                    (day08/get-x-digits patterns 6))))
        (assert (= (length 6-digits-missing) 3))
        (let ((d (caar (--filter (not
                                  (or (memq c it)
                                      (memq e it)))
                                 6-digits-missing))))
          (list d e))))))

(defun day08/get--a (patterns c f)
  (let ((7-sequence (car (day08/get-x-digits patterns 3)) ))
    (car (delete f (delete c (copy-sequence 7-sequence))))))

(defun day08/get--c-and-f (patterns)
  "Get 'c' the missing line from 2 digit that is also missing in one of the 6 digits (the other is f)"
  (let ((2-digit (car (day08/get-x-digits patterns 2)))
        (6-digits-missing-lines (-map #'day08/get-missing-lines (day08/get-x-digits patterns 6))))
    (let ((first-2-digit-missing (car 2-digit))
          (second-2-digit-missing (cadr 2-digit))
          (c-f nil))
      (if (--any (memq first-2-digit-missing it) 6-digits-missing-lines)
          2-digit
        (reverse 2-digit)))))

(defun day08/get--b (patterns f)
  (let ((b-f (car
              (--filter (memq f it)
                        (-map #'day08/get-missing-lines
                              (day08/get-x-digits patterns 5))))))
    (car (day08/safe-remove b-f f))))

(defun day08/get-line (conversion line)
  (advent/get conversion line))

(defun day08/get--g (conversion)
  (let ((values nil))
    (maphash (lambda (k v)
               (setq values (cons v values)))
             conversion)
    (car (--filter (not (memq it values)) '(:a :b :c :d :e :f :g)))))

(defun day08/invert-conversion (conversion)
  (let ((result (advent/table)))
    (maphash (lambda (k v)
               (advent/put result v k))
             conversion)
    result))

(defun day08/debug--dictionary (conversion)
  (day08/print "Dictionary: ")
  (maphash (lambda (k v)
             (day08/print (format "| %s -> %s" k v)))
           conversion)
  conversion)

(defun day08/find-conversion-table (patterns)
  (day08/print "=== Patterns:")
  (--each patterns (day08/print (format "%d :: %s" (length it) it)))
  (day08/print "===")
  "Logic to decode the pattern"
  (let ((conversion (advent/table)))
    ;; Decode the C and F lines
    (let ((c-f (day08/get--c-and-f patterns)))
      (advent/put conversion :c (car c-f))
      (advent/put conversion :f (cadr c-f)))
    ;; Decode A using c and f
    (advent/put conversion :a (day08/get--a patterns
                                           (day08/get-line conversion :c)
                                           (day08/get-line conversion :f)))
    ;; Decode B using f
    (advent/put conversion :b (day08/get--b patterns (day08/get-line conversion :f)))
    ;; Decode the D and E lines using c
    (let ((d-e (day08/get--d-and-e patterns (day08/get-line conversion :c))))
      (advent/put conversion :d (car d-e))
      (advent/put conversion :e (cadr d-e)))    
    ;; Decode G as the only value not yet decoded
    (advent/put conversion :g (day08/get--g conversion))    

    (day08/debug--dictionary conversion)
    (day08/debug--dictionary (day08/invert-conversion conversion))))

(defun day08/decode-input (code)
  "Read a pattern/output code and returns the output value"
  (let* ((patterns (plist-get code :patterns))
         (conversion (day08/find-conversion-table patterns)))
    (day08/decode-output conversion (plist-get code :output))))

(defun day08/part-2 (lines)
  (apply #'+
         (-map #'day08/decode-input
               (day08/read-codes lines))))

(provide 'day08)
