(require 'dash)
(require 'advent-utils)

(defconst day20/displacements '((-1 . -1)
                                (-1 . 0)
                                (-1 . 1)
                                (0 . -1)
                                (0 . 0)
                                (0 . 1)
                                (1 . -1)
                                (1 . 0)
                                (1 . 1)))

(defun day20/string-to-pixel (schar)
  (cond
   ((string= schar "#") 1)
   ((string= schar ".") 0)
   (t (error (format "Unexpected code point: '%s'" schar)))))

(defun day20/pixel-to-string (value)
  (cond
   ((= value 1) "#")
   ((= value 0) ".")
   (t (error (format "Unexpected value: '%d'" value)))))

(defun day20/read-code (line)
  (apply #'vector (-map #'day20/string-to-pixel (split-string line "" t))))

(defun day20/read-image (lines)
  (let ((grid (advent/lines-to-grid lines #'day20/string-to-pixel))
        (data (advent/table)))
    (advent/loop-grid grid
      (advent/put data it (advent/grid-get grid it)))
    data))

(defun day20/read-data (blocks)
  (list :code (day20/read-code (caar blocks))
        :image (day20/read-image (cadr blocks))
        :inf 0))

(defun day20/get-neighbors-coords (coord)
  (--map (cons (+ (car it) (car coord))
               (+ (cdr it) (cdr coord)))
         day20/displacements))

(defun day20/neighbors-to-key (neighbor-values)
  (string-to-number (apply #'concat (-map #'number-to-string neighbor-values)) 2))

(defun day20/get-pixel (data coord inf)
  (advent/get image coord inf))

(defun day20/new-value (code image neighbors inf)
  (let ((key (day20/neighbors-to-key (--map (day20/get-pixel image it inf) neighbors))))
    (aref code key)))

(defun day20/debug--get-bounds (data)
  (--reduce-from (list (cons (min (car (car acc)) (car it))
                             (min (cdr (car acc)) (cdr it)))
                       (cons (max (car (cadr acc)) (car it))
                             (max (cdr (cadr acc)) (cdr it))))
                 (list (cons 0 0) (cons 0 0))
                 (advent/-map-hash (plist-get data :image) it-key)))

(defun day20/debug-print-image (data)
  (let* ((image (plist-get data :image))
         (bounds (day20/debug--get-bounds data))
         (inf (plist-get data :inf))
         (min (car bounds))
         (max (cadr bounds)))
    (with-current-buffer (get-buffer-create "*Day 20 output*")
      (erase-buffer)
      (loop for i from (car min) upto (car max) do
            (loop for j from (cdr min) upto (cdr max) do
                  (insert (day20/pixel-to-string (day20/get-pixel image (cons i j) inf))))
            (insert "\n"))
      (redisplay)))
  data)

(defun day20/evolve (data)
  "Evolve the data into a new image"
  (let ((code (plist-get data :code))
        (image (plist-get data :image))
        (inf (plist-get data :inf))
        (new-image (advent/table))
        (pixels-touched (advent/table)))
    
    ;; evolve the current pixels
    (advent/-each-hash image 
      (let* ((neighbors (day20/get-neighbors-coords it-key))
             (new-value (day20/new-value code image neighbors inf)))
        ;; every time I look up at any pixel, add it to the list of pixels worthy of consideration
        (--each neighbors (advent/put pixels-touched it :undefined))
        ;; Set the current pixel as "evaluated" in both the new image and the "interesting neighbors"
        (advent/put new-image it-key new-value)))
    
    ;; pixels from the image doesn't need to be evaluated twice
    (advent/-each-hash image
      (remhash it-key pixels-touched))
    
    ;; we still need to evolve the pixels that haven't been evolved in the periphery
    (advent/-each-hash pixels-touched
      (advent/put new-image it-key (day20/new-value code image (day20/get-neighbors-coords it-key) inf)))

    ;; return the new image
    (list :code code
          :image new-image
          :inf (aref code inf))))

(defun day20/count-lit-pixels (data)
  (apply #'+ (advent/-map-hash (plist-get data :image) it-value)))

(defun day20/debug--pad! (data padding)
  (let* ((bounds (day20/debug--get-bounds data))
         (min (car bounds))
         (max (cadr bounds))
         (image (plist-get data :image)))
    (loop for i from (- (car min) padding) upto (+ (car max) padding) do
          (loop for j from (- (cdr min) padding) upto (+ (cdr max) padding) do
                (let ((key (cons i j)))
                  (unless (advent/get image key)
                    (advent/put image key 0))))))
  data)

(defun day20/evolve-n (data evolutions &optional print-f)
  (let ((print-f (or print-f identity)))
   (--reduce-from (funcall print-f (day20/evolve acc)) (funcall print-f data)
                  (number-sequence 1 evolutions))))

(defun day20/count-evolved (data evolutions)
  (day20/count-lit-pixels (day20/evolve-n data evolutions)))

(defun day20/debug-evolve-n (data evolutions &optional padding sleep-int)
  (day20/evolve-n (day20/debug--pad! data (or padding 0)) evolutions
                  (lambda (x)
                    (day20/debug-print-image x)
                    (sleep-for (or sleep-int 0.5))
                    x)))

(defun day20/part-1 (lines)
  (day20/count-evolved (day20/read-data lines) 2))

(defun day20/part-2 (lines)
  (day20/count-evolved (day20/read-data lines) 50))

(provide 'day20)

;;;  5402 too high 5169 low 5232 anche sbagliato 5229
(setq example (day20/read-data (advent/read-blocks-of-lines 20 :example)))
(setq problem (day20/read-data (advent/read-blocks-of-lines 20 :problem)))
