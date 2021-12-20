(require 'dash)
(require 'advent-utils)

(defconst day20/displacements '((-1 . -1) (-1 . 0) (-1 . 1)
                                (0 . -1) (0 . 0) (0 . 1)
                                (1 . -1) (1 . 0) (1 . 1)))
(defconst day20/padding 2)
(defconst day20/dropped 1)

(defconst day20/debug-buffer-name "*Day 20 output*")

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

(defun day20/get-image-size (image)
  (cons (length image) (length (aref image 0))))

(defun day20/read-data (blocks)
  (let ((image (advent/lines-to-grid (cadr blocks) #'day20/string-to-pixel)))
    (list :code (day20/read-code (caar blocks))
          :image image
          :size (day20/get-image-size image)
          :infinite 0)))

(defun day20/get-neighbors-coords (coord)
  (--map (cons (+ (car it) (car coord))
               (+ (cdr it) (cdr coord)))
         day20/displacements))

(defun day20/neighbors-to-key (neighbor-values)
  (string-to-number (apply #'concat (-map #'number-to-string neighbor-values)) 2))

(defun day20/get-pixel (image coord default)
  "Get a pixel or return 'default' if the coordinate is out of range"
  (condition-case nil
      (advent/grid-get image coord)
    (error default)))

(defun day20/new-value (code image neighbors default)
  (let ((key (day20/neighbors-to-key (--map (day20/get-pixel image it default) neighbors))))
    (aref code key)))

(defun day20/debug-print-image (data)
  (let* ((image (plist-get data :image))
         (size (plist-get data :size)))
    (with-current-buffer (get-buffer-create day20/debug-buffer-name)
      (erase-buffer)
      (loop for i from 0 below (car size) do
            (loop for j from 0 below (cdr size) do
                  (insert (day20/pixel-to-string (day20/get-pixel image (cons i j) 42))))
            (insert "\n"))
      (goto-char (point-min))
      (redisplay)))
  data)

(defun day20/reduce-grid (grid dropped)
  (let ((new-grid (advent/make-grid (- (length grid) (* 2 dropped))
                                    (- (length (aref grid 0)) (* 2 dropped))
                                    :invalid)))
    (advent/loop-grid new-grid
      (advent/grid-set! new-grid it                        
                        (advent/grid-get grid (cons (+ (car it) dropped)
                                                    (+ (cdr it) dropped)))))
    new-grid))

(defun day20/pad-grid (grid padding default)
  (let ((new-grid (advent/make-grid (+ (length grid) (* 2 padding))
                                    (+ (length (aref grid 0)) (* 2 padding))
                                    default)))
    (advent/loop-grid grid
      (advent/grid-set! new-grid
                        (cons (+ (car it) padding)
                              (+ (cdr it) padding))
                        (advent/grid-get grid it)))
    new-grid))

(defun day20/evolve (data)
  "Evolve the data into a new image"
  (let* ((code (plist-get data :code))
         (image (plist-get data :image))
         (size (plist-get data :size))
         (infinite (plist-get data :infinite))
         (padded-size (cons (+ (car size) (* day20/padding 2))
                            (+ (cdr size) (* day20/padding 2)))))
    
    ;; evolve a grid padded with day20/padding each side
    (let* ((next-infinite (if (zerop infinite) (aref code 0) (aref code 511)))
           (new-image (day20/pad-grid image day20/padding next-infinite)))
      (advent/loop-grid new-image
        (let* ((neighbors (day20/get-neighbors-coords (cons (- (car it) day20/padding)
                                                            (- (cdr it) day20/padding))))
               (new-value (day20/new-value code image neighbors infinite)))
          ;; Set the current pixel as "evaluated" in both the new image and the "interesting neighbors"
          (condition-case nil ;; wrap
              (advent/grid-set! new-image it
                                new-value)
            (error nil))))

      ;; drop the cells from each border
      (let ((reduced-image (day20/reduce-grid new-image day20/dropped)))
        (list :code code
              :image reduced-image
              :size (day20/get-image-size reduced-image)
              :infinite next-infinite)))))

(defun day20/count-lit-pixels (data)
  (let ((image (plist-get data :image))
        (counter 0))
    (advent/loop-grid image
      (setq counter (+ counter (advent/grid-get image it))))
    counter))

(defun day20/evolve-n (data evolutions &optional print-f)
  (let ((print-f (or print-f 'identity)))
    (--reduce-from (funcall print-f (day20/evolve acc)) (funcall print-f data)
                   (number-sequence 1 evolutions))))

(defun day20/count-evolved (data evolutions)
  (day20/count-lit-pixels (day20/evolve-n data evolutions)))

(defun day20/part-1 (lines)
  (day20/count-evolved (day20/read-data lines) 2))

(defun day20/part-2 (lines)
  (day20/count-evolved (day20/read-data lines) 50))

(defun day20/debug-evolve-n (data evolutions &optional sleep-int)
  (switch-to-buffer (get-buffer-create day20/debug-buffer-name))
  (day20/count-lit-pixels
   (day20/evolve-n data evolutions
                   (lambda (x)
                     (day20/debug-print-image x)
                     (sleep-for (or sleep-int 0.5))
                     x))))

(provide 'day20)
