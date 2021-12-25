(require 'dash)
(require 'advent-utils)

(defconst day25/debug-buffer "* Day 25 *")

(defun day25/symbol-to-string (symbol)
  (or (case symbol
     (:> ">")
     (:v "v"))
      "."))

(defun day25/to-string (grid)
  (let ((result ""))
    (advent/-each-grid grid
      (when (and (zerop (cdr it-coord))
               (not (equal it-coord '(0 . 0))))
        (setq result (concat result "\n")))
      (setq result (concat result (day25/symbol-to-string it-value))))
    result))

(defun day25/debug--print (grid)
  (with-current-buffer (get-buffer-create day25/debug-buffer)
    (erase-buffer)
    (goto-char (point-min)) ;overkill probably
    (insert (day25/to-string grid))))

(defun day25/read-map (grid)
  (advent/-update-grid! grid
    (cond
     ((string= it ">") :>)
     ((string= it "v") :v)
     ((string= it ".") nil)
     ((t (error "Unexpected value!"))))))


(defun day25/evolve-right (grid)
  (loop for line from 0 below (length grid) do
        (day25/evolve-line! grid line)))

(defun day25/convert-coordinates (grid-size coords)
  (cons (mod (car coords) (car grid-size))
        (mod (cdr coords) (cdr grid-size))))

(defun day25/get-wrap (grid grid-size coord )
  (advent/grid-get grid (day25/convert-coordinates grid-size coord)))

(defun day25/set-wrap (grid grid-size coord value)
  (advent/grid-set! grid (day25/convert-coordinates grid-size coord) value))

(defun day25/get-block (grid grid-size pos block-size)
  ;;; TODO/FIXME cache the block
  (let ((block (advent/make-grid (car block-size) (cdr block-size) nil)))
    (loop for i from 0 below (car block-size) do
          (loop for j from 0 below (cdr block-size) do
                (let ((value (day25/get-wrap grid
                                             grid-size
                                             (cons (+ (car pos) i) (+ (cdr pos) j)))))
                  (advent/grid-set! block (cons i j) value))))
    block))

(defun day25/set-block! (grid grid-size pos block)
  (let ((block-size (advent/get-grid-size block)))
    (loop for i from (1- (car block-size)) downto 0  do
          (loop for j from (1- (cdr block-size)) downto 0 do
                (let ((block-value (advent/grid-get block (cons i j))))
                  (advent/grid-set! grid
                                    (day25/convert-coordinates grid-size
                                                               (cons (+ i (car pos))
                                           (+ j (cdr pos))))
                                    block-value))))))

(defun day25/transform (grid f psize)
  (let* ((grid-size (advent/get-grid-size grid))
        (new-grid (advent/make-grid (car grid-size) (cdr grid-size) nil)))
    (advent/each-grid grid
                      (lambda (coord value)
                        (let ((block (day25/get-block grid grid-size coord psize)))
                          (day25/set-block! new-grid grid-size coord (funcall f block)))))
    new-grid))

(defun day25/horizontal-f (sample)
    (if (equal sample [[:> nil]])
      (vector (vector nil :>))
    sample)
)

(defun day25/vertical-f (sample)
    (if (equal sample [[:v] [nil]])
      (vector (vector nil) (vector :v))
    sample)
  sample
)

(defun day25/evolve (grid)
  (let ((transformed (day25/transform grid #'day25/horizontal-f '(1 . 2))))
    (day25/debug--print transformed)
    (day25/transform transformed #'day25/vertical-f '(2 . 1))))

(defun day25/read-column (grid column rows)
  (let ((new-column (make-vector rows nil)))
    (loop for i from 0 below rows do
          (aset new-column i (aref (aref grid i) column)))
    new-column))

(defun day25/write-column! (grid column vector rows)
  (loop for i from 0 below rows do
        (aset (aref grid i) column (aref vector i))))

(defun day25/evolve-column (vector rows)
  ;; TODO/FIXME algorithm
  )

(defun day25/evolve-row! (vector columns)
  ;; TODO/FIXME algorithm
  )

(defun day25/evolve (grid)
    (let* ((grid-size (advent/get-grid-size grid))
           (new-grid (advent/copy-grid grid)))
      (loop for row from 0 below (car grid-size) do
            (day25/evolve-row! (aref new-grid row) (cdr grid-size)))
      (loop for column from 0 below (cdr grid-size) do
            (let ((column-vector (day25/read-column new-grid column (car grid-size))))
              (day25/evolve-column column-vector (car grid-size))
              (day25/write-column! new-grid column column-vector (car grid-size))))
      new-grid))

(defun day25/part-1 (lines)
  (error "Not yet implemented"))

(defun day25/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day25)

(setq example (day25/read-map (advent/read-grid 25 :example #'identity)))
(setq problem (day25/read-map (advent/read-grid 25 :problem #'identity)))
