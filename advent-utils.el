(require 'cl)
(require 'dash)

(defmacro comment (&rest x)
  "Important (and missed…) enough to warrant a global-like name"
  nil)

(defun advent/table ()
  (make-hash-table :test #'equal))

(defun advent/copy-table (table)
  (copy-hash-table table))

(defun advent/put (table key value)
  (puthash key value table)
  table)

(defun advent/cache (table key value)
  "Like advent/put, but returns the value instead of the table"
  (puthash key value table)
  value)

(defun advent/get (table key &optional default)
  (gethash key table default))

(defun advent/update (table key f &optional default &rest other)
  "Update the table using the result of f that accepts the key and the old value (or default).

\"other\" is appended to the list of arguments of f, if present

Returns the table

WARNING: nil values are not properly supported!"
  (let* ((old-value (advent/get table key))
         (new-value (apply f (cons key (cons (or old-value default) other)))))
    (advent/put table key new-value)
    table))

(defmacro advent/-update (table key form &optional default &rest other)
  "Anaphoric form of advent/update

It binds:
    it-key to the key
    it-value to the value
    other to the remaining arguments"
  `(advent/update ,table
                  ,key
                  (lambda (it-key it-value &rest other) ,form)
                  ,default
                  ,other))

(defun advent/map-hash (table function)
  "Like maphash, but accumulates the return like -map does"
  (let ((result))
    (maphash (lambda (k v)
               (setq result (cons (funcall function k v) result)))
             table)
    (nreverse result)))

(defmacro advent/-map-hash (table &rest forms)
  "Anaphoric version of advent/map-hash that binds key and value to it-key and it-value"
  (declare (indent 1))
  `(advent/map-hash ,table
                    (lambda (it-key it-value)
                      ,@forms)))

(defun advent/each-hash (table function)
  "Same as (maphash function table)"
  (maphash function table))

(defmacro advent/-each-hash (table &rest forms)
  "Anaphoric for for advent/each-hash that binds key and value to it-key and it-value"
  (declare (indent 1))
  `(advent/each-hash ,table (lambda (it-key it-value) ,@forms)))

(defun advent/create--grid-line (row columns)
  (--map (cons row it) (number-sequence 0 (1- columns))))

(defun advent/create-coordinates (rows columns)
  (-flatten (--map (advent/create--grid-line it columns)
                   (number-sequence 0 (1- rows)))))

(defun advent/iterate (f initial-value n)
  (let ((value initial-value))
   (while (> n 0)
     (setq value (funcall f value))
     (setq n (1- n)))
   value))

(defun advent/compute-input-name (day type)
  (format (cond
           ((eq type :example)
            "data/day%d-example.txt")
           ((eq type :problem)
            "data/day%d-problem.txt")
           (t (error "Unexpected problem type")))
          day))

(defun advent/read-problem-text (day type)
  (with-temp-buffer
    (insert-file-contents (advent/compute-input-name day type))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun advent/read-problem-lines (day type)
  (split-string (advent/read-problem-text day type) "\n" t))

(defun advent/read--grid-line (line conversion-f)
  (apply #'vector (-map conversion-f (split-string line "" t))))

(defun advent/lines-to-grid (lines &optional conversion-f)
  (apply #'vector
         (--map (advent/read--grid-line it (or conversion-f #'string-to-number))
                lines)))

(defun advent/read-grid (day type &optional conversion-f)
  (advent/lines-to-grid (advent/read-problem-lines day type)
                                        conversion-f))

(defun advent/make-grid (n-rows n-columns value)
  (let ((rows (make-vector n-rows nil)))
    (loop for row below n-rows do
          (aset rows row (make-vector n-columns value)))
    rows))

(defun advent/copy-grid (grid)
  "Returns a copy of the grid (cells are referenced)"
  (apply #'vector
         (--map (copy-sequence (aref grid it))
                (number-sequence 0 (1- (length grid))))))

(defun advent/debug-str-grid (grid)
  (let ((result ""))
    (loop for i below (length grid) do
          (let ((current-row (aref grid i)))
            (loop for j below (length current-row) do
                 (setq result (concat result (format " %s" (aref current-row j))))))
          (setq result (concat result "\n")))
    result))

(defun advent/update-grid-value! (grid coord f)
  (let ((i (car coord))
        (j (cdr coord)))
    (let* ((old-value (aref (aref grid i) j))
          (new-value (funcall f old-value)))
      (aset (aref grid i) j new-value)
      new-value)))

(defmacro advent/-update-grid-value! (grid coord &rest body)
  "Anaphoric form. Binds 'it' to value"
  (declare (indent 2))
  `(advent/update-grid-value! ,grid ,coord (lambda (it) ,@body)))

(defun advent/update-grid! (grid f)
  "Update in placethe value of a grid with f, which receives the current cell value as input"
  (loop for i below (length grid) do
        (loop for j below (length (aref grid 0)) do
              (advent/update-grid-value! grid (cons i j) f)))
  grid)

(defmacro advent/-update-grid! (grid &rest forms)
  "Anaphoric version of advent/update-grid!

The value is binded to 'it'"
  (declare (indent 1))
  `(advent/update-grid! ,grid (lambda (it) ,@forms)))

(defun advent/grid-set! (grid row-column value)
  (aset (aref grid (car row-column)) (cdr row-column) value))

(defun advent/grid-get (grid row-column)
  (aref (aref grid (car row-column)) (cdr row-column)))

(defun advent/read-problem-numbers-line (day type)
  (-map #'string-to-number
        (split-string (car (advent/read-problem-lines day type))
                      ","
                      t)))

(defun advent/read-problem-numbers (day type)
  (-map #'string-to-number (advent/read-problem-lines day type)))

(defun advent/read-problem-tokens (day type)
  (--map (split-string it " ") (advent/read-problem-lines day type)))

(defun advent/read-problem-instructions (day type)
  (--map (list (intern (concat ":" (car it)))
               (string-to-number (cadr it)))
         (advent/read-problem-tokens day type)))

(defun advent/read-raw-problem-lines (day type)
  (split-string (advent/read-problem-text day type) "\n"))

(defun advent/add-to-accumulator (acc line)
  (let ((groups (car acc))
        (current-group (cadr acc)))
    (if (/= (length line) 0)
        (list groups (cons line current-group))
      (list (cons (reverse current-group) groups) nil))))

(defun advent/group-lines (lines)
  (let ((accumulated (-reduce-from #'advent/add-to-accumulator
                                   '(() ())
                                   lines)))
    (if (cadr accumulated)
        (reverse (cons (reverse (cadr accumulated)) (car accumulated)))
      (reverse (car accumulated)))))

(defun advent/read-blocks-of-lines (day type)
  (advent/group-lines (advent/read-raw-problem-lines day type)))

(defun advent/bogus-gradient (start end f)
  "A getto gradient function that bisects through the domain"
  (if (< (- end start) 2)
      (let ((start-value (funcall f start))
            (end-value (funcall f end)))
        (if (> end-value start-value)
            (cons start start-value)
          (cons end end-value)))
    (let ((center (floor (/ (+ end start) 2))))
      (let ((center-val (funcall f center))
                (right-val (funcall f (1+ center))))
            (cond
             ((> right-val center-val)
              (advent/bogus-gradient start center f))
             ((< right-val center-val)
              (advent/bogus-gradient center end f))
             (t (error "This is weird…")))))))

(defmacro advent/loop-grid (grid &rest forms)
  "Non-hygienic macro that bind all coordinates of the grid to 'it'

it is bound to the current row and column"
  (declare (indent 1))
  (let ((rows (make-symbol "rows"))
        (columns (make-symbol "columns"))
        (i (make-symbol "i"))
        (j (make-symbol "j"))
        (it (make-symbol "it")))
   `(let ((,rows (length ,grid))
          (,columns (length (aref ,grid 0))))
      (loop for ,i from 0 below ,rows do
            (loop for ,j from 0 below ,columns do
                  (let ((it (cons ,i ,j)))
                    ,@forms))))))

(defmacro advent/time (&rest forms)
  "Time the forms and return a cons with the time in ms and the result"
  (declare (indent 0))
  (let ((start-time (make-symbol "start-time"))
        (result (make-symbol "result")))
    `(let ((,start-time (float-time))
           (,result))
       (setq ,result (progn ,@forms))
       (list (- (float-time) ,start-time)
             ,result))))

(provide 'advent-utils)
