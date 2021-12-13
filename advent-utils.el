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
  (puthash key value table))

(defun advent/get (table key &optional default)
  (gethash key table default))

(defun advent/update (table key f &optional default &rest other)
  "Update the table using the result of f that accepts the key and the old value (or default).

\"other\" is appended to the list of arguments of f, if present

Returns the new value.

WARNING: nil values are not properly supported!"
  (let* ((old-value (advent/get table key))
         (new-value (apply f (cons key (cons (or old-value default) other)))))
    (advent/put table key new-value)
    new-value))

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

;;; TODO/FIXME create an anaphoric version and use it in day6
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

(provide 'advent-utils)
