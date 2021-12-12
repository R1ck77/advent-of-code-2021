(require 'dash)
(require 's)
(setq load-path (cons "." load-path))

(defun last-test ()
  (car
   (sort (--filter (s-suffix? "-test.el" it)
                   (directory-files "."))
         #'string>)))

(defun run-last-test ()
  (with-temp-buffer    
    (insert-file (last-test))
    (eval-buffer)
    (buttercup-run)))
