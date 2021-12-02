(require 'dash)
(require 'advent-utils)

(defun day2/update-coordinates (acc it)
  (let ((x-pos (car acc))
        (y-pos (cadr acc)))
   (let ((opcode (car it))
         (value (cadr it)))
     (case opcode
       (:forward (list (+ value x-pos) y-pos))
       (:up (list x-pos (- y-pos value)))
       (:down (list x-pos (+ y-pos value)))))))

(defun day2/combine-coordinates (list)
  (apply #'* (-take 2 list)))

(defun day2/part-1 (input)
  (day2/combine-coordinates
   (-reduce-from #'day2/update-coordinates '(0 0) input)))

(defun day2/update-coordinates-with-aim (acc it)
  (let ((x-pos (elt acc 0))
        (y-pos (elt acc 1))
        (aim (elt acc 2)))
   (let ((opcode (car it))
         (value (cadr it)))
     (case opcode
       (:forward (list (+ value x-pos) (+ y-pos (* value aim)) aim))
       (:up (list x-pos y-pos (- aim value)))
       (:down (list x-pos y-pos (+ aim value)))))))

(defun day2/part-2 (input)
  (day2/combine-coordinates
   (-reduce-from #'day2/update-coordinates-with-aim '(0 0 0) input)))

(provide 'day2)
