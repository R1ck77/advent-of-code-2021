(require 'dash)
(require 'advent-utils)

(defconst day21/dlimit 1000 "threshold for the deterministic dice")
(defconst day21/qlimit 21 "threshold for the dirac dice")

(defun day21/read-start-pos (line)
  (string-to-number (elt (split-string line) 4)))


(defun day21/read-input (lines)
  (cons (day21/read-start-pos (car lines))
        (day21/read-start-pos (cadr lines))))

(defun day21/create-det-dice ()
  (-partition 3 (-flatten (-repeat 3 (number-sequence 1 100)))))

(defun day21/create-det-state (players)
  (list :dice (day21/create-det-dice)
        :rolls 0
        :player-1 (list :score 0
                        :pos (car players))
        :player-2 (list :score 0
                        :pos (cdr players))))

(defun day21/read-det-state (lines)
  (day21/create-det-state (day21/read-input lines)))


(defun day21/new-det-dice (dice)
  (cons (apply #'+ (car dice))
        (let ((new-state (cdr dice)))
          (or new-state (day21/create-det-dice)))))

(defun day21/next-pos (pos steps)
  "Return the next position after a roll"
  (1+ (mod (+ (1- pos) steps) 10)))

(defun day21/player-step (player dice)
  "Returns the new player and the new dice"
  (let ((score (plist-get player :score))
        (pos (plist-get player :pos))
        (score-dice (day21/new-det-dice dice)))
    (setq pos (day21/next-pos pos (car score-dice)))
    (cons (list :pos pos
                :score (+ score pos))
          (cdr score-dice))))

(defun day21/game-step (state)
  "Evolve the state, if the first player wins, the step is interrupted midway"
  (let ((player-1 (plist-get state :player-1))
        (player-2 (plist-get state :player-2))
        (rolls (plist-get state :rolls))
        (dice (plist-get state :dice)))
    (let ((player-dice (day21/player-step player-1 dice)))
      (setq player-1 (car player-dice))
      (setq dice (cdr player-dice))
      (setq rolls (+ rolls 3))
      (unless (>= (plist-get player-1 :score) day21/dlimit)
        (let ((player-dice (day21/player-step player-2 dice)))
          (setq player-2 (car player-dice))
          (setq dice (cdr player-dice))
          (setq rolls (+ rolls 3)))))
    (list :dice dice
          :rolls rolls
          :player-1 player-1
          :player-2 player-2)))

(defun day21/is-win? (state)
  (let ((score-1 (plist-get (plist-get state :player-1) :score))
        (score-2 (plist-get (plist-get state :player-2) :score))
        (rolls (plist-get state :rolls)))
    (cond
     ((>= score-1 day21/dlimit) (* rolls score-2))
     ((>= score-2 day21/dlimit) (* rolls score-1))
     (t nil))))

(defun day21/deterministic-game (state)
  (let ((win))
    (while (not win)
      (setq state (day21/game-step state))
      (setq win (day21/is-win? state)))
    win))

(defun day21/part-1 (lines)
  (day21/deterministic-game (day21/read-det-state lines)))

;; dices results(times) are 3 (1), 4(3), 5(6), 6(7), 7(6), 8(3), 9(1)
(defconst day21/qmoves [nil
                        nil
                        nil
                        [nil  4  5  6  7  8  9 10  1  2  3]
                        [nil  5  6  7  8  9 10  1  2  3  4]
                        [nil  6  7  8  9 10  1  2  3  4  5]
                        [nil  7  8  9 10  1  2  3  4  5  6]
                        [nil  8  9 10  1  2  3  4  5  6  7]
                        [nil  9 10  1  2  3  4  5  6  7  8]
                        [nil 10  1  2  3  4  5  6  7  8  9]]
  "List of moves. Given the position n and the dice rol d the next position is qmoves[d][n]")

(defmacro day21/qmove (pos roll)
  (declare (indent 2))
  `(aref (aref day21/qmoves ,roll) ,pos))

(defun day21/qstep (pos score roll)
  (let ((next-pos (day21/qmove pos roll)))
    (cons next-pos (+ score next-pos))))

(defun day21/player-qstep (pos score)
  "Return the outcomes of the player game"
  (list (cons 1 (day21/qstep pos score 3))
        (cons 3 (day21/qstep pos score 4))
        (cons 6 (day21/qstep pos score 5))
        (cons 7 (day21/qstep pos score 6))
        (cons 6 (day21/qstep pos score 7))
        (cons 3 (day21/qstep pos score 8))
        (cons 1 (day21/qstep pos score 9))))


(defun day21/qevolve (qstate)
  "Evolve the state, if the first player wins, the step is interrupted midway"
  (let ((p1-pos (aref qstate 0))
        (p1-score (aref qstate 1))
        (p2-pos (aref qstate 2))
        (p2-score (aref qstate 3)))
    (let ((players-1 (day21/player-qstep p1-pos p1-score))
          (players-2 (day21/player-qstep p2-pos p2-score)))
      (let ((all-states))
       (loop for p1 in players-1 do
             (loop for p2 in players-2 do
                   (push (cons (cons (car p1) (car p2))
                               (vector (car (cdr p1)) (cdr (cdr p1)) (car (cdr p2)) (cdr (cdr p2))))
                         all-states)))
       all-states))))

(defun day21/qvictory? (qstate)
  (if (>= (aref qstate 1) day21/qlimit)
      (vector 1 0)
    (when (>= (aref qstate 3) day21/qlimit)
        (vector 0 1))))

(defun day21/sum-wins (va vb)
  (vector (+ (aref va 0)
             (aref vb 0))
          (+ (aref va 1)
             (aref vb 1))))

(defun day21/mult-wins (m1-m2 v)
  (let ((f (* (car m1-m2) (cdr m1-m2))))
   (vector (* f (aref v 0))
           (* f (aref v 1)))))

(defun day21/multiplicity (m1-m2 v)
  (let ((v1 (aref v 0))
        (v2 (aref v 1)))
    (assert (or (zerop v1) (zerop v2)))
    (vector (* (car m1-m2) v1)
            (* (car m1-m2) (cdr m1-m2) v2))))

(defun day21/count-victories! (cache qstate m1-m2)
  "Returns a cons of victories for the current qstate. Updates the cache"
  (if-let ((cached (advent/get cache qstate)))
      (day21/mult-wins m1-m2 cached)
    (if-let ((dead-end (day21/qvictory? qstate)))
        (day21/multiplicity m1-m2 dead-end)
      (let* ((new-m-states (day21/qevolve qstate))
             (reduced (--reduce-from (let ((m1-m2 (car it))
                                           (s1-s2 (cdr it)))
                                       (day21/sum-wins acc (day21/count-victories! cache s1-s2 m1-m2)))
                                     (vector 0 0)
                                     new-m-states)))
        (advent/put cache qstate reduced)
        (day21/mult-wins m1-m2 reduced)))))


(defun day21/create-qstate (players)
  (vector (car players) 0 (cdr players) 0))


(defun day21/part-2 (lines)
  (let ((result (day21/count-victories! (advent/table)
                                        (day21/create-qstate (day21/read-input lines))
                                        (cons 1 1))))
    (max (aref result 0)
         (aref result 1))))

(provide 'day21)

(setq example (day21/read-input (advent/read-problem-lines 21 :example)))
(setq problem (day21/read-input (advent/read-problem-lines 21 :problem)))
