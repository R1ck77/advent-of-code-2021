(require 'dash)
(require 's)
(require 'advent-utils)

;;; Room layout:
;;; h0 h1 . h2 . h3 . h4 . h5 h6  
;;;      a0   b0   c0   d0
;;;      a1   b1   c1   d1
;;;      (possibly a1 and then a2)

(defconst day23/hyper--score 1e20 "A score so high no combination can possibly reach it")

(defconst day23/s-potential-paths
  '(
    :a0 ((:h1) (:h1 :h0) (:h2) (:h2 :h3) (:h2 :h3 :h4) (:h2 :h3 :h4 :h5) (:h2 :h3 :h4 :h5 :h6)
         (:h2 :b0) (:h2 :b0 :b1)
         (:h2 :h3 :c0) (:h2 :h3 :c0 :c1)
         (:h2 :h3 :h4 :d0) (:h2 :h3 :h4 :d0 :d1))
    :a1 ((:a0 :h1) (:a0 :h1 :h0) (:a0 :h2) (:a0 :h2 :h3) (:a0 :h2 :h3 :h4) (:a0 :h2 :h3 :h4 :h5) (:a0 :h2 :h3 :h4 :h5 :h6)
         (:a0 :h2 :b0) (:a0 :h2 :b0 :b1)
         (:a0 :h2 :h3 :c0) (:a0 :h2 :h3 :c0 :c1)
         (:a0 :h2 :h3 :h4 :d0) (:a0 :h2 :h3 :h4 :d0 :d1))
    :b0 ((:h2) (:h2 :h1) (:h2 :h1 :h0) (:h3) (:h3 :h4) (:h3 :h4 :h5) (:h3 :h4 :h5 :h6)
         (:h2 :a0) (:h2 :a0 :a1)
         (:h3 :c0) (:h3 :c0 :c1)
         (:h3 :h4 :d0) (:h3 :h4 :d1))
    :b1 ((:b0 :h2) (:b0 :h2 :h1) (:b0 :h2 :h1 :h0) (:b0 :h3) (:b0 :h3 :h4) (:b0 :h3 :h4 :h5) (:b0 :h3 :h4 :h5 :h6)
         (:b0 :h2 :a0) (:b0 :h2 :a0 :a1)
         (:b0 :h3 :c0) (:b0 :h3 :c0 :c1)
         (:b0 :h3 :h4 :d0) (:b0 :h3 :h4 :d0 :d1))
    :c0 ((:h3) (:h3 :h2) (:h3 :h2 :h1) (:h3 :h2 :h1 :h0) (:h4) (:h4 :h5) (:h4 :h5 :h6)
         (:h3 :h2 :a0) (:h3 :h2 :a0 :a1)
         (:h3 :b0) (:h3 :b0 :b1)
         (:h4 :d0) (:h4 :d0 :d1))
    :c1 ((:c0 :h3) (:c0 :h3 :h2) (:c0 :h3 :h2 :h1) (:c0 :h3 :h2 :h1 :h0) (:c0 :h4) (:c0 :h4 :h5) (:c0 :h4 :h5 :h6)
         (:c0 :h3 :h2 :a0) (:c0 :h3 :h2 :a0 :a1)
         (:c0 :h3 :b0) (:c0 :h3 :b0 :b1)
         (:c0 :h4 :d0) (:c0 :h4 :d0 :d1))
    :d0 ((:h5) (:h5 :h6) (:h4) (:h4 :h3) (:h4 :h3 :h2) (:h4 :h3 :h2 :h1) (:h4 :h3 :h2 :h1 :h0)
         (:h4 :c0) (:h4 :c0 :c1)
         (:h4 :h3 :b0) (:h4 :h3 :b0 :b1)
         (:h4 :h3 :h2 :a0) (:h4 :h3 :h2 :a0 :a1))
    :d1 ((:d0 :h5) (:d0 :h5 :h6) (:d0 :h4) (:d0 :h4 :h3) (:d0 :h4 :h3 :h2) (:d0 :h4 :h3 :h2 :h1) (:d0 :h4 :h3 :h2 :h1 :h0)
         (:d0 :h4 :c0) (:d0 :h4 :c0 :c1)
         (:d0 :h4 :h3 :b0) (:d0 :h4 :h3 :b0 :b1)
         (:d0 :h4 :h3 :h2 :a0) (:d0 :h4 :h3 :h2 :a0 :a1))
    :h0 ((:h1 :a0) (:h1 :a0 :a1)
         (:h1 :h2 :b0) (:h1 :h2 :b0 :b1)
         (:h1 :h2 :h3 :c0) (:h1 :h2 :h3 :c0 :c1)
         (:h1 :h2 :h3 :h4 :d0) (:h1 :h2 :h3 :h4 :d0 :d1))
    :h1 ((:a0) (:a0 :a1)
         (:h2 :b0) (:h2 :b0 :b1)
         (:h2 :h3 :c0) (:h2 :h3 :c0 :c1)
         (:h2 :h3 :h4 :d0) (:h2 :h3 :h4 :d0 :d1))
    :h2 ((:a0) (:a0 :a1)
         (:b0) (:b0 :b1)
         (:h3 :c0) (:h3 :c0 :c1)
         (:h3 :h4 :d0) (:h3 :h4 :d0 :d1))
    :h3 ((:b0) (:b0 :b1)
         (:h2 :a0) (:h2 :a0 :a1)
         (:c0) (:c0 :c1)
         (:h4 :d0) (:h4 :d0 :d1))
    :h4 ((:d0) (:d0 :d1)
         (:c0) (:c0 :c1)
         (:h3 :b0) (:h3 :b0 :b1)
         (:h3 :h2 :a0) (:h3 :h2 :a0 :a1))
    :h5 ((:d0) (:d0 :d1)
         (:h4 :c0) (:h4 :c0 :c1)
         (:h4 :h3 :b0) (:h4 :h3 :b0 :b1)
         (:h4 :h3 :h2 :a0) (:h4 :h3 :h2 :a0 :a1))
    :h6 ((:h5 :d0) (:h5 :d0 :d1)
         (:h5 :h4 :c0) (:h5 :h4 :c0 :c1)
         (:h5 :h4 :h3 :b0) (:h5 :h4 :h3 :b0 :b1)
         (:h5 :h4 :h3 :h2 :a0) (:h5 :h4 :h3 :h2 :a0 :a1))))

;;; Preprocessing
(defun day23/convert-paths (x)
  (let ((table (advent/table)))
    (-each (-partition 2 x)
      (lambda (block)
        (let ((start (car block))
              (paths (cadr block)))
          (--each paths
            (progn
              (print (format "%s -> %s"(cons start (car (reverse it))) it))
              (advent/put table
                         (cons start (car (reverse it)))
                         it))))))
    table))

(defconst day23/s-from-to-paths (day23/convert-paths day23/s-potential-paths))

(defconst day23/raw--double-cost-moves
  '(
    (:h1 . :a0) (:a0 . :h1)
    (:h2 . :a0) (:a0 . :h2)
    (:h1 . :h2) (:h2 . :h1)
    (:h2 . :b0) (:b0 . :h2)
    (:h3 . :b0) (:b0 . :h3)
    (:h3 . :h2) (:h2 . :h3)
    (:h3 . :c0) (:c0 . :h3)
    (:h4 . :c0) (:c0 . :h4)
    (:h3 . :h4) (:h4 . :h3)
    (:h4 . :d0) (:d0 . :h4)
    (:h5 . :d0) (:d0 . :h5)
    (:h4 . :h5) (:h5 . :h4)))

;;; Preprocessing
(defun day23/add-to-set (list)
  (let ((table (advent/table)))
    (--each list (advent/put table it 2))
    table))

(defconst day23/double--cost-moves (day23/add-to-set day23/raw--double-cost-moves))

(defun day23/compute--moves-cost (from-to)
  (let ((all-costs (advent/table)))
    (maphash (lambda (src-dst other-cells) 
               (let* ((all-steps (--map (cons (car it) (cadr it))  (-partition-in-steps 2 1 (cons (car src-dst) other-cells))))
                      (move-cost (apply #'+ (--map (advent/get day23/double--cost-moves it 1) all-steps))))
                 (print (format "%s -> %s (%s)" src-dst move-cost all-steps))
                 (advent/put all-costs src-dst move-cost)))
             from-to)
    all-costs))

;; This is constant, so caching is sort of mandatory
(defconst day23/s-move-costs (day23/compute--moves-cost day23/s-from-to-paths))

(defvar day23/halls (list :h0 :h1 :h2 :h3 :h4 :h5 :h6))

(defvar day23/s-rooms (list :a0 :a1 :b0 :b1 :c0 :c1 :d0 :d1))
(defvar day23/s-locations (append day23/halls day23/s-rooms))

(defvar day23/l-rooms (list :a0 :a1 :a2 :a3 :b0 :b1 :b2 :b3 :c0 :c1 :c2 :c3 :d0 :d1 :d2 :d3))
(defvar day23/l-locations (append day23/halls day23/l-rooms))

(defun day23/letter-to-symbol (letter)
  (intern (concat ":" (downcase letter))))

(defun day23/symbol-to-letter (symbol)
  (if symbol
      (upcase (substring (symbol-name symbol) 1))
    "."))

(defun day23/read-agents (line)
  (-map #'day23/letter-to-symbol (split-string line "[ \#]" t)))

(defmacro day23/preprocess-template (template)
  (let ((new-template (s-replace "x" "%s" template)))
    new-template))

(defun day23/get--string (state agent)
  (day23/symbol-to-letter (plist-get state agent)))

(defun day23/s-to-string (state)
  (format (day23/preprocess-template "#############
#xx.x.x.x.xx#
###x#x#x#x###
  #x#x#x#x#
  #########")
          (day23/get--string state :h0) (day23/get--string state :h1)
          (day23/get--string state :h2) (day23/get--string state :h3)
          (day23/get--string state :h4) (day23/get--string state :h5)
          (day23/get--string state :h6)
          (day23/get--string state :a0) (day23/get--string state :b0) (day23/get--string state :c0) (day23/get--string state :d0)
          (day23/get--string state :a1) (day23/get--string state :b1) (day23/get--string state :c1) (day23/get--string state :d1)))

(defun day23/l-to-string (state)
  (format (day23/preprocess-template "#############
#xx.x.x.x.xx#
###x#x#x#x###
  #x#x#x#x#
  #x#x#x#x#
  #x#x#x#x#
  #########")
          (day23/get--string state :h0) (day23/get--string state :h1)
          (day23/get--string state :h2) (day23/get--string state :h3)
          (day23/get--string state :h4) (day23/get--string state :h5)
          (day23/get--string state :h6)
          (day23/get--string state :a0) (day23/get--string state :b0) (day23/get--string state :c0) (day23/get--string state :d0)
          (day23/get--string state :a1) (day23/get--string state :b1) (day23/get--string state :c1) (day23/get--string state :d1)
          (day23/get--string state :a2) (day23/get--string state :b2) (day23/get--string state :c2) (day23/get--string state :d2)
          (day23/get--string state :a3) (day23/get--string state :b3) (day23/get--string state :c3) (day23/get--string state :d3)))

(defun day23/to-string (state)
  (if (= (length state) 32)
      (day23/s-to-string state)
    (day23/l-to-string state)))

;; TODO/FIXME loop, perhaps?
(defmacro day23/make--hall-getter (i)
         (let ((hall-name (intern (format ":h%d" i)))
               (function-name (intern (format "day23/get-h%d" i))))
                 `(defun ,function-name (state)
                    (plist-get state ,hall-name))))
(day23/make--hall-getter 0)
(day23/make--hall-getter 1)
(day23/make--hall-getter 2)
(day23/make--hall-getter 3)
(day23/make--hall-getter 4)
(day23/make--hall-getter 5)
(day23/make--hall-getter 6)

(defmacro day23/make--room-getter (pos)
         (let ((letter-symbol (intern (format ":%s" pos)))
               (function-name (intern (format "day23/get-%s" pos))))
                 `(defun ,function-name (state)
                    (plist-get state ,letter-symbol))))
(day23/make--room-getter "a0")
(day23/make--room-getter "a1")
(day23/make--room-getter "a2")
(day23/make--room-getter "a3")
(day23/make--room-getter "b0")
(day23/make--room-getter "b1")
(day23/make--room-getter "b2")
(day23/make--room-getter "b3")
(day23/make--room-getter "c0")
(day23/make--room-getter "c1")
(day23/make--room-getter "c2")
(day23/make--room-getter "c3")
(day23/make--room-getter "d0")
(day23/make--room-getter "d1")
(day23/make--room-getter "d2")
(day23/make--room-getter "d3")

(defun day23/get-score (state)
  (plist-get state :score))


(defun day23/s-read-problem (lines)
  (let ((first-line (day23/read-agents (elt lines 2)))
        (second-line (day23/read-agents (elt lines 3))))
    (list :h0 nil :h1 nil :h2 nil :h3 nil :h4 nil :h5 nil :h6 nil
          :a0 (elt first-line 0)
          :b0 (elt first-line 1)
          :c0 (elt first-line 2)
          :d0 (elt first-line 3)
          :a1 (elt second-line 0)
          :b1 (elt second-line 1)
          :c1 (elt second-line 2)
          :d1 (elt second-line 3)
          :score 0)))

(defun day23/l-read-problem (lines)
  (let ((first-line (day23/read-agents (elt lines 2)))
        (second-line (day23/read-agents (elt lines 3)))
        (third-line (day23/read-agents (elt lines 4)))
        (fourth-line (day23/read-agents (elt lines 5))))
    (list :h0 nil :h1 nil :h2 nil :h3 nil :h4 nil :h5 nil :h6 nil
          :a0 (elt first-line 0) :b0 (elt first-line 1) :c0 (elt first-line 2) :d0 (elt first-line 3)
          :a1 (elt second-line 0) :b1 (elt second-line 1) :c1 (elt second-line 2) :d1 (elt second-line 3)
          :a2 (elt third-line 0) :b2 (elt third-line 1) :c2 (elt third-line 2) :d2 (elt third-line 3)
          :a3 (elt fourth-line 0) :b3 (elt fourth-line 1) :c3 (elt fourth-line 2) :d3 (elt fourth-line 3)
          :score 0)))

(defun day23/s-is-win? (state)
  (and (eq (day23/get-a0 state) :a)
       (eq (day23/get-a1 state) :a)
       (eq (day23/get-b0 state) :b)
       (eq (day23/get-b1 state) :b)
       (eq (day23/get-c0 state) :c)
       (eq (day23/get-c1 state) :c)
       (eq (day23/get-d0 state) :d)
       (eq (day23/get-d1 state) :d)
       (day23/get-score state)))

(defun day23/can-move-there? (state src dst)
  "Returns true if the path from src to dst is not blocked"
  (let ((path (advent/get day23/s-from-to-paths (cons src dst))))
    (not (-non-nil (--map (plist-get state it) path)))))

;; TODO/FIXME unused?
(defun day23/s-can-move? (state location)
  "returs the location if it's associated with an agent which has available moves"
  (if (memq location day23/halls)
      (day23/s-can-move-from-hall? state location)
    (day23/s-can-move-from-room? state location)))

(defun day23/s-get-layout-for-room (letter)
  (case letter
    (:a '(:a0 :a1))
    (:b '(:b0 :b1))
    (:c '(:c0 :c1))
    (:d '(:d0 :d1))
    (t (error "Invalid letter"))))

(defun day23/s-room-occupants (state letter)
  "Returns a list of cons corresponding to the occupied rooms, from top to down"
  (-filter #'cdr
          (--map (cons it (plist-get state it))
                 (day23/s-get-layout-for-room letter))))

(defun day23/s-first-empty-for-room (state room)
  (--first (not (plist-get state it))
   (case room
     (:a '(:a1 :a0))
     (:b '(:b1 :b0))
     (:c '(:c1 :c0))
     (:d '(:d1 :d0))
     (t (error "Unexpected room"))))) 

(defun day23/s-get-room-state (state letter)
  "Returns the state of the room corresopnding to the letter:

:full                 if the room is occupied by the owners
:space                if the room has space for a owner
(<place> , <letter>)  the first guest that should leave"
  (let ((guests (day23/s-room-occupants state letter)))
    (cond
     ((not guests) :space) ;completely empty
     ((equal (-uniq (-map #'cdr guests)) (list letter)) ;all guests are of the correct letter
      (if (= (length guests) 2)
          :full ;and they fill the room
        :space))
     (t (car guests) (car guests)))))

(defun day23/get-rooms-state (state)
  (list :a (day23/s-get-room-state state :a)
        :b (day23/s-get-room-state state :b)
        :c (day23/s-get-room-state state :c)
        :d (day23/s-get-room-state state :d)))

(defun day23/get-room-moves (state room-states room)
  "Return a list of all possible moves"
  (let ((this-state (plist-get room-states room)))
    (unless (or (eq this-state :space) ; cannot move stuff *from* here
                (eq this-state :full)) ; no space
      (let ((from (car this-state))
            (letter (cdr this-state)))
        ;; make sure the moves are valid
        (--filter (day23/can-move-there? state (car it) (cdr it))
                  ;; room to corridor moves
                  (--map (cons from it) day23/halls))))))

(defun day23/get-hall-agents (state)
  (-filter #'cdr (--map (cons it (plist-get state it)) day23/halls)))

(defun day23/get-hall-moves (state room-states)
  ;; List of agents that *could* go in a room, theoretically
  (let ((pos-agents-with-destination (--filter (eq (plist-get room-states (cdr it)) :space)
                                           (day23/get-hall-agents state))))
    (--filter (day23/can-move-there? state (car it) (cdr it))
              (--map (let* ((src (car it))
                            (agent (cdr it))
                            (destination (day23/s-first-empty-for-room state agent)))
                       (assert destination)
                       (cons src destination))
                     pos-agents-with-destination))))

(defun day23/s-compute-cost (move letter)
  (* (advent/get day23/s-move-costs move)
     (case letter
       (:a 1)
       (:b 10)
       (:c 100)
       (:d 1000)
       (t (error "Unexpected letter!")))))

(defun day23/s-next (state)
  "Return all possible moves of the current state, or nil if none exists

The move is in the form ((src . destination) letter cost)"
  (let ((room-states (day23/get-rooms-state state)))
    (let ((room-moves (apply #'append
                             (--map (day23/get-room-moves state room-states it)
                                    '(:a :b :c :d))))
          (hall-moves (day23/get-hall-moves state room-states)))
      (--map (let ((letter (plist-get state (car it))))
               (list it letter (day23/s-compute-cost it letter)))
             (append room-moves hall-moves)))))

(defun day23/update (state move)
  (let ((move (elt move 0))
        (letter (elt move 1))
        (cost (elt move 2))
        (state (copy-sequence state))
        (old-score (plist-get state :score)))
    (plist-put (plist-put (plist-put state
                           (car move)
                           nil)
                (cdr move)
                letter)
               :score
               (+ old-score cost))))

(defvar day23/stepping nil)

(defvar day23/debug-print-enabled nil)

(defun day23/debug-print (value)
  (when day23/debug-print-enabled
    (print value)
    (redisplay))
  nil)

(defun day23/projected-min-d-cost (state)
  "Returns the minimum cost required to move both d in place"
  (let* ((d-locations (-map #'car (--filter (eq (cadr it) :d) (-partition 2 state))))
         (off-d-locations(--filter (not (or (eq it :d0) (eq it :d1))) d-locations)))
    (cond
     ;; both d *could* be in place
     ((not off-d-locations) 0)
     ;; both d are surely out of place
     ((= (length off-d-locations) 2)
      (+ (day23/s-compute-cost (cons (car off-d-locations) :d0) :d)
         (day23/s-compute-cost (cons (cadr off-d-locations) :d1) :d)))
     ((= (length off-d-locations) 1)
      ;; one d is out of place
      (let ((d0 (plist-get state :d0))
            (d1 (plist-get state :d1)))
        (if (eq d0 :d)
            ;; d1 is out of place
            (day23/s-compute-cost (cons (car off-d-locations) :d1) :d)           
          ;; d0 is out of place
          (day23/s-compute-cost (cons (car off-d-locations) :d0) :d))))
     (t (error "Unexpected condition")))))

(defun day23/projected-min-c-cost (state)
  "Returns the minimum cost required to move both c in place"
  (let* ((c-locations (-map #'car (--filter (eq (cadr it) :c) (-partition 2 state))))
         (off-c-locations(--filter (not (or (eq it :c0) (eq it :c1))) c-locations)))
    (cond
     ;; both c *could* be in place
     ((not off-c-locations) 0)
     ;; both c are surely out of place
     ((= (length off-c-locations) 2)
      (+ (day23/s-compute-cost (cons (car off-c-locations) :c0) :c)
         (day23/s-compute-cost (cons (cadr off-c-locations) :c1) :c)))
     ((= (length off-c-locations) 1)
      ;; one c is out of place
      (let ((c0 (plist-get state :c0))
            (c1 (plist-get state :c1)))
        (if (eq c0 :c)
            ;; c1 is out of place
            (day23/s-compute-cost (cons (car off-c-locations) :c1) :c)           
          ;; c0 is out of place
          (day23/s-compute-cost (cons (car off-c-locations) :c0) :c))))
     (t (error "Unexpected condition")))))

(defun day23/projected-min-b-cost (state)
  "Returns the minimum cost required to move both b in place"
  (let* ((b-locations (-map #'car (--filter (eq (cadr it) :b) (-partition 2 state))))
         (off-b-locations(--filter (not (or (eq it :b0) (eq it :b1))) b-locations)))
    (cond
     ;; both b *could* be in place
     ((not off-b-locations) 0)
     ;; both b are surely out of place
     ((= (length off-b-locations) 2)
      (+ (day23/s-compute-cost (cons (car off-b-locations) :b0) :b)
         (day23/s-compute-cost (cons (cadr off-b-locations) :b1) :b)))
     ((= (length off-b-locations) 1)
      ;; one b is out of place
      (let ((b0 (plist-get state :b0))
            (b1 (plist-get state :b1)))
        (if (eq b0 :b)
            ;; b1 is out of place
            (day23/s-compute-cost (cons (car off-b-locations) :b1) :b)           
          ;; b0 is out of place
          (day23/s-compute-cost (cons (car off-b-locations) :b0) :b))))
     (t (error "Unexpected condition")))))

(defun day23/projected-min-a-cost (state)
  "Returns the minimum cost required to move both a in place"
  (let* ((a-locations (-map #'car (--filter (eq (cadr it) :a) (-partition 2 state))))
         (off-a-locations(--filter (not (or (eq it :a0) (eq it :a1))) a-locations)))
    (cond
     ;; both a *could* be in place
     ((not off-a-locations) 0)
     ;; both a are surely out of place
     ((= (length off-a-locations) 2)
      (+ (day23/s-compute-cost (cons (car off-a-locations) :a0) :a)
         (day23/s-compute-cost (cons (cadr off-a-locations) :a1) :a)))
     ((= (length off-a-locations) 1)
      ;; one a is out of place
      (let ((a0 (plist-get state :a0))
            (a1 (plist-get state :a1)))
        (if (eq a0 :a)
            ;; a1 is out of place
            (day23/s-compute-cost (cons (car off-a-locations) :a1) :a)           
          ;; a0 is out of place
          (day23/s-compute-cost (cons (car off-a-locations) :a0) :a))))
     (t (error "Unexpected condition")))))

(defun day23/projected-min-cost (state)
  (+ (day23/projected-min-a-cost state)
     (day23/projected-min-b-cost state)
     (day23/projected-min-c-cost state)
     (day23/projected-min-d-cost state)))

(defun day23/evolve (state minimum-score)
  "Returns the minimum score for a win"
  (let ((current-score (plist-get state :score)))
    ;(day23/debug-print (format "%s\nState:\n%s\n(score: %d)\n" state (day23/to-string state) current-score))
    (when day23/stepping
      (read-string "Continue?"))
    (if (day23/s-is-win? state)
        (progn
          (print (format "New win! %d" current-score))
          (redisplay)
          current-score)
      (if (>= (+ current-score (day23/projected-min-cost state)) minimum-score)
          (day23/debug-print (format "USELESS! %d" current-score))        
          (let ((next-moves (day23/s-next state)))
            (if next-moves
                ;; how many are still below the previous minimum?
                (let ((useful-moves (--filter (< (+ current-score
                                                    (elt it 2))
                                                 minimum-score)
                                              next-moves )))
                  (if useful-moves    ; otherwise is 'nil', that is, a dead end
                      (--reduce-from (let ((new-score-or-nil (day23/evolve (day23/update state it) acc)))
                                       (or (and new-score-or-nil (min new-score-or-nil acc)) acc))
                                     minimum-score
                                     useful-moves)
                    (day23/debug-print (format "OVERFLOW (%d + move > %d)" current-score minimum-score))))
              (day23/debug-print "DEAD END!")))))))

(defun day23/debug--print-futures (text)
  (let ((state (if (stringp text)
                   (day23/s-read-problem (split-string text))
                 text)))
    (print (format "CURRENT (score: %d):\n%s\n%s"
                   (plist-get state :score)
                   state
                   (day23/to-string state)))
    (let ((next-states (--map (day23/update state it) (day23/s-next state))))
      (print (format "FUTURES:\n"))
      (--each next-states (print (format "*** SCORE: %d\n%s\n%s"
                                         (plist-get it :score)
                                         it
                                         (day23/to-string it))))))
  nil)


(defun day23/part-1 (lines)
  (day23/evolve (day23/s-read-problem lines) day23/hyper--score))

(defun day23/part-2 (lines)
  (day23/l-read-problem lines)
  (error "Not yet implemented"))

(provide 'day23)

(setq es (day23/s-read-problem (advent/read-problem-lines 23 :example 1)))
(setq ps (day23/s-read-problem (advent/read-problem-lines 23 :problem 1)))
(setq el (day23/l-read-problem (advent/read-problem-lines 23 :example 2)))
(setq pl (day23/l-read-problem (advent/read-problem-lines 23 :problem 2)))

(setq max-lisp-eval-depth 100000)
(setq max-specpdl-size 100000)
