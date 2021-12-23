(require 'dash)
(require 's)
(require 'advent-utils)

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
  (if (= (length state) 30)
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

(defun day23/s-can-move-from-hall? (state location)
  )

(defun day23/s-can-move-from-room? (state location)
  )

(defun day23/s-can-move? (state location)
  "returs the location if it's associated with an agent which has available moves"
  (if (memq location day23/halls)
      (day23/s-can-move-from-hall? state location)
    (day23/s-can-move-from-room? state location)))

(defun day23/s-next (state)
  "Return all possible outcomes of the current state, or nil if none exists"
  (--filter (day23/s-can-move? state day23/s-locations) state))


(defun day23/part-1 (lines)
  (day23/s-read-problem lines)
  (error "Not yet implemented"))

(defun day23/part-2 (lines)
  (day23/l-read-problem lines)
  (error "Not yet implemented"))

(provide 'day23)

(setq es (day23/s-read-problem (advent/read-problem-lines 23 :example 1)))
(setq ps (day23/s-read-problem (advent/read-problem-lines 23 :problem 1)))
(setq el (day23/l-read-problem (advent/read-problem-lines 23 :example 2)))
(setq pl (day23/l-read-problem (advent/read-problem-lines 23 :problem 2)))
