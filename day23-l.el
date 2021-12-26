(require 'dash)
(require 's)
(require 'advent-utils)

;;; Room layout:
;;; h0 h1 . h2 . h3 . h4 . h5 h6  
;;;      a0   b0   c0   d0
;;;      a1   b1   c1   d1
;;;      a2   b2   c2   d2
;;;      a3   b3   c3   d3


(defconst day23-l/hyper--score 1e20 "A score so high no combination can possibly reach it")

(defconst day23-l/l-potential-paths
  '(
    :a0 ((:h1) (:h1 :h0) (:h2) (:h2 :h3) (:h2 :h3 :h4) (:h2 :h3 :h4 :h5) (:h2 :h3 :h4 :h5 :h6)
         (:h2 :b0) (:h2 :b0 :b1) (:h2 :b0 :b1 :b2) (:h2 :b0 :b1 :b2 :b3)
         (:h2 :h3 :c0) (:h2 :h3 :c0 :c1) (:h2 :h3 :c0 :c1 :c2) (:h2 :h3 :c0 :c1 :c2 :c3)
         (:h2 :h3 :h4 :d0) (:h2 :h3 :h4 :d0 :d1) (:h2 :h3 :h4 :d0 :d1 :d2) (:h2 :h3 :h4 :d0 :d1 :d2 :d3))
    :a1 ((:a0 :h1) (:a0 :h1 :h0) (:a0 :h2) (:a0 :h2 :h3) (:a0 :h2 :h3 :h4) (:a0 :h2 :h3 :h4 :h5) (:a0 :h2 :h3 :h4 :h5 :h6)
         (:a0 :h2 :b0) (:a0 :h2 :b0 :b1) (:a0 :h2 :b0 :b1 :b2) (:a0 :h2 :b0 :b1 :b2 :b3)
         (:a0 :h2 :h3 :c0) (:a0 :h2 :h3 :c0 :c1) (:a0 :h2 :h3 :c0 :c1 :c2) (:a0 :h2 :h3 :c0 :c1 :c2 :c3)
         (:a0 :h2 :h3 :h4 :d0) (:a0 :h2 :h3 :h4 :d0 :d1) (:a0 :h2 :h3 :h4 :d0 :d1 :d2) (:a0 :h2 :h3 :h4 :d0 :d1 :d2 :d3))
    :a2 ((:a1 :a0 :h1) (:a1 :a0 :h1 :h0) (:a1 :a0 :h2) (:a1 :a0 :h2 :h3) (:a1 :a0 :h2 :h3 :h4) (:a1 :a0 :h2 :h3 :h4 :h5) (:a1 :a0 :h2 :h3 :h4 :h5 :h6)
         (:a1 :a0 :h2 :b0) (:a1 :a0 :h2 :b0 :b1) (:a1 :a0 :h2 :b0 :b1 :b2) (:a1 :a0 :h2 :b0 :b1 :b2 :b3)
         (:a1 :a0 :h2 :h3 :c0) (:a1 :a0 :h2 :h3 :c0 :c1) (:a1 :a0 :h2 :h3 :c0 :c1 :c2) (:a1 :a0 :h2 :h3 :c0 :c1 :c2 :c3)
         (:a1 :a0 :h2 :h3 :h4 :d0) (:a1 :a0 :h2 :h3 :h4 :d0 :d1) (:a1 :a0 :h2 :h3 :h4 :d0 :d1 :d2) (:a1 :a0 :h2 :h3 :h4 :d0 :d1 :d2 :d3))
    :a3 ((:a2 :a1 :a0 :h1) (:a2 :a1 :a0 :h1 :h0) (:a2 :a1 :a0 :h2) (:a2 :a1 :a0 :h2 :h3) (:a2 :a1 :a0 :h2 :h3 :h4) (:a2 :a1 :a0 :h2 :h3 :h4 :h5) (:a2 :a1 :a0 :h2 :h3 :h4 :h5 :h6)
         (:a2 :a1 :a0 :h2 :b0) (:a2 :a1 :a0 :h2 :b0 :b1) (:a2 :a1 :a0 :h2 :b0 :b1 :b2) (:a2 :a1 :a0 :h2 :b0 :b1 :b2 :b3)
         (:a2 :a1 :a0 :h2 :h3 :c0) (:a2 :a1 :a0 :h2 :h3 :c0 :c1) (:a2 :a1 :a0 :h2 :h3 :c0 :c1 :c2) (:a2 :a1 :a0 :h2 :h3 :c0 :c1 :c2 :c3)
         (:a2 :a1 :a0 :h2 :h3 :h4 :d0) (:a2 :a1 :a0 :h2 :h3 :h4 :d0 :d1) (:a2 :a1 :a0 :h2 :h3 :h4 :d0 :d1 :d2) (:a2 :a1 :a0 :h2 :h3 :h4 :d0 :d1 :d2 :d3))
    :b0 ((:h2) (:h2 :h1) (:h2 :h1 :h0) (:h3) (:h3 :h4) (:h3 :h4 :h5) (:h3 :h4 :h5 :h6)
         (:h2 :a0) (:h2 :a0 :a1) (:h2 :a0 :a1 :a2) (:h2 :a0 :a1 :a2 :a3)
         (:h3 :c0) (:h3 :c0 :c1) (:h3 :c0 :c1 :c2) (:h3 :c0 :c1 :c2 :c3)
         (:h3 :h4 :d0) (:h3 :h4 :d1) (:h3 :h4 :d1 :d2) (:h3 :h4 :d1 :d2 :d3))
    :b1 ((:b0 :h2) (:b0 :h2 :h1) (:b0 :h2 :h1 :h0) (:b0 :h3) (:b0 :h3 :h4) (:b0 :h3 :h4 :h5) (:b0 :h3 :h4 :h5 :h6)
         (:b0 :h2 :a0) (:b0 :h2 :a0 :a1) (:b0 :h2 :a0 :a1 :a2) (:b0 :h2 :a0 :a1 :a2 :a3)
         (:b0 :h3 :c0) (:b0 :h3 :c0 :c1) (:b0 :h3 :c0 :c1 :c2) (:b0 :h3 :c0 :c1 :c2 :c3)
         (:b0 :h3 :h4 :d0) (:b0 :h3 :h4 :d0 :d1) (:b0 :h3 :h4 :d0 :d1 :d2) (:b0 :h3 :h4 :d0 :d1 :d2 :d3))
    :b2 ((:b1 :b0 :h2) (:b1 :b0 :h2 :h1) (:b1 :b0 :h2 :h1 :h0) (:b1 :b0 :h3) (:b1 :b0 :h3 :h4) (:b1 :b0 :h3 :h4 :h5) (:b1 :b0 :h3 :h4 :h5 :h6)
         (:b1 :b0 :h2 :a0) (:b1 :b0 :h2 :a0 :a1) (:b1 :b0 :h2 :a0 :a1 :a2) (:b1 :b0 :h2 :a0 :a1 :a2 :a3)
         (:b1 :b0 :h3 :c0) (:b1 :b0 :h3 :c0 :c1) (:b1 :b0 :h3 :c0 :c1 :c2) (:b1 :b0 :h3 :c0 :c1 :c2 :c3)
         (:b1 :b0 :h3 :h4 :d0) (:b1 :b0 :h3 :h4 :d0 :d1) (:b1 :b0 :h3 :h4 :d0 :d1 :d2) (:b1 :b0 :h3 :h4 :d0 :d1 :d2 :d3))
    :b3 ((:b2 :b1 :b0 :h2) (:b2 :b1 :b0 :h2 :h1) (:b2 :b1 :b0 :h2 :h1 :h0) (:b2 :b1 :b0 :h3) (:b2 :b1 :b0 :h3 :h4) (:b2 :b1 :b0 :h3 :h4 :h5) (:b2 :b1 :b0 :h3 :h4 :h5 :h6)
         (:b2 :b1 :b0 :h2 :a0) (:b2 :b1 :b0 :h2 :a0 :a1) (:b2 :b1 :b0 :h2 :a0 :a1 :a2) (:b2 :b1 :b0 :h2 :a0 :a1 :a2 :a3)
         (:b2 :b1 :b0 :h3 :c0) (:b2 :b1 :b0 :h3 :c0 :c1) (:b2 :b1 :b0 :h3 :c0 :c1 :c2) (:b2 :b1 :b0 :h3 :c0 :c1 :c2 :c3)
         (:b2 :b1 :b0 :h3 :h4 :d0) (:b2 :b1 :b0 :h3 :h4 :d0 :d1) (:b2 :b1 :b0 :h3 :h4 :d0 :d1 :d2) (:b2 :b1 :b0 :h3 :h4 :d0 :d1 :d2 :d3))        
    :c0 ((:h3) (:h3 :h2) (:h3 :h2 :h1) (:h3 :h2 :h1 :h0) (:h4) (:h4 :h5) (:h4 :h5 :h6)
         (:h3 :h2 :a0) (:h3 :h2 :a0 :a1) (:h3 :h2 :a0 :a1 :a2) (:h3 :h2 :a0 :a1 :a2 :a3)
         (:h3 :b0) (:h3 :b0 :b1) (:h3 :b0 :b1 :b2) (:h3 :b0 :b1 :b2 :b3)
         (:h4 :d0) (:h4 :d0 :d1) (:h4 :d0 :d1 :d2) (:h4 :d0 :d1 :d2 :d3))
    :c1 ((:c0 :h3) (:c0 :h3 :h2) (:c0 :h3 :h2 :h1) (:c0 :h3 :h2 :h1 :h0) (:c0 :h4) (:c0 :h4 :h5) (:c0 :h4 :h5 :h6)
         (:c0 :h3 :h2 :a0) (:c0 :h3 :h2 :a0 :a1) (:c0 :h3 :h2 :a0 :a1 :a2) (:c0 :h3 :h2 :a0 :a1 :a2 :a3)
         (:c0 :h3 :b0) (:c0 :h3 :b0 :b1) (:c0 :h3 :b0 :b1 :b2) (:c0 :h3 :b0 :b1 :b2 :b3)
         (:c0 :h4 :d0) (:c0 :h4 :d0 :d1) (:c0 :h4 :d0 :d1 :d2) (:c0 :h4 :d0 :d1 :d2 :d3))
    :c2 ((:c1 :c0 :h3) (:c1 :c0 :h3 :h2) (:c1 :c0 :h3 :h2 :h1) (:c1 :c0 :h3 :h2 :h1 :h0) (:c1 :c0 :h4) (:c1 :c0 :h4 :h5) (:c1 :c0 :h4 :h5 :h6)
         (:c1 :c0 :h3 :h2 :a0) (:c1 :c0 :h3 :h2 :a0 :a1) (:c1 :c0 :h3 :h2 :a0 :a1 :a2) (:c1 :c0 :h3 :h2 :a0 :a1 :a2 :a3)
         (:c1 :c0 :h3 :b0) (:c1 :c0 :h3 :b0 :b1) (:c1 :c0 :h3 :b0 :b1 :b2) (:c1 :c0 :h3 :b0 :b1 :b2 :b3)
         (:c1 :c0 :h4 :d0) (:c1 :c0 :h4 :d0 :d1) (:c1 :c0 :h4 :d0 :d1 :d2) (:c1 :c0 :h4 :d0 :d1 :d2 :d3))
    :c3 ((:c2 :c1 :c0 :h3) (:c2 :c1 :c0 :h3 :h2) (:c2 :c1 :c0 :h3 :h2 :h1) (:c2 :c1 :c0 :h3 :h2 :h1 :h0) (:c2 :c1 :c0 :h4) (:c2 :c1 :c0 :h4 :h5) (:c2 :c1 :c0 :h4 :h5 :h6)
         (:c2 :c1 :c0 :h3 :h2 :a0) (:c2 :c1 :c0 :h3 :h2 :a0 :a1) (:c2 :c1 :c0 :h3 :h2 :a0 :a1 :a2) (:c2 :c1 :c0 :h3 :h2 :a0 :a1 :a2 :a3)
         (:c2 :c1 :c0 :h3 :b0) (:c2 :c1 :c0 :h3 :b0 :b1) (:c2 :c1 :c0 :h3 :b0 :b1 :b2) (:c2 :c1 :c0 :h3 :b0 :b1 :b2 :b3)
         (:c2 :c1 :c0 :h4 :d0) (:c2 :c1 :c0 :h4 :d0 :d1) (:c2 :c1 :c0 :h4 :d0 :d1 :d2) (:c2 :c1 :c0 :h4 :d0 :d1 :d2 :d3))    
    :d0 ((:h5) (:h5 :h6) (:h4) (:h4 :h3) (:h4 :h3 :h2) (:h4 :h3 :h2 :h1) (:h4 :h3 :h2 :h1 :h0)
         (:h4 :c0) (:h4 :c0 :c1) (:h4 :c0 :c1 :c2) (:h4 :c0 :c1 :c2 :c3)
         (:h4 :h3 :b0) (:h4 :h3 :b0 :b1) (:h4 :h3 :b0 :b1 :b2) (:h4 :h3 :b0 :b1 :b2 :b3)
         (:h4 :h3 :h2 :a0) (:h4 :h3 :h2 :a0 :a1) (:h4 :h3 :h2 :a0 :a1 :a2) (:h4 :h3 :h2 :a0 :a1 :a2 :a3))
    :d1 ((:d0 :h5) (:d0 :h5 :h6) (:d0 :h4) (:d0 :h4 :h3) (:d0 :h4 :h3 :h2) (:d0 :h4 :h3 :h2 :h1) (:d0 :h4 :h3 :h2 :h1 :h0)
         (:d0 :h4 :c0) (:d0 :h4 :c0 :c1) (:d0 :h4 :c0 :c1 :c2) (:d0 :h4 :c0 :c1 :c2 :c3)
         (:d0 :h4 :h3 :b0) (:d0 :h4 :h3 :b0 :b1) (:d0 :h4 :h3 :b0 :b1 :b2) (:d0 :h4 :h3 :b0 :b1 :b2 :b3)
         (:d0 :h4 :h3 :h2 :a0) (:d0 :h4 :h3 :h2 :a0 :a1) (:d0 :h4 :h3 :h2 :a0 :a1 :a2) (:d0 :h4 :h3 :h2 :a0 :a1 :a2 :a3))
    :d2 ((:d1 :d0 :h5) (:d1 :d0 :h5 :h6) (:d1 :d0 :h4) (:d1 :d0 :h4 :h3) (:d1 :d0 :h4 :h3 :h2) (:d1 :d0 :h4 :h3 :h2 :h1) (:d1 :d0 :h4 :h3 :h2 :h1 :h0)
         (:d1 :d0 :h4 :c0) (:d1 :d0 :h4 :c0 :c1) (:d1 :d0 :h4 :c0 :c1 :c2) (:d1 :d0 :h4 :c0 :c1 :c2 :c3)
         (:d1 :d0 :h4 :h3 :b0) (:d1 :d0 :h4 :h3 :b0 :b1) (:d1 :d0 :h4 :h3 :b0 :b1 :b2) (:d1 :d0 :h4 :h3 :b0 :b1 :b2 :b3)
         (:d1 :d0 :h4 :h3 :h2 :a0) (:d1 :d0 :h4 :h3 :h2 :a0 :a1) (:d1 :d0 :h4 :h3 :h2 :a0 :a1 :a2) (:d1 :d0 :h4 :h3 :h2 :a0 :a1 :a2 :a3))
    :d3 ((:d2 :d1 :d0 :h5) (:d2 :d1 :d0 :h5 :h6) (:d2 :d1 :d0 :h4) (:d2 :d1 :d0 :h4 :h3) (:d2 :d1 :d0 :h4 :h3 :h2) (:d2 :d1 :d0 :h4 :h3 :h2 :h1) (:d2 :d1 :d0 :h4 :h3 :h2 :h1 :h0)
         (:d2 :d1 :d0 :h4 :c0) (:d2 :d1 :d0 :h4 :c0 :c1) (:d2 :d1 :d0 :h4 :c0 :c1 :c2) (:d2 :d1 :d0 :h4 :c0 :c1 :c2 :c3)
         (:d2 :d1 :d0 :h4 :h3 :b0) (:d2 :d1 :d0 :h4 :h3 :b0 :b1) (:d2 :d1 :d0 :h4 :h3 :b0 :b1 :b2) (:d2 :d1 :d0 :h4 :h3 :b0 :b1 :b2 :b3)
         (:d2 :d1 :d0 :h4 :h3 :h2 :a0) (:d2 :d1 :d0 :h4 :h3 :h2 :a0 :a1) (:d2 :d1 :d0 :h4 :h3 :h2 :a0 :a1 :a2) (:d2 :d1 :d0 :h4 :h3 :h2 :a0 :a1 :a2 :a3))
    :h0 ((:h1 :a0) (:h1 :a0 :a1) (:h1 :a0 :a1 :a2) (:h1 :a0 :a1 :a2 :a3)
         (:h1 :h2 :b0) (:h1 :h2 :b0 :b1) (:h1 :h2 :b0 :b1 :b2) (:h1 :h2 :b0 :b1 :b2 :b3)
         (:h1 :h2 :h3 :c0) (:h1 :h2 :h3 :c0 :c1) (:h1 :h2 :h3 :c0 :c1 :c2) (:h1 :h2 :h3 :c0 :c1 :c2 :c3)
         (:h1 :h2 :h3 :h4 :d0) (:h1 :h2 :h3 :h4 :d0 :d1) (:h1 :h2 :h3 :h4 :d0 :d1 :d2) (:h1 :h2 :h3 :h4 :d0 :d1 :d2 :d3))
    :h1 ((:a0) (:a0 :a1) (:a0 :a1 :a2) (:a0 :a1 :a2 :a3)
         (:h2 :b0) (:h2 :b0 :b1) (:h2 :b0 :b1 :b2) (:h2 :b0 :b1 :b2 :b3)
         (:h2 :h3 :c0) (:h2 :h3 :c0 :c1) (:h2 :h3 :c0 :c1 :c2) (:h2 :h3 :c0 :c1 :c2 :c3)
         (:h2 :h3 :h4 :d0) (:h2 :h3 :h4 :d0 :d1) (:h2 :h3 :h4 :d0 :d1 :d2) (:h2 :h3 :h4 :d0 :d1 :d2 :d3))
    :h2 ((:a0) (:a0 :a1) (:a0 :a1 :a2) (:a0 :a1 :a2 :a3)
         (:b0) (:b0 :b1) (:b0 :b1 :b2) (:b0 :b1 :b2 :b3)
         (:h3 :c0) (:h3 :c0 :c1) (:h3 :c0 :c1 :c2) (:h3 :c0 :c1 :c2 :c3)
         (:h3 :h4 :d0) (:h3 :h4 :d0 :d1) (:h3 :h4 :d0 :d1 :d2) (:h3 :h4 :d0 :d1 :d2 :d3))
    :h3 ((:b0) (:b0 :b1) (:b0 :b1 :b2) (:b0 :b1 :b2 :b3)
         (:h2 :a0) (:h2 :a0 :a1) (:h2 :a0 :a1 :a2) (:h2 :a0 :a1 :a2 :a3)
         (:c0) (:c0 :c1) (:c0 :c1 :c2) (:c0 :c1 :c2 :c3)
         (:h4 :d0) (:h4 :d0 :d1) (:h4 :d0 :d1 :d2) (:h4 :d0 :d1 :d2 :d3))
    :h4 ((:d0) (:d0 :d1) (:d0 :d1 :d2) (:d0 :d1 :d2 :d3)
         (:c0) (:c0 :c1) (:c0 :c1 :c2) (:c0 :c1 :c2 :c3)
         (:h3 :b0) (:h3 :b0 :b1) (:h3 :b0 :b1 :b2) (:h3 :b0 :b1 :b2 :b3)
         (:h3 :h2 :a0) (:h3 :h2 :a0 :a1) (:h3 :h2 :a0 :a1 :a2) (:h3 :h2 :a0 :a1 :a2 :a3))
    :h5 ((:d0) (:d0 :d1) (:d0 :d1 :d2) (:d0 :d1 :d2 :d3)
         (:h4 :c0) (:h4 :c0 :c1) (:h4 :c0 :c1 :c2) (:h4 :c0 :c1 :c2 :c3)
         (:h4 :h3 :b0) (:h4 :h3 :b0 :b1) (:h4 :h3 :b0 :b1 :b2) (:h4 :h3 :b0 :b1 :b2 :b3)
         (:h4 :h3 :h2 :a0) (:h4 :h3 :h2 :a0 :a1) (:h4 :h3 :h2 :a0 :a1 :a2) (:h4 :h3 :h2 :a0 :a1 :a2 :a3))
    :h6 ((:h5 :d0) (:h5 :d0 :d1) (:h5 :d0 :d1 :d2) (:h5 :d0 :d1 :d2 :d3)
         (:h5 :h4 :c0) (:h5 :h4 :c0 :c1) (:h5 :h4 :c0 :c1 :c2) (:h5 :h4 :c0 :c1 :c2 :c3)
         (:h5 :h4 :h3 :b0) (:h5 :h4 :h3 :b0 :b1) (:h5 :h4 :h3 :b0 :b1 :b2) (:h5 :h4 :h3 :b0 :b1 :b2 :b3)
         (:h5 :h4 :h3 :h2 :a0) (:h5 :h4 :h3 :h2 :a0 :a1) (:h5 :h4 :h3 :h2 :a0 :a1 :a2) (:h5 :h4 :h3 :h2 :a0 :a1 :a2 :a3))))

;;; Preprocessing
(defun day23-l/convert-paths (x)
  (let ((table (advent/table)))
    (-each (-partition 2 x)
      (lambda (block)
        (let ((start (car block))
              (paths (cadr block)))
          (--each paths
            (progn
              ;(print (format "%s -> %s"(cons start (car (reverse it))) it))
              (advent/put table
                         (cons start (car (reverse it)))
                         it))))))
    table))

(defconst day23-l/l-from-to-paths (day23-l/convert-paths day23-l/l-potential-paths))

(defconst day23-l/raw--double-cost-moves
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
(defun day23-l/add-to-set (list)
  (let ((table (advent/table)))
    (--each list (advent/put table it 2))
    table))

(defconst day23-l/double--cost-moves (day23-l/add-to-set day23-l/raw--double-cost-moves))

(defun day23-l/compute--moves-cost (from-to)
  (let ((all-costs (advent/table)))
    (maphash (lambda (src-dst other-cells) 
               (let* ((all-steps (--map (cons (car it) (cadr it))  (-partition-in-steps 2 1 (cons (car src-dst) other-cells))))
                      (move-cost (apply #'+ (--map (advent/get day23-l/double--cost-moves it 1) all-steps))))
                 ;(print (format "%s -> %s (%s)" src-dst move-cost all-steps))
                 (advent/put all-costs src-dst move-cost)))
             from-to)
    all-costs))

;; This is constant, so caching is sort of mandatory
(defconst day23-l/l-move-costs (day23-l/compute--moves-cost day23-l/l-from-to-paths))

(defvar day23-l/halls (list :h0 :h1 :h2 :h3 :h4 :h5 :h6))

(defvar day23-l/l-rooms (list :a0 :a1 :a2 :a3 :b0 :b1 :b2 :b3 :c0 :c1 :c2 :c3 :d0 :d1 :d2 :d3))
(defvar day23-l/l-locations (append day23-l/halls day23-l/l-rooms))

(defun day23-l/letter-to-symbol (letter)
  (intern (concat ":" (downcase letter))))

(defun day23-l/symbol-to-letter (symbol)
  (if symbol
      (upcase (substring (symbol-name symbol) 1))
    "."))

(defun day23-l/read-agents (line)
  (-map #'day23-l/letter-to-symbol (split-string line "[ \#]" t)))

(defmacro day23-l/preprocess-template (template)
  (let ((new-template (s-replace "x" "%s" template)))
    new-template))

(defun day23-l/get--string (state agent)
  (day23-l/symbol-to-letter (plist-get state agent)))

(defun day23-l/to-string (state)
  (format (day23-l/preprocess-template "#############
#xx.x.x.x.xx#
###x#x#x#x###
  #x#x#x#x#
  #x#x#x#x#
  #x#x#x#x#
  #########")
          (day23-l/get--string state :h0) (day23-l/get--string state :h1)
          (day23-l/get--string state :h2) (day23-l/get--string state :h3)
          (day23-l/get--string state :h4) (day23-l/get--string state :h5)
          (day23-l/get--string state :h6)
          (day23-l/get--string state :a0) (day23-l/get--string state :b0) (day23-l/get--string state :c0) (day23-l/get--string state :d0)
          (day23-l/get--string state :a1) (day23-l/get--string state :b1) (day23-l/get--string state :c1) (day23-l/get--string state :d1)
          (day23-l/get--string state :a2) (day23-l/get--string state :b2) (day23-l/get--string state :c2) (day23-l/get--string state :d2)
          (day23-l/get--string state :a3) (day23-l/get--string state :b3) (day23-l/get--string state :c3) (day23-l/get--string state :d3)))

(defmacro day23-l/make--room-getter (pos)
         (let ((letter-symbol (intern (format ":%s" pos)))
               (function-name (intern (format "day23-l/get-%s" pos))))
                 `(defun ,function-name (state)
                    (plist-get state ,letter-symbol))))
(day23-l/make--room-getter "a0")
(day23-l/make--room-getter "a1")
(day23-l/make--room-getter "a2")
(day23-l/make--room-getter "a3")
(day23-l/make--room-getter "b0")
(day23-l/make--room-getter "b1")
(day23-l/make--room-getter "b2")
(day23-l/make--room-getter "b3")
(day23-l/make--room-getter "c0")
(day23-l/make--room-getter "c1")
(day23-l/make--room-getter "c2")
(day23-l/make--room-getter "c3")
(day23-l/make--room-getter "d0")
(day23-l/make--room-getter "d1")
(day23-l/make--room-getter "d2")
(day23-l/make--room-getter "d3")

(defun day23-l/get-score (state)
  (plist-get state :score))

(defun day23-l/l-read-problem (lines)
  (let ((first-line (day23-l/read-agents (elt lines 2)))
        (second-line (day23-l/read-agents (elt lines 3)))
        (third-line (day23-l/read-agents (elt lines 4)))
        (fourth-line (day23-l/read-agents (elt lines 5))))
    (list :h0 nil :h1 nil :h2 nil :h3 nil :h4 nil :h5 nil :h6 nil
          :a0 (elt first-line 0) :b0 (elt first-line 1) :c0 (elt first-line 2) :d0 (elt first-line 3)
          :a1 (elt second-line 0) :b1 (elt second-line 1) :c1 (elt second-line 2) :d1 (elt second-line 3)
          :a2 (elt third-line 0) :b2 (elt third-line 1) :c2 (elt third-line 2) :d2 (elt third-line 3)
          :a3 (elt fourth-line 0) :b3 (elt fourth-line 1) :c3 (elt fourth-line 2) :d3 (elt fourth-line 3)
          :score 0)))

(defun day23-l/l-is-win? (state)
  (and (eq (day23-l/get-a0 state) :a)
       (eq (day23-l/get-a1 state) :a)
       (eq (day23-l/get-a2 state) :a)
       (eq (day23-l/get-a3 state) :a)
       (eq (day23-l/get-b0 state) :b)
       (eq (day23-l/get-b1 state) :b)
       (eq (day23-l/get-b2 state) :b)
       (eq (day23-l/get-b3 state) :b)
       (eq (day23-l/get-c0 state) :c)
       (eq (day23-l/get-c1 state) :c)
       (eq (day23-l/get-c2 state) :c)
       (eq (day23-l/get-c3 state) :c)
       (eq (day23-l/get-d0 state) :d)
       (eq (day23-l/get-d1 state) :d)
       (eq (day23-l/get-d2 state) :d)
       (eq (day23-l/get-d3 state) :d)
       (day23-l/get-score state)))

(defun day23-l/can-move-there? (state src dst)
  "Returns true if the path from src to dst is not blocked"
  (let ((path (advent/get day23-l/l-from-to-paths (cons src dst))))
    (not (-non-nil (--map (plist-get state it) path)))))

(defun day23-l/l-get-layout-for-room (letter)
  (case letter
    (:a '(:a0 :a1 :a2 :a3))
    (:b '(:b0 :b1 :b2 :b3))
    (:c '(:c0 :c1 :c2 :c3))
    (:d '(:d0 :d1 :d2 :d3))
    (t (error "Invalid letter"))))

(defun day23-l/l-room-occupants (state letter)
  "Returns a list of cons corresponding to the occupied rooms, from top to down"
  (-filter #'cdr
          (--map (cons it (plist-get state it))
                 (day23-l/l-get-layout-for-room letter))))

(defun day23-l/l-first-empty-for-room (state room)
  (--first (not (plist-get state it))
   (case room
     (:a '(:a3 :a2 :a1 :a0))
     (:b '(:b3 :b2 :b1 :b0))
     (:c '(:c3 :c2 :c1 :c0))
     (:d '(:d3 :d2 :d1 :d0))
     (t (error "Unexpected room"))))) 

(defun day23-l/l-get-room-state (state letter)
  "Returns the state of the room corresopnding to the letter:

:full                 if the room is occupied by the owners
:space                if the room has space for a owner
(<place> , <letter>)  the first guest that should leave"
  (let ((guests (day23-l/l-room-occupants state letter)))
    (cond
     ((not guests) :space) ;completely empty
     ((equal (-uniq (-map #'cdr guests)) (list letter)) ;all guests are of the correct letter
      (if (= (length guests) 4)
          :full ;and they fill the room
        :space))
     (t (car guests)))))

(defun day23-l/get-rooms-state (state)
  (list :a (day23-l/l-get-room-state state :a)
        :b (day23-l/l-get-room-state state :b)
        :c (day23-l/l-get-room-state state :c)
        :d (day23-l/l-get-room-state state :d)))

(defun day23-l/get-room-moves (state room-states room)
  "Return a list of all possible moves"
  (let ((this-state (plist-get room-states room)))
    (unless (or (eq this-state :space) ; cannot move stuff *from* here
                (eq this-state :full)) ; no space
      (let ((from (car this-state))
            (letter (cdr this-state)))
        ;; make sure the moves are valid
        (--filter (day23-l/can-move-there? state (car it) (cdr it))
                  ;; room to corridor moves
                  (--map (cons from it) day23-l/halls))))))

(defun day23-l/get-hall-agents (state)
  (-filter #'cdr (--map (cons it (plist-get state it)) day23-l/halls)))

(defun day23-l/get-hall-moves (state room-states)
  ;; List of agents that *could* go in a room, theoretically
  (let ((pos-agents-with-destination (--filter (eq (plist-get room-states (cdr it)) :space)
                                           (day23-l/get-hall-agents state))))
    (--filter (day23-l/can-move-there? state (car it) (cdr it))
              (--map (let* ((src (car it))
                            (agent (cdr it))
                            (destination (day23-l/l-first-empty-for-room state agent)))
                       (assert destination)
                       (cons src destination))
                     pos-agents-with-destination))))

(defun day23-l/l-compute-cost (move letter)
  (* (advent/get day23-l/l-move-costs move)
     (case letter
       (:a 1)
       (:b 10)
       (:c 100)
       (:d 1000)
       (t (error "Unexpected letter!")))))

(defun day23-l/l-next (state)
  "Return all possible moves of the current state, or nil if none exists

The move is in the form ((src . destination) letter cost)"
  (let ((room-states (day23-l/get-rooms-state state)))
    (let ((room-moves (apply #'append
                             (--map (day23-l/get-room-moves state room-states it)
                                    '(:a :b :c :d))))
          (hall-moves (day23-l/get-hall-moves state room-states)))
      (--map (let ((letter (plist-get state (car it))))
               (list it letter (day23-l/l-compute-cost it letter)))
             (append room-moves hall-moves)))))

(defun day23-l/update (state move)
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

(defvar day23-l/stepping nil)

(defvar day23-l/debug-print-enabled t)

(defun day23-l/debug-print (value)
  (when day23-l/debug-print-enabled
    (print value)
    (redisplay))
  nil)

(defun day23-l/projected-min-d-cost (state)
  "Returns the minimum cost required to move both d in place"
  (let* ((d-locations (-map #'car (--filter (eq (cadr it) :d) (-partition 2 state))))
         ;; all location that contain a d and are not in d0 d1 d2 d3
         (off-d-locations(--filter (not (or (eq it :d0)
                                            (eq it :d1)
                                            (eq it :d2)
                                            (eq it :d3)))
                                   d-locations))
         ;; all d-room locations not occupied by a 'd'
         (empty-places (--filter (not (eq (plist-get state it) :d))'(:d0 :d1 :d2 :d3))))
    (assert (= (length empty-places) (length off-d-locations)))
    (apply #'+ (--map (day23-l/l-compute-cost it :d)  (-zip empty-places off-d-locations)))))

(defun day23-l/projected-min-b-cost (state)
  "Returns the minimum cost required to move both b in place"
  (let* ((b-locations (-map #'car (--filter (eq (cadr it) :b) (-partition 2 state))))
         ;; all location that contain a b and are not in b0 b1 b2 b3
         (off-b-locations(--filter (not (or (eq it :b0)
                                            (eq it :b1)
                                            (eq it :b2)
                                            (eq it :b3)))
                                   b-locations))
         ;; all b-room locations not occupied by a 'b'
         (empty-places (--filter (not (eq (plist-get state it) :b))'(:b0 :b1 :b2 :b3))))
    (assert (= (length empty-places) (length off-b-locations)))
    (apply #'+ (--map (day23-l/l-compute-cost it :b)  (-zip empty-places off-b-locations)))))

(defun day23-l/projected-min-c-cost (state)
  "Returns the minimum cost required to move both c in place"
  (let* ((c-locations (-map #'car (--filter (eq (cadr it) :c) (-partition 2 state))))
         ;; all location that contain a c and are not in c0 c1 c2 c3
         (off-c-locations(--filter (not (or (eq it :c0)
                                            (eq it :c1)
                                            (eq it :c2)
                                            (eq it :c3)))
                                   c-locations))
         ;; all c-room locations not occupied by a 'c'
         (empty-places (--filter (not (eq (plist-get state it) :c))'(:c0 :c1 :c2 :c3))))
    (assert (= (length empty-places) (length off-c-locations)))
    (apply #'+ (--map (day23-l/l-compute-cost it :c)  (-zip empty-places off-c-locations)))))

(defun day23-l/projected-min-a-cost (state)
  "Returns the minimum cost required to move both a in place"
  (let* ((a-locations (-map #'car (--filter (eq (cadr it) :a) (-partition 2 state))))
         ;; all location that contain a a and are not in a0 a1 a2 a3
         (off-a-locations(--filter (not (or (eq it :a0)
                                            (eq it :a1)
                                            (eq it :a2)
                                            (eq it :a3)))
                                   a-locations))
         ;; all a-room locations not occupied by a 'a'
         (empty-places (--filter (not (eq (plist-get state it) :a))'(:a0 :a1 :a2 :a3))))
    (assert (= (length empty-places) (length off-a-locations)))
    (apply #'+ (--map (day23-l/l-compute-cost it :a)  (-zip empty-places off-a-locations)))))

(defun day23-l/projected-min-cost (state)
  (+ (day23-l/projected-min-a-cost state)
     (day23-l/projected-min-b-cost state)
     (day23-l/projected-min-c-cost state)
     (day23-l/projected-min-d-cost state)))

(defun day23-l/evolve (state minimum-score)
  "Returns the minimum score for a win"
  (let ((current-score (plist-get state :score)))
    ;(day23-l/debug-print (format "%s\nState:\n%s\n(score: %d)\n" state (day23-l/to-string state) current-score))
    (when day23-l/stepping
      (read-string "Continue?"))
    (if (day23-l/l-is-win? state)
        (progn
          (print (format "New win! %d" current-score))
          (redisplay)
          current-score)
      (if (>= (+ current-score (day23-l/projected-min-cost state)) minimum-score)
          (day23-l/debug-print (format "USELESS! %d" current-score))        
          (let ((next-moves (day23-l/l-next state)))
            (if next-moves
                ;; how many are still below the previous minimum?
                (let ((useful-moves (--filter (< (+ current-score
                                                    (elt it 2))
                                                 minimum-score)
                                              next-moves )))
                  (if useful-moves    ; otherwise is 'nil', that is, a dead end
                      (--reduce-from (let ((new-score-or-nil (day23-l/evolve (day23-l/update state it) acc)))
                                       (or (and new-score-or-nil (min new-score-or-nil acc)) acc))
                                     minimum-score
                                     useful-moves)
                    (day23-l/debug-print (format "OVERFLOW (%d + move > %d)" current-score minimum-score))))
              ))))))

(defun day23-l/debug--print-futures (text)
  (let ((state (if (stringp text)
                   (day23-l/l-read-problem (split-string text))
                 text)))
    (print (format "CURRENT (score: %d):\n%s\n%s"
                   (plist-get state :score)
                   state
                   (day23-l/to-string state)))
    (let ((next-states (--map (day23-l/update state it) (day23-l/l-next state))))
      (print (format "FUTURES:\n"))
      (--each next-states (print (format "*** SCORE: %d\n%s\n%s"
                                         (plist-get it :score)
                                         it
                                         (day23-l/to-string it))))))
  nil)

(defun day23-l/solution (lines)
  (day23-l/evolve (day23-l/l-read-problem lines) day23-l/hyper--score))

(provide 'day23-l)

