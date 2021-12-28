(require 'day24)
(require 'buttercup)

(describe "--- Day 24: Arithmetic Logic Unit ---"
  (describe "part 1"
    (it "solves the problem    ⚠ semi-manual hack"
      (expect (day24/part-1 (advent/read-problem-lines 24 :problem))
              :to-be 59692994994998)))
  (describe "part 2"
    (it "solves the problem    ⚠ semi-manual hack"
      (expect (day24/part-2 (advent/read-problem-lines 24 :problem))
              :to-be 16181111641521))))
