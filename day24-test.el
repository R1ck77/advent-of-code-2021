(require 'day24)
(require 'buttercup)

;; I'm not even trying to read the data here: the information was extracted manually
(describe "--- Day 24: Arithmetic Logic Unit ---"
  (describe "part 1"
    (it "solves the problem    ⚠ semi-manual hack"
      (expect (day24/part-1)
              :to-be 59692994994998)))
  (describe "part 2"
    (it "solves the problem    ⚠ semi-manual hack"
      (expect (day24/part-2)
              :to-be 16181111641521))))
