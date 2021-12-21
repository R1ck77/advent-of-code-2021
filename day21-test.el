(require 'day21)
(require 'buttercup)

(describe "Day 21"
  (describe "part 1"
    (it "replicates the example"
      (expect (day21/part-1 (advent/read-problem-lines 21 :example))
              :to-be 739785))
    (it "solves the problem"
      (expect (day21/part-1 (advent/read-problem-lines 21 :problem))
              :to-be 598416)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day21/part-2 (advent/read-problem-lines 21 :example))
              :to-be 444356092776315))
    (xit "solves the problem"
      (expect (day21/part-2 (advent/read-problem-lines 21 :problem))
              :to-be 42))))
