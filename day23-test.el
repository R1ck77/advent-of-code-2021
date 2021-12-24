(require 'day23)
(require 'buttercup)

(describe "Day 23"
  (describe "part 1"
    (it "replicates the example"
      (expect (day23/part-1 (advent/read-problem-lines 23 :example 1))
              :to-be 12521))
    (it "solves the problem"
      (expect (day23/part-1 (advent/read-problem-lines 23 :problem 1))
              :to-be 16157)))
  (describe "part 2"
    (xit "replicates the example"
      (expect (day23/part-2 (advent/read-problem-lines 23 :example 2))
              :to-be 44169))
    (it "solves the problem"
      (expect (day23/part-2 (advent/read-problem-lines 23 :problem 2))
              :to-be 43481))))
