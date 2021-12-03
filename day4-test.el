(require 'day4)
(require 'buttercup)

(describe "Day 4"
  (describe "part 1"
    (it "replicates the example"
      (expect (day4/part-1 (read-problem-lines 4 :example))
              :to-be 198))
    (xit "solves the problem"
      (expect (day4/part-1 (read-problem-lines 4 :problem))
              :to-be 3633500)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day4/part-2 (read-problem-lines 4 :example))
              :to-be 230))
    (xit "solves the problem"
      (expect (day4/part-2 (read-problem-lines 4 :problem))
              :to-be 4550283))))
