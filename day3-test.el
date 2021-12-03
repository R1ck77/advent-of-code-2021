(require 'day3)
(require 'buttercup)

(describe "Day 2"
  (describe "part 1"
    (it "replicates the example"
      (expect (day3/part-1 (read-problem-lines 3 :example))
              :to-be 198))
    (xit "solves the problem"
      (expect (day3/part-1 (read-problem-lines 3 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day3/part-2 (read-problem-lines 3 :example))
              :to-be 42))
    (it "solves the problem"
      (expect (day3/part-2 (read-problem-lines 3 :problem))
              :to-be 42))))
