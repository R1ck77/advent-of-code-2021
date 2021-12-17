(require 'day18)
(require 'buttercup)

(describe "Day 18"
  (describe "part 1"
    (it "replicates the example"
      (expect (day18/part-1 (advent/read-problem-lines 18 :example))
              :to-be 42 ))
    (xit "solves the problem"
      (expect (day18/part-1 (advent/read-problem-lines 18 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day18/part-2 (advent/read-problem-lines 18 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day18/part-2 (advent/read-problem-lines 18 :problem))
              :to-be 42))))