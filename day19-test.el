(require 'day19)
(require 'buttercup)

(describe "Day 19"
  (describe "part 1"
    (it "replicates the example"
      (expect (day19/part-1 (advent/read-problem-lines 19 :example))
              :to-be 42 ))
    (xit "solves the problem"
      (expect (day19/part-1 (advent/read-problem-lines 19 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day19/part-2 (advent/read-problem-lines 19 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day19/part-2 (advent/read-problem-lines 19 :problem))
              :to-be 42))))
