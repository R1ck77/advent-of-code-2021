(require 'day23)
(require 'buttercup)

(describe "Day 23"
  (describe "part 1"
    (it "replicates the example"
      (expect (day23/part-1 (advent/read-problem-lines 23 :example))
              :to-be 42 ))
    (xit "solves the problem"
      (expect (day23/part-1 (advent/read-problem-lines 23 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day23/part-2 (advent/read-problem-lines 23 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day23/part-2 (advent/read-problem-lines 23 :problem))
              :to-be 42))))
