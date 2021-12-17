(require 'day17)
(require 'buttercup)

(describe "Day 17"
  (describe "part 1"
    (it "replicates the example"
      (expect (day17/part-1 (car (advent/read-problem-lines 17 :example)))
              :to-be 45 ))
    (xit "solves the problem"
      (expect (day17/part-1 (car (advent/read-problem-lines 17 :problem)))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day17/part-2 (car (advent/read-problem-lines 17 :example)))
              :to-be 42))
    (xit "solves the problem"
      (expect (day17/part-2 (car (advent/read-problem-lines 17 :problem)))
              :to-be 42))))
