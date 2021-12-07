(require 'day7)
(require 'buttercup)

(describe "Day 7"
  (describe "part 1"
    (it "replicates the example"
      (expect (day7/part-1 (car (advent/read-problem-lines 7 :example)))
              :to-be 37 ))
    (it "solves the problem"
      (expect (day7/part-1 (car (advent/read-problem-lines 7 :problem)))
              :to-be 342730)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day7/part-2 (car (advent/read-problem-lines 7 :example)))
              :to-be 168))
    (it "solves the problem"
      (expect (day7/part-2 (car (advent/read-problem-lines 7 :problem)))
              :to-be 92335207))))
