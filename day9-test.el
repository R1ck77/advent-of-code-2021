(require 'day9)
(require 'buttercup)

(describe "Day 9"
  (describe "part 1"
    (it "replicates the example"
      (expect (day9/part-1 (advent/read-problem-lines 9 :example))
              :to-be 15 ))
    (it "solves the problem"
      (expect (day9/part-1 (advent/read-problem-lines 9 :problem))
              :to-be 512)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day9/part-2 (advent/read-problem-lines 9 :example))
              :to-be 1134))
    (it "solves the problem"
      (expect (day9/part-2 (advent/read-problem-lines 9 :problem))
              :to-be 1600104))))