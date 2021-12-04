(require 'day3)
(require 'buttercup)

(describe "Day 3"
  (describe "part 1"
    (it "replicates the example"
      (expect (day3/part-1 (advent/read-problem-lines 3 :example))
              :to-be 198))
    (it "solves the problem"
      (expect (day3/part-1 (advent/read-problem-lines 3 :problem))
              :to-be 3633500)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day3/part-2 (advent/read-problem-lines 3 :example))
              :to-be 230))
    (it "solves the problem"
      (expect (day3/part-2 (advent/read-problem-lines 3 :problem))
              :to-be 4550283))))
