(require 'day08)
(require 'buttercup)

(describe "--- Day 8: Seven Segment Search ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day08/part-1 (advent/read-problem-lines 8 :example))
              :to-be 26 ))
    (it "solves the problem"
      (expect (day08/part-1 (advent/read-problem-lines 8 :problem))
              :to-be 543)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day08/part-2 (advent/read-problem-lines 8 :example))
              :to-be 61229))
    (it "solves the problem"
      (expect (day08/part-2 (advent/read-problem-lines 8 :problem))
              :to-be 994266))))
