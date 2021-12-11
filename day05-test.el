(require 'day05)
(require 'buttercup)

(describe "Day 5"
  (describe "part 1"
    (it "replicates the example"
      (expect (day05/part-1 (advent/read-problem-lines 5 :example))
              :to-be 5 ))
    (it "solves the problem"
      (expect (day05/part-1 (advent/read-problem-lines 5 :problem))
              :to-be 6311)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day05/part-2 (advent/read-problem-lines 5 :example))
              :to-be 12))
    (it "solves the problem"
      (expect (day05/part-2 (advent/read-problem-lines 5 :problem))
              :to-be 19929))))
