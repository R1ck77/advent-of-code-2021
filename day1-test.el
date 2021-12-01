(require 'day1)
(require 'buttercup)

(describe "Day 1 solution"
  (it "replicates the example of part 1"
    (expect (day1/part-1 (read-problem-numbers 1 :example))
            :to-be 7))
  (it "solves the first part of the problem"
    (expect (day1/part-1 (read-problem-numbers 1 :problem))
            :to-be 1233))
  (it "replicates the example of part 2"
    (expect (day1/part-2 (read-problem-numbers 1 :example))
            :to-be 5))
  (it "solves the second part of the problem"
    (expect (day1/part-2 (read-problem-numbers 1 :problem))
            :to-be 1275)))
