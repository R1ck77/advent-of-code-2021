(require 'day1)
(require 'buttercup)

(describe "Day 1 solution"
  (it "replicates the example of part 1"
    (expect (day1/part-1 (read-problem-numbers 1 1 :example))
            :to-be 7))
  (it "solves the first part of the problem"
    (expect (day1/part-1 (read-problem-numbers 1 1 :problem))
            :to-be 1233)))
