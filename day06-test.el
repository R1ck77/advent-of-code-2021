(require 'day06)
(require 'buttercup)

(describe "--- Day 6: Lanternfish ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day06/part-1 (advent/read-problem-numbers-line 6 :example))
              :to-be 5934 ))
    (it "solves the problem"
      (expect (day06/part-1 (advent/read-problem-numbers-line 6 :problem))
              :to-be 352872)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day06/part-2 (advent/read-problem-numbers-line 6 :example))
              :to-be 26984457539))
    (it "solves the problem"
      (expect (day06/part-2 (advent/read-problem-numbers-line 6 :problem))
              :to-be 1604361182149))))
