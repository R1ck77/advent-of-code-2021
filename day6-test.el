(require 'day6)
(require 'buttercup)

(describe "Day 6"
  (describe "part 1"
    (it "replicates the example"
      (expect (day6/part-1 (car (advent/read-problem-lines 6 :example)))
              :to-be 5934 ))
    (it "solves the problem"
      (expect (day6/part-1 (car (advent/read-problem-lines 6 :problem)))
              :to-be 352872)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day6/part-2 (car (advent/read-problem-lines 6 :example)))
              :to-be 26984457539))
    (it "solves the problem"
      (expect (day6/part-2 (car (advent/read-problem-lines 6 :problem)))
              :to-be 1604361182149))))
