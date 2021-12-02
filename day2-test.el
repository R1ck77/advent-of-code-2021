(require 'day2)
(require 'buttercup)

(describe "Day 2"
  (describe "part 1"
    (it "replicates the example"
      (expect (day2/part-1 (read-problem-instructions 2 :example))
              :to-be 150))
    (it "solves the problem"
      (expect (day2/part-1 (read-problem-instructions 2 :problem))
              :to-be 1924923)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day2/part-2 (read-problem-instructions 2 :example))
              :to-be 900))
    (it "solves the problem"
      (expect (day2/part-2 (read-problem-instructions 2 :problem))
              :to-be 1982495697))))
