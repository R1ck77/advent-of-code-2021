(require 'day02)
(require 'buttercup)

(describe "Day 2"
  (describe "part 1"
    (it "replicates the example"
      (expect (day02/part-1 (advent/read-problem-instructions 2 :example))
              :to-be 150))
    (it "solves the problem"
      (expect (day02/part-1 (advent/read-problem-instructions 2 :problem))
              :to-be 1924923)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day02/part-2 (advent/read-problem-instructions 2 :example))
              :to-be 900))
    (it "solves the problem"
      (expect (day02/part-2 (advent/read-problem-instructions 2 :problem))
              :to-be 1982495697))))
