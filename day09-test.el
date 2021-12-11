(require 'day09.el)
(require 'buttercup)

(describe "Day 9"
  (describe "part 1"
    (it "replicates the example"
      (expect (day09.el/part-1 (advent/read-problem-lines 9 :example))
              :to-be 15 ))
    (it "solves the problem"
      (expect (day09.el/part-1 (advent/read-problem-lines 9 :problem))
              :to-be 512)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day09.el/part-2 (advent/read-problem-lines 9 :example))
              :to-be 1134))
    (it "solves the problem"
      (expect (day09.el/part-2 (advent/read-problem-lines 9 :problem))
              :to-be 1600104))))
