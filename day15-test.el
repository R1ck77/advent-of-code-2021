(require 'day15)
(require 'buttercup)

(describe "Day 15"
  (describe "part 1"
    (it "replicates the example"
      (expect (day15/part-1 (advent/read-grid 15 :example #'string-to-number))
              :to-be 40 ))
    (xit "solves the problem"
      (expect (day15/part-1 (advent/read-grid 15 :problem #'string-to-number))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day15/part-2 (advent/read-grid 15 :example #'string-to-number))
              :to-be 42))
    (xit "solves the problem"
      (expect (day15/part-2 (advent/read-grid 15 :problem #'string-to-number))
              :to-be 42))))
