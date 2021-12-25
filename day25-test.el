(require 'day25)
(require 'buttercup)

(describe "Day 25"
  (describe "part 1"
    (it "replicates the example"
      (expect (day25/part-1 (advent/read-grid 25 :example #'identity))
              :to-be 58))
    (it "solves the problem"
      (expect (day25/part-1 (advent/read-grid 25 :problem #'identity))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day25/part-2 (advent/read-grid 25 :example #'identity))
              :to-be 42))
    (xit "solves the problem"
      (expect (day25/part-2 (advent/read-grid 25 :problem #'identity))
              :to-be 42))))
