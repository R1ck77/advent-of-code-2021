(require 'day5)
(require 'buttercup)

(describe "Day 5"
  (describe "part 1"
    (it "replicates the example"
      (expect (day5/part-1 (advent/read-blocks-of-lines 5 :example))
              :to-be 42 ))
    (xit "solves the problem"
      (expect (day5/part-1 (advent/read-blocks-of-lines 5 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day5/part-2 (advent/read-blocks-of-lines 5 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day5/part-2 (advent/read-blocks-of-lines 5 :problem))
              :to-be 42))))
