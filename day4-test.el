(require 'day4)
(require 'buttercup)

(describe "Day 4"
  (describe "part 1"
    (it "replicates the example"
      (expect (day4/part-1 (advent/read-blocks-of-lines 4 :example))
              :to-be 4512 ))
    (it "solves the problem"
      (expect (day4/part-1 (advent/read-blocks-of-lines 4 :problem))
              :to-be 51776)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day4/part-2 (advent/read-blocks-of-lines 4 :example))
              :to-be 230))
    (xit "solves the problem"
      (expect (day4/part-2 (advent/read-blocks-of-lines 4 :problem))
              :to-be 4550283))))
