(require 'day14)
(require 'buttercup)

(describe "Day 14"
  (describe "part 1"
    (it "replicates the example"
      (expect (day14/part-1 (advent/read-blocks-of-lines 14 :example))
              :to-be 1588 ))
    (it "solves the problem"
      (expect (day14/part-1 (advent/read-blocks-of-lines 14 :problem))
              :to-be 2937)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day14/part-2 (advent/read-blocks-of-lines 14 :example))
              :to-be 2188189693529))
    (it "solves the problem"
      (expect (day14/part-2 (advent/read-blocks-of-lines 14 :problem))
              :to-be 3390034818249))))
