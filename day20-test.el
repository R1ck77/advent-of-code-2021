(require 'day20)
(require 'buttercup)

(describe "--- Day 20: Trench Map ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day20/part-1 (advent/read-blocks-of-lines 20 :example))
              :to-be 35 ))
    (it "solves the problem"
      (expect (day20/part-1 (advent/read-blocks-of-lines 20 :problem))
              :to-be 5229)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day20/part-2 (advent/read-blocks-of-lines 20 :example))
              :to-be 3351))
    (it "solves the problem"
      (expect (day20/part-2 (advent/read-blocks-of-lines 20 :problem))
              :to-be 17009))))
