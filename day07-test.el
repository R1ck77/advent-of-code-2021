(require 'day07)
(require 'buttercup)

(describe "--- Day 7: The Treachery of Whales ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day07/part-1 (advent/read-problem-numbers-line 7 :example))
              :to-be 37 ))
    (it "solves the problem"
      (expect (day07/part-1 (advent/read-problem-numbers-line 7 :problem))
              :to-be 342730)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day07/part-2 (advent/read-problem-numbers-line 7 :example))
              :to-be 168))
    (it "solves the problem"
      (expect (day07/part-2 (advent/read-problem-numbers-line 7 :problem))
              :to-be 92335207))))
