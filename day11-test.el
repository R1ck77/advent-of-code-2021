(require 'day11)
(require 'buttercup)

(describe "Day 11"
  (describe "part 1"
    (describe "small example evolution"
      (it "can replicate step 1"
        (let ((input (day11/read-grid (split-string "11111
19991
19191
19991
11111"))))
          (day11/evolve! input)
          (expect (day11/str-grid input)
                  :to-equal (day11/str-grid
                             (day11/read-grid
                              (split-string "34543
40004
50005
40004
34543"))))))

      (it "can replicate step 2"
        (let ((input (day11/read-grid (split-string "34543
40004
50005
40004
34543"))))
          (day11/evolve! input)
         (expect (day11/str-grid input)
                 :to-equal (day11/str-grid
                            (day11/read-grid
                             (split-string "45654
51115
61116
51115
45654")))))))
    (it "replicates the example"
      (expect (day11/part-1 (advent/read-problem-lines 11 :example))
              :to-be 1656))
    (it "solves the problem"
      (expect (day11/part-1 (advent/read-problem-lines 11 :problem))
              :to-be 1729)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day11/part-2 (advent/read-problem-lines 11 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day11/part-2 (advent/read-problem-lines 11 :problem))
              :to-be 42))))
