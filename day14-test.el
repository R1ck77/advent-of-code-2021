(require 'day14)
(require 'buttercup)


(describe "Day 14"
  (describe "part 1"
    (describe "expansion"
      (it "expands the example correctly once"
        (expect (day14/format-polymer
                 (day14/expand
                  (day14/read-template (advent/read-blocks-of-lines 14 :example))))
                :to-equal "NCNBCHB"))
      (it "expands the example correctly twice"
        (expect (day14/format-polymer
                 (day14/expand
                  (day14/read-template (advent/read-blocks-of-lines 14 :example)) 2))
                :to-equal "NBCCNBBBCBHCB"))
      (it "expands the example correctly three times"
        (expect (day14/format-polymer
                 (day14/expand
                  (day14/read-template (advent/read-blocks-of-lines 14 :example)) 3))
                :to-equal "NBBBCNCCNBBNBNBBCHBHHBCHB"))
      (it "expands the example correctly four times"
        (expect (day14/format-polymer
                 (day14/expand
                  (day14/read-template (advent/read-blocks-of-lines 14 :example)) 4))
                :to-equal "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")))
    (it "replicates the example"
      (expect (day14/part-1 (advent/read-blocks-of-lines 14 :example))
              :to-be 1588 ))
    (xit "solves the problem"
      (expect (day14/part-1 (advent/read-blocks-of-lines 14 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day14/part-2 (advent/read-blocks-of-lines 14 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day14/part-2 (advent/read-blocks-of-lines 14 :problem))
              :to-be 42))))
