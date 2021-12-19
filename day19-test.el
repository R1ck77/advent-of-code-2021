(require 'day19)
(require 'buttercup)

(defconst day19-test/example (day19/read-scans (advent/read-blocks-of-lines 19 :example)))

(describe "Day 19"
  (describe "part 1"
    (xdescribe "scan pairs detection"
      (it "can detect the pairing of 1 and 0"
        (expect (day19/two-scanners-match? (elt day19-test/example 0)
                                           (elt day19-test/example 1))
               :not :to-be nil)
        (expect (day19/two-scanners-match? (elt day19-test/example 1)
                                           (elt day19-test/example 0))
               :not :to-be nil))
      (it "can detect the pairing of 1 and 4"
        (expect (day19/two-scanners-match? (elt day19-test/example 4)
                                           (elt day19-test/example 1))
               :not :to-be nil)
        (expect (day19/two-scanners-match? (elt day19-test/example 1)
                                           (elt day19-test/example 4))
                :not :to-be nil))
      (it "can detect the pairing of 4 and 2"
        (expect (day19/two-scanners-match? (elt day19-test/example 4)
                                           (elt day19-test/example 2))
               :not :to-be nil)
        (expect (day19/two-scanners-match? (elt day19-test/example 2)
                                           (elt day19-test/example 4))
                :not :to-be nil))
      (it "can detect the pairing of 1 and 3"
        (expect (day19/two-scanners-match? (elt day19-test/example 1)
                                           (elt day19-test/example 3))
               :not :to-be nil)
        (expect (day19/two-scanners-match? (elt day19-test/example 3)
                                           (elt day19-test/example 1))
               :not :to-be nil))      
      (it "can detect the lack of pairing between 3 and 4"
        (expect (day19/two-scanners-match? (elt day19-test/example 3)
                                           (elt day19-test/example 4))
               :to-be nil)
        (expect (day19/two-scanners-match? (elt day19-test/example 3)
                                           (elt day19-test/example 4))
                :to-be nil))
      (it "can detect the lack of pairing between 1 and 2"
        (expect (day19/two-scanners-match? (elt day19-test/example 1)
                                           (elt day19-test/example 2))
               :to-be nil)
        (expect (day19/two-scanners-match? (elt day19-test/example 2)
                                           (elt day19-test/example 1))
                :to-be nil))
      (it "can detect the lack of pairing between 4 and 3"
        (expect (day19/two-scanners-match? (elt day19-test/example 4)
                                           (elt day19-test/example 3))
               :to-be nil)
        (expect (day19/two-scanners-match? (elt day19-test/example 3)
                                           (elt day19-test/example 4))
               :to-be nil)))
    (it "replicates the example"
      (expect (day19/part-1 (advent/read-blocks-of-lines 19 :example))
              :to-be 79 ))
    (it "solves the problem"
      (expect (day19/part-1 (advent/read-blocks-of-lines 19 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day19/part-2 (advent/read-blocks-of-lines 19 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day19/part-2 (advent/read-blocks-of-lines 19 :problem))
              :to-be 42))))
