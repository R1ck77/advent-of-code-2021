(require 'day22)
(require 'buttercup)

(defvar day22-test/small-example (list "on x=10..12,y=10..12,z=10..12"
                                       "on x=11..13,y=11..13,z=11..13"
                                       "off x=9..11,y=9..11,z=9..11"
                                       "on x=10..10,y=10..10,z=10..10"))

(describe "--- Day 22: Reactor Reboot ---"
  (describe "part 1"
    (it "replicates the small example"
            (expect (day22/part-1 day22-test/small-example) :to-be 39 ))
    (it "replicates the example"
      (expect (day22/part-1 (advent/read-problem-lines 22 :example)) :to-be 474140 ))
    (it "solves the problem"
      (expect (day22/part-1 (advent/read-problem-lines 22 :problem)) :to-be 611176)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day22/part-2 (advent/read-problem-lines 22 :example))
              :to-be 2758514936282235))
    (it "solves the problem"
      (expect (day22/part-2 (advent/read-problem-lines 22 :problem))
              :to-be 1201259791805392))))
