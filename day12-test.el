(require 'day12)
(require 'buttercup)

(describe "Day 12"
  (describe "part 1"
    (describe "small examples"
      (it "solves the small example"
        (expect (day12/part-1 "start-A
start-b
A-c
A-b
b-d
A-end
b-end")
                :to-be 10))
      (it "generates the same paths for the small example"
        (expect (sort
                 (-map #'day12/str-path
                       (day12/compute-all-paths "start-A
start-b
A-c
A-b
b-d
A-end
b-end"))
                 #'string<)
                :to-be (sort ("start,A,b,A,c,A,end"
                              "start,A,b,A,end"
                              "start,A,b,end"
                              "start,A,c,A,b,A,end"
                              "start,A,c,A,b,end"
                              "start,A,c,A,end"
                              "start,A,end"
                              "start,b,A,c,A,end"
                              "start,b,A,end"
                              "start,b,end")
                             #'string<)))
      (xit "solves the medium example"
        (expect (day12/part-1 "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")
                :to-be 19)))
    (xit "replicates the example"
      (expect (day12/part-1 (advent/read-problem-lines 12 :example))
              :to-be 226))
    (xit "solves the problem"
      (expect (day12/part-1 (advent/read-problem-lines 12 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day12/part-2 (advent/read-problem-lines 12 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day12/part-2 (advent/read-problem-lines 12 :problem))
              :to-be 42))))
