(require 'day12)
(require 'buttercup)

(describe "Day 12"
  (describe "part 1"
    (describe "small examples"
      (it "computes the number of paths for the tiny example"
        (expect (day12/part-1
                 (split-string "start-A
start-b
A-c
A-b
b-d
A-end
b-end"))
                :to-be 10))
      (it "computes all paths for the tiny example"
        (expect (sort
                 (-map #'day12/str-path
                       (day12/compute-all-paths
                        (split-string "start-A
start-b
A-c
A-b
b-d
A-end
b-end")))
                 #'string<)
                :to-equal (sort '("start,A,b,A,c,A,end"
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
      (xit "computes the number of paths for the medium example"
        (expect (day12/part-1
                 (split-string "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"))
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
