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
                       (day12/compute-all-paths-simple
                        (day12/read-nodes (split-string "start-A
start-b
A-c
A-b
b-d
A-end
b-end"))))
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
      (it "computes the number of paths for the medium example"
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
    (it "replicates the example"
      (expect (day12/part-1 (advent/read-problem-lines 12 :example))
              :to-be 226))
    (it "solves the problem"
      (expect (day12/part-1 (advent/read-problem-lines 12 :problem))
              :to-be 4754)))
  (describe "part 2"
    (describe "small examples"
      (it "computes the number of paths for the tiny example"
        (expect (day12/part-2
                 (split-string "start-A
start-b
A-c
A-b
b-d
A-end
b-end"))
                :to-be 36))
      (it "computes all paths for the tiny example"
        (expect (sort
                 (-map #'day12/str-path
                       (day12/compute-all-paths-count
                        (day12/read-nodes (split-string "start-A
start-b
A-c
A-b
b-d
A-end
b-end"))))
                 #'string<)
                :to-equal (sort '("start,A,b,A,b,A,c,A,end"
                                  "start,A,b,A,b,A,end"
                                  "start,A,b,A,b,end"
                                  "start,A,b,A,c,A,b,A,end"
                                  "start,A,b,A,c,A,b,end"
                                  "start,A,b,A,c,A,c,A,end"
                                  "start,A,b,A,c,A,end"
                                  "start,A,b,A,end"
                                  "start,A,b,d,b,A,c,A,end"
                                  "start,A,b,d,b,A,end"
                                  "start,A,b,d,b,end"
                                  "start,A,b,end"
                                  "start,A,c,A,b,A,b,A,end"
                                  "start,A,c,A,b,A,b,end"
                                  "start,A,c,A,b,A,c,A,end"
                                  "start,A,c,A,b,A,end"
                                  "start,A,c,A,b,d,b,A,end"
                                  "start,A,c,A,b,d,b,end"
                                  "start,A,c,A,b,end"
                                  "start,A,c,A,c,A,b,A,end"
                                  "start,A,c,A,c,A,b,end"
                                  "start,A,c,A,c,A,end"
                                  "start,A,c,A,end"
                                  "start,A,end"
                                  "start,b,A,b,A,c,A,end"
                                  "start,b,A,b,A,end"
                                  "start,b,A,b,end"
                                  "start,b,A,c,A,b,A,end"
                                  "start,b,A,c,A,b,end"
                                  "start,b,A,c,A,c,A,end"
                                  "start,b,A,c,A,end"
                                  "start,b,A,end"
                                  "start,b,d,b,A,c,A,end"
                                  "start,b,d,b,A,end"
                                  "start,b,d,b,end"
                                  "start,b,end")
                                #'string<)))
      (it "computes the number of paths for the medium example"
        (expect (day12/part-2
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
                :to-be 103)))
    (it "replicates the example"
      (expect (day12/part-2 (advent/read-problem-lines 12 :example))
              :to-be 3509))
    (it "solves the problem"
      (expect (day12/part-2 (advent/read-problem-lines 12 :problem))
              :to-be 143562))))
