(require 'day16)
(require 'buttercup)

(describe "Day 16"
  (describe "part 1"
    (it "replicates the example 1"
      (expect (day16/part-1 "8A004A801A8002F478") :to-be 16))
    (it "replicates the example 2"
      (expect (day16/part-1 "620080001611562C8802118E34") :to-be 12))
    (it "replicates the example 3"
      (expect (day16/part-1 "C0015000016115A2E0802F182340") :to-be 23))
    (it "replicates the example 4"
      (expect (day16/part-1 "A0016C880162017C3686B18A3D4780") :to-be 31))        
    (it "solves the problem"
      (expect (day16/part-1 (car (advent/read-problem-lines 16 :problem)))
              :to-be 893)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day16/part-2 (advent/read-problem-lines 16 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day16/part-2 (advent/read-problem-lines 16 :problem))
              :to-be 42))))
