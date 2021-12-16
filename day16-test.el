(require 'day16)
(require 'buttercup)

(describe "Day 16"
  (describe "part 1"
    (describe "examples"
     (it "replicates the example 1"
       (expect (day16/part-1 "8A004A801A8002F478") :to-be 16))
     (it "replicates the example 2"
       (expect (day16/part-1 "620080001611562C8802118E34") :to-be 12))
     (it "replicates the example 3"
       (expect (day16/part-1 "C0015000016115A2E0802F182340") :to-be 23))
     (it "replicates the example 4"
       (expect (day16/part-1 "A0016C880162017C3686B18A3D4780") :to-be 31)))        
    (it "solves the problem"
      (expect (day16/part-1 (car (advent/read-problem-lines 16 :problem)))
              :to-be 893)))
  (describe "part 2"
    (describe "examples"
      (it "replicates the example 1"
        (expect (day16/part-2 "C200B40A82") :to-be 3))
      (it "replicates the example 2"
        (expect (day16/part-2 "04005AC33890") :to-be 54))
      (it "replicates the example 3"
        (expect (day16/part-2 "880086C3E88112") :to-be 7))    
      (it "replicates the example 4"
        (expect (day16/part-2 "CE00C43D881120") :to-be 9))
      (it "replicates the example 5"
        (expect (day16/part-2 "D8005AC2A8F0") :to-be 1))
      (it "replicates the example 6"
        (expect (day16/part-2 "F600BC2D8F") :to-be 0))
      (it "replicates the example 7"
        (expect (day16/part-2 "9C005AC2F8F0") :to-be 0))
      (it "replicates the example 8"
        (expect (day16/part-2 "9C0141080250320F1802104A08") :to-be 1)))    
    (it "solves the problem"
      (expect (day16/part-2 (car (advent/read-problem-lines 16 :problem)))
              :to-be 4358595186090))))
