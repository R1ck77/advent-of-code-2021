(require 'day18)
(require 'buttercup)

(describe "Day 18"
  (describe "read"
    (it "can read expressions correctly"
      (expect (day18/read-number "[1,2]") :to-equal '(1 2))
      (expect (day18/read-number "[[1,2],[[3,4],5]]") :to-equal '((1 2) ((3 4) 5)))))
  (describe "part 1"
    (describe "operations"
      (it "performs the magnitude operation correctly"
        (expect (day18/magnitude (day18/read-number "[[1,2],[[3,4],5]]")) :to-be 143)
        (expect (day18/magnitude (day18/read-number "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")) :to-be 1384)
        (expect (day18/magnitude (day18/read-number "[[[[1,1],[2,2]],[3,3]],[4,4]]")) :to-be 445)
        (expect (day18/magnitude (day18/read-number "[[[[3,0],[5,3]],[4,4]],[5,5]]")) :to-be 791)
        (expect (day18/magnitude (day18/read-number "[[[[5,0],[7,4]],[5,5]],[6,6]]")) :to-be 1137)
        (expect (day18/magnitude (day18/read-number "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")) :to-be 3488))
      (it "can do simple sums without reduction correctly"
        (expect (day18/sum-all (-map #'day18/read-number (list "[1,1]" "[2,2]" "[3,3]" "[4,4]")))
                :to-equal (day18/read-number "[[[[1,1],[2,2]],[3,3]],[4,4]]")))
      (it "can explode a number correctly"
        (expect (day18/explode-1 (day18/read-number "[[[[[9,8],1],2],3],4]") :to-equal (day18/read-number "[[[[0,9],2],3],4]")))
        (expect (day18/explode-1 (day18/read-number "[7,[6,[5,[4,[3,2]]]]]") :to-equal (day18/read-number "[7,[6,[5,[7,0]]]]")))
        (expect (day18/explode-1 (day18/read-number "[[6,[5,[4,[3,2]]]],1]") :to-equal (day18/read-number "[[6,[5,[7,0]]],3]")))
        (expect (day18/explode-1 (day18/read-number "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]") :to-equal (day18/read-number "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")))
        (expect (day18/explode-1 (day18/read-number "") :to-equal (day18/read-number "")))
        (expect (day18/explode-1 (day18/read-number "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]") :to-equal (day18/read-number "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"))))
      (it "can split a number correctly"
        (expect (day18/reduce (day18/read-number "[1,[6,13]]")) :to-equal (day18/read-number "[1,[6,[6,7]]]")))
      (it "can reduce a number correctly"
        (expect (day18/reduce (day18/read-number "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")) :to-equal (day18/read-number "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")))
      (it "can do pairwise sums correctly"
        (expect (apply #'day18/sum (-map #'day18/read-number (list "[[[[4,3],4],4],[7,[[8,4],9]]]" "[1,1]")))
                :to-equal (day18/read-number "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")))
      (it "can do simple sums correctly"        
        (expect (day18/sum-all (-map #'day18/read-number (list "[1,1]" "[2,2]" "[3,3]" "[4,4]")))
                :to-equal (day18/read-number "[[[[1,1],[2,2]],[3,3]],[4,4]]"))
        (expect (day18/sum-all (-map #'day18/read-number (list "[1,1]" "[2,2]" "[3,3]" "[4,4]" "[5,5]")))
                :to-equal (day18/read-number "[[[[3,0],[5,3]],[4,4]],[5,5]]"))
        (expect (day18/sum-all (-map #'day18/read-number (list "[1,1]" "[2,2]" "[3,3]" "[4,4]" "[5,5]" "[6,6]")))
                :to-equal (day18/read-number "[[[[5,0],[7,4]],[5,5]],[6,6]]")))
      (it "can do a chain of sums correctly"
        (expect (day18/sum-all (-map #'day18/read-number (list "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
                                                               "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
                                                               "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
                                                               "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
                                                               "[7,[5,[[3,8],[1,4]]]]"
                                                               "[[2,[2,2]],[8,[8,1]]]"
                                                               "[2,9]"
                                                               "[1,[[[9,3],9],[[9,0],[0,7]]]]"
                                                               "[[[5,[7,4]],7],1]"
                                                               "[[[[4,2],2],6],[8,7]]")))
                :to-equal (day18/read-number "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))))
    (it "replicates the example"
      (expect (day18/part-1 (advent/read-problem-lines 18 :example))
              :to-be 4140 ))
    (xit "solves the problem"
      (expect (day18/part-1 (advent/read-problem-lines 18 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day18/part-2 (advent/read-problem-lines 18 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day18/part-2 (advent/read-problem-lines 18 :problem))
              :to-be 42))))
