(require 'day10)
(require 'buttercup)

(describe "Day 10"
  (describe "part 1"
    (describe "single tests"
      (it "correctly categorizes [(()[<>])]({[<{<<[]>>( as not corrupted"
        (expect (day10/corrupted? (day10/read-blocks "[(()[<>])]({[<{<<[]>>("))
                :to-be nil))
      (it "correctly categorizes {([(<{}[<>[]}>{[]{[(<()> as corrupted"
        (expect (day10/corrupted? (day10/read-blocks "{([(<{}[<>[]}>{[]{[(<()>"))
                :to-be :-c))
      (it "correctly categorizes (((({<>}<{<{<>}{[]{[]{} as not corrupted"
        (expect (day10/corrupted? (day10/read-blocks "(((({<>}<{<{<>}{[]{[]{}"))
                :to-be nil))
      (it "correctly categorizes [[<[([]))<([[{}[[()]]] as corrupted"
        (expect (day10/corrupted? (day10/read-blocks "[[<[([]))<([[{}[[()]]]"))
                :to-be :-r))
      (it "correctly categorizes [{[{({}]{}}([{[{{{}}([] as corrupted"
        (expect (day10/corrupted? (day10/read-blocks "[{[{({}]{}}([{[{{{}}([]"))
                :to-be :-s))
      (it "correctly categorizes {<[[]]>}<{[{[{[]{()[[[] as not corrupted"
        (expect (day10/corrupted? (day10/read-blocks "{<[[]]>}<{[{[{[]{()[[[]"))
                :to-be nil))
      (it "correctly categorizes [<(<(<(<{}))><([]([]() as corrupted"
        (expect (day10/corrupted? (day10/read-blocks "[<(<(<(<{}))><([]([]()"))
                :to-be :-r))
      (it "correctly categorizes <{([([[(<>()){}]>(<<{{ as corrupted"
        (expect (day10/corrupted? (day10/read-blocks "<{([([[(<>()){}]>(<<{{"))
                :to-be :-a))
      (it "correctly categorizes <{([{{}}[<[[[<>{}]]]>[]] as not corrupted"
        (expect (day10/corrupted? (day10/read-blocks "<{([{{}}[<[[[<>{}]]]>[]]"))
                :to-be nil)))
    (it "replicates the example"
      (expect (day10/part-1 (advent/read-problem-lines 10 :example))
              :to-be 26397 ))
    (xit "solves the problem"
      (expect (day10/part-1 (advent/read-problem-lines 10 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day10/part-2 (advent/read-problem-lines 10 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day10/part-2 (advent/read-problem-lines 10 :problem))
              :to-be 42))))
