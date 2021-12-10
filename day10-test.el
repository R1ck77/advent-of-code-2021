(require 'day10)
(require 'buttercup)

(describe "Day 10"
  (describe "part 1"
    (describe "single instruction categorization"
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
    (it "solves the problem"
      (expect (day10/part-1 (advent/read-problem-lines 10 :problem))
              :to-be 167379)))
  (describe "part 2"
    (describe "single instruction completion"
      (it "doesn't complete <{([([[(<>()){}]>(<<{{ as it's corrupted"        
        (expect (day10/missing-brackets (day10/read-blocks "<{([([[(<>()){}]>(<<{{"))
                :to-be nil))
      (it "completes (((({<>}<{<{<>}{[]{[]{} correctly"
        (expect (day10/missing-brackets (day10/read-blocks "(((({<>}<{<{<>}{[]{[]{}"))
                :to-equal '(:-c :-c :-a :-c :-a :-r :-r :-r :-r)))
      (it "completes {<[[]]>}<{[{[{[]{()[[[] correctly"
        (expect (day10/missing-brackets (day10/read-blocks "{<[[]]>}<{[{[{[]{()[[[]"))
                :to-equal '(:-s :-s :-c :-c :-s :-c :-s :-c :-a)))
      (it "completes <{([{{}}[<[[[<>{}]]]>[]] correctly"
        (expect (day10/missing-brackets (day10/read-blocks "<{([{{}}[<[[[<>{}]]]>[]]"))
                :to-equal '(:-s :-r :-c :-a)))      
      (it "completes [({(<(())[]>[[{[]{<()<>> correctly"
        (expect (day10/missing-brackets (day10/read-blocks "[({(<(())[]>[[{[]{<()<>>"))
                :to-equal '(:-c :-c :-s :-s :-r :-c :-r :-s)))
      (it "completes [(()[<>])]({[<{<<[]>>( correctly"
        (expect (day10/missing-brackets (day10/read-blocks "[(()[<>])]({[<{<<[]>>("))
                :to-equal '(:-r :-c :-a :-s :-c :-r))))
    (describe "scoring"
      (it "scores }}]])})] correctly"
        (expect (day10/score-missing-brackets '(:-c :-c :-s :-s :-r :-c :-r :-s))
                :to-be 288957))
      (it "scores )}>]}) correctly"
        (expect (day10/score-missing-brackets '(:-r :-c :-a :-s :-c :-r))
                :to-be 5566))
      (it "scores }}>}>)))) correctly"
        (expect (day10/score-missing-brackets '(:-c :-c :-a :-c :-a :-r :-r :-r :-r))
                :to-be 1480781))
      (it "scores ]]}}]}]}> correctly"
        (expect (day10/score-missing-brackets '(:-s :-s :-c :-c :-s :-c :-s :-c :-a))
                :to-be 995444))
      (it "scores ])}> correctly"
        (expect (day10/score-missing-brackets '(:-s :-r :-c :-a))
                :to-be 294)))
    (it "replicates the example"
      (expect (day10/part-2 (advent/read-problem-lines 10 :example))
              :to-be 288957))
    (it "solves the problem"
      (expect (day10/part-2 (advent/read-problem-lines 10 :problem))
              :to-be 2776842859 ))))
