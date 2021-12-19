(require 'day19)
(require 'buttercup)

(defconst day19-test/example (day19/read-scans (advent/read-blocks-of-lines 19 :example)))

(describe "Day 19"
  (describe "part 1"
    (describe "scan pairs detection"
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
      (it "can detect the pairing of 2 and 3"
        (expect (day19/two-scanners-match? (elt day19-test/example 2)
                                           (elt day19-test/example 3))
               :not :to-be nil)
        (expect (day19/two-scanners-match? (elt day19-test/example 3)
                                           (elt day19-test/example 2))
               :not :to-be nil))      
      (it "can detect the lack of pairing between 0 and 4"
        (expect (day19/two-scanners-match? (elt day19-test/example 0)
                                           (elt day19-test/example 4))
               :to-be nil)
        (expect (day19/two-scanners-match? (elt day19-test/example 4)
                                           (elt day19-test/example 0))
                :to-be nil))
      (it "can detect the lack of pairing between 1 and 3"
        (expect (day19/two-scanners-match? (elt day19-test/example 1)
                                           (elt day19-test/example 3))
               :to-be nil)
        (expect (day19/two-scanners-match? (elt day19-test/example 3)
                                           (elt day19-test/example 1))
                :to-be nil))
      (it "can detect the lack of pairing between 4 and 3"
        (expect (day19/two-scanners-match? (elt day19-test/example 4)
                                           (elt day19-test/example 3))
               :to-be nil)
        (expect (day19/two-scanners-match? (elt day19-test/example 3)
                                           (elt day19-test/example 4))
               :to-be nil)))
    (xdescribe "list of beacons"
      (it "can build the correct list"
        (expect (sort (day19/build-list (advent/read-blocks-of-lines 19 :example))
                      (lambda (a b) (< (car a) (car b))))
                :to-equal '((-892 524 684) (-876 649 763) (-838 591 734) (-789 900 -551)
                            (-739 -1745 668) (-706 -3180 -659) (-697 -3072 -689) (-689 845 -530)
                            (-687 -1600 576) (-661 -816 -575) (-654 -3158 -753) (-635 -1737 486)
                            (-631 -672 1502) (-624 -1620 1868) (-620 -3212 371) (-618 -824 -621)
                            (-612 -1695 1788) (-601 -1648 -643) (-584 868 -557) (-537 -823 -458)
                            (-532 -1715 1894) (-518 -1681 -600) (-499 -1607 -770) (-485 -357 347)
                            (-470 -3283 303) (-456 -621 1527) (-447 -329 318) (-430 -3130 366)
                            (-413 -627 1469) (-345 -311 381) (-36 -1284 1171) (-27 -1108 -65)
                            (7 -33 -71) (12 -2351 -103) (26 -1119 1091) (346 -2985 342)
                            (366 -3059 397) (377 -2827 367) (390 -675 -793) (396 -1931 -563)
                            (404 -588 -901) (408 -1815 803) (423 -701 434) (432 -2009 850)
                            (443 580 662) (455 729 728) (456 -540 1869) (459 -707 401)
                            (465 -695 1988) (474 580 667) (496 -1584 1900) (497 -1838 -617)
                            (527 -524 1933) (528 -643 409) (534 -1912 768) (544 -627 -890)
                            (553 345 -567) (564 392 -477) (568 -2007 -577) (605 -1665 1952)
                            (612 -1593 1893) (630 319 -379) (686 -3108 -505) (776 -3184 -501)
                            (846 -3110 -434) (1135 -1161 1235) (1243 -1093 1063) (1660 -552 429)
                            (1693 -557 386) (1735 -437 1738) (1749 -1800 1813) (1772 -405 1572)
                            (1776 -675 371) (1779 -442 1789) (1780 -1548 337) (1786 -1538 337)
                            (1847 -1591 415) (1889 -1729 1762) (1994 -1805 1792)))))
    (xit "replicates the example"
      (expect (day19/part-1 (advent/read-blocks-of-lines 19 :example))
              :to-be 79 ))
    (xit "solves the problem"
      (expect (day19/part-1 (advent/read-blocks-of-lines 19 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day19/part-2 (advent/read-blocks-of-lines 19 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day19/part-2 (advent/read-blocks-of-lines 19 :problem))
              :to-be 42))))
