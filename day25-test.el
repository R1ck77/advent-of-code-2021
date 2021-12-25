(require 'day25)
(require 'buttercup)

(defconst demos '(
                  0
                  "v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>"

                  1
                  "....>.>v.>
v.v>.>v.v.
>v>>..>v..
>>v>v>.>.v
.>v.v...v.
v>>.>vvv..
..v...>>..
vv...>>vv.
>.v.v..v.v"

                  2
                  ">.v.v>>..v
v.v.>>vv..
>v>.>.>.v.
>>v>v.>v>.
.>..v....v
.>v>>.v.v.
v....v>v>.
.vv..>>v..
v>.....vv."

                  3
                  "v>v.v>.>v.
v...>>.v.v
>vv>.>v>..
>>v>v.>.v>
..>....v..
.>.>v>v..v
..v..v>vv>
v.v..>>v..
.v>....v.."

                  4
                  "v>..v.>>..
v.v.>.>.v.
>vv.>>.v>v
>>.>..v>.>
..v>v...v.
..>>.>vv..
>.v.vv>v.v
.....>>vv.
vvv>...v.."

                  5
                  "vv>...>v>.
v.v.v>.>v.
>.v.>.>.>v
>v>.>..v>>
..v>v.v...
..>.>>vvv.
.>...v>v..
..v.v>>v.v
v.v.>...v."

                  10
                  "..>..>>vv.
v.....>>.v
..v.v>>>v>
v>.>v.>>>.
..v>v.vv.v
.v.>>>.v..
v.v..>v>..
..v...>v.>
.vv..v>vv."

                  20
                  "v>.....>>.
>vv>.....v
.>v>v.vv>>
v>>>v.>v.>
....vv>v..
.v.>>>vvv.
..v..>>vv.
v.v...>>.v
..v.....v>"

                  30
                  ".vv.v..>>>
v>...v...>
>.v>.>vv.>
>v>.>.>v.>
.>..v.vv..
..v>..>>v.
....v>..>v
v.v...>vv>
v.v...>vvv"

                  40
                  ">>v>v..v..
..>>v..vv.
..>>>v.>.v
..>>>>vvv>
v.....>...
v.v...>v>>
>vv.....v>
.>v...v.>v
vvv.v..v.>"

                  50
                  "..>>v>vv.v
..v.>>vv..
v.>>v>>v..
..>>>>>vv.
vvv....>vv
..v....>>>
v>.......>
.vv>....v>
.>v.vv.v.."

                  55
                  "..>>v>vv..
..v.>>vv..
..>>v>>vv.
..>>>>>vv.
v......>vv
v>v....>>v
vvv...>..>
>vv.....>.
.>v.vv.v.."

                  56
                  "..>>v>vv..
..v.>>vv..
..>>v>>vv.
..>>>>>vv.
v......>vv
v>v....>>v
vvv....>.>
>vv......>
.>v.vv.v.."

                  57
                  "..>>v>vv..
..v.>>vv..
..>>v>>vv.
..>>>>>vv.
v......>vv
v>v....>>v
vvv.....>>
>vv......>
.>v.vv.v.."

                  58
                  "..>>v>vv..
..v.>>vv..
..>>v>>vv.
..>>>>>vv.
v......>vv
v>v....>>v
vvv.....>>
>vv......>
.>v.vv.v.."))

(describe "Day 25"
  (describe "part 1"
    (describe "example-steps"
      (it "replicates step 0"
        (expect (day25/to-string
                 (day25/test--evolve-n (day25/read-map (advent/read-grid 25 :example #'identity))
                                       0))
                :to-equal (plist-get demos 0)))
      (it "replicates step 1"
        (expect (day25/to-string
                 (day25/test--evolve-n (day25/read-map (advent/read-grid 25 :example #'identity))
                                       1))
                :to-equal (plist-get demos 1)))
      (it "replicates step 2"
        (expect (day25/to-string
                 (day25/test--evolve-n (day25/read-map (advent/read-grid 25 :example #'identity))
                                       2))
                :to-equal (plist-get demos 2)))
      (it "replicates step 3"
        (expect (day25/to-string
                 (day25/test--evolve-n (day25/read-map (advent/read-grid 25 :example #'identity))
                                       3))
                :to-equal (plist-get demos 3)))
      (it "replicates step 4"
        (expect (day25/to-string
                 (day25/test--evolve-n (day25/read-map (advent/read-grid 25 :example #'identity))
                                       4))
                :to-equal (plist-get demos 4)))
      (it "replicates step 5"
        (expect (day25/to-string
                 (day25/test--evolve-n (day25/read-map (advent/read-grid 25 :example #'identity))
                                       5))
                :to-equal (plist-get demos 5)))
      (it "replicates step 50"
        (expect (day25/to-string
                 (day25/test--evolve-n (day25/read-map (advent/read-grid 25 :example #'identity))
                                       50))
                :to-equal (plist-get demos 50))))
    (it "replicates the example"
      (expect (day25/part-1 (advent/read-grid 25 :example #'identity))
              :to-be 58))
    (it "solves the problem"
      (expect (day25/part-1 (advent/read-grid 25 :problem #'identity))
              :to-be 417)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day25/part-2 (advent/read-grid 25 :example #'identity))
              :to-be 42))
    (xit "solves the problem"
      (expect (day25/part-2 (advent/read-grid 25 :problem #'identity))
              :to-be 42))))
