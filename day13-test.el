(require 'day13)
(require 'buttercup)

(describe "--- Day 13: Transparent Origami ---"
  (describe "part 1"
    (describe "vertical fold works"
      (it "replicates the first fold"
       (expect (day13/format-paper (day13/fold (day13/read-state (advent/read-blocks-of-lines 13 :example))))
               :to-equal "#.##..#..#.
#...#......
......#...#
#...#......
.#.#..#.###
...........
...........
")))
    (it "replicates the example"
      (expect (day13/part-1 (advent/read-blocks-of-lines 13 :example))
              :to-be 17 ))
    (it "solves the problem"
      (expect (day13/part-1 (advent/read-blocks-of-lines 13 :problem))
              :to-be 621)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day13/part-2 (advent/read-blocks-of-lines 13 :example))
              :to-equal "#####
#...#
#...#
#...#
#####
.....
.....
"))
    (it "solves the problem"
      (expect (day13/part-2 (advent/read-blocks-of-lines 13 :problem))
              :to-equal "#..#.#..#.#..#...##..##...##....##.####.
#..#.#.#..#..#....#.#..#.#..#....#....#.
####.##...#..#....#.#....#..#....#...#..
#..#.#.#..#..#....#.#.##.####....#..#...
#..#.#.#..#..#.#..#.#..#.#..#.#..#.#....
#..#.#..#..##...##...###.#..#..##..####.
"))))
