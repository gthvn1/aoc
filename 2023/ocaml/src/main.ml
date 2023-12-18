let solve_day name part1 part2 : unit =
  let filename = String.concat "/" [ "input"; name ] in
  let lines = Aoc_libs.Aoc.string_from_input filename in
  let answer1 = part1 lines in
  let answer2 = part2 lines in

  print_endline @@ String.concat " " [ name; "part1:"; answer1 ];
  print_endline @@ String.concat " " [ name; "part2:"; answer2 ]

let () =
  solve_day "day01" Aoc_libs.Day01.part1 Aoc_libs.Day01.part2;
  solve_day "day02" Aoc_libs.Day02.part1 Aoc_libs.Day02.part2;
  solve_day "day03" Aoc_libs.Day03.part1 Aoc_libs.Day03.part2;
  solve_day "day04" Aoc_libs.Day04.part1 Aoc_libs.Day04.part2;
  solve_day "day05" Aoc_libs.Day05.part1 Aoc_libs.Day05.part2;
  solve_day "day06" Aoc_libs.Day06.part1 Aoc_libs.Day06.part2;
  solve_day "day07" Aoc_libs.Day07.part1 Aoc_libs.Day07.part2;
  solve_day "day08" Aoc_libs.Day08.part1 Aoc_libs.Day08.part2;
  solve_day "day09" Aoc_libs.Day09.part1 Aoc_libs.Day09.part2;
  solve_day "day10" Aoc_libs.Day10.part1 Aoc_libs.Day10.part2;
  solve_day "day11" Aoc_libs.Day11.part1 Aoc_libs.Day11.part2;
  solve_day "day12" Aoc_libs.Day12.part1 Aoc_libs.Day12.part2
