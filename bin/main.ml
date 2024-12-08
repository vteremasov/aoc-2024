let () =
  match Sys.argv with
  | [||] | [|_|] -> Printf.printf "Error: Specify day\n"
  | [|_;d|] -> (
    Printf.printf "Result for %s: \n" d;
    match d with
    | "day1" -> 
        Printf.printf " - Solution 1: %d\n" (Day1.Solution_1.result());
        Printf.printf " - Solution 2: %d\n" (Day1.Solution_2.result());
    | "day2" -> 
        Printf.printf " - Solution 1: %d\n" (Day2.Solution_1.result());
        Printf.printf " - Solution 2: %d\n" (Day2.Solution_2.result());
    | "day3" -> 
        Printf.printf " - Solution 1: %d\n" (Day3.Solution_1.result());
        Printf.printf " - Solution 2: %d\n" (Day3.Solution_2.result());
    | "day4" -> 
        Printf.printf " - Solution 1: %d\n" (Day4.Solution_1.result());
        Printf.printf " - Solution 2: %d\n" (Day4.Solution_2.result());
    | "day5" -> 
        Printf.printf " - Solution 1: %d\n" (Day5.Solution_1.result());
        Printf.printf " - Solution 2: %d\n" (Day5.Solution_2.result());
    | "day6" -> 
        Printf.printf " - Solution 1: %d\n" (Day6.Solution_1.result());
        Printf.printf " - Solution 2: %d\n" (Day6.Solution_2.result());
    | "day7" -> 
        Printf.printf " - Solution 1: %d\n" (Day7.Solution_1.result());
        Printf.printf " - Solution 2: %d\n" (Day7.Solution_2.result());
    | "day8" -> 
        Printf.printf " - Solution 1: %d\n" (Day8.Solution_1.result());
        Printf.printf " - Solution 2: %d\n" (Day8.Solution_2.result());
 
    | _ -> Printf.printf "Error: Day not found \n"
  )
  | _ -> Printf.printf "Error: Invalid argumentes. Usage: command [:specify day:] \n"
;;

