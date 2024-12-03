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

    | _ -> Printf.printf "Error: Day not found \n"
  )
  | _ -> Printf.printf "Error: Invalid argumentes. Usage: command [:specify day:] \n"
;;

