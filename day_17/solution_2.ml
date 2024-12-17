let file = "./day_17/input/input";;

type registersT = {
  mutable a : int;
  mutable b : int;
  mutable c : int;
}

let get_registers str =
  let parts = List.filter (fun str -> String.length str > 0) (Lib.splitrim '\n' str) in
  let n = List.map (fun s -> List.nth (Lib.splitrim ' ' s) 2) (parts) in
  match n with
  | a::b::c::_ -> {
    a = int_of_string a;
    b = int_of_string b;
    c = int_of_string c;}
  | _ -> failwith "Oh noooo"
;;

let get_3_lowest_bits n =
  n land 0b111
;;

let rec pow x n =
  if n < 0 then invalid_arg "Negative exponent not supported"
  else if n = 0 then 1
  else if n mod 2 = 0 then let half = pow x (n / 2) in half * half
  else x * pow x (n - 1)
;;

let get_program str =
  let parts = String.split_on_char ' ' str in
  let nums  = Lib.splitrim ',' (List.nth parts 1) in
  List.map (int_of_string) nums
;;

let get_val registers opcode =
  match opcode with
  | 0 | 1 | 2 | 3 as v -> v
  | 4 -> registers.a
  | 5 -> registers.b
  | 6 -> registers.c
  | o -> failwith (Printf.sprintf "invalid operand %d" o)
;;

let evaluate registers (opcode, operand) pointer out =
  match opcode with
  | 0 -> (let res = registers.a / (pow 2 (get_val registers operand)) in
         registers.a <- res;
         pointer := !pointer + 2)
  | 1 -> (let res = registers.b lxor operand in
         registers.b <- res;
         pointer := !pointer + 2)
  | 2 -> (let res = get_3_lowest_bits((get_val registers operand) mod 8) in
          registers.b <- res;
          pointer := !pointer + 2)
  | 3 -> (if registers.a != 0 then pointer := operand else pointer := !pointer + 2)
  | 4 -> (let res = registers.b lxor registers.c in
         registers.b <- res;
         pointer := !pointer + 2)
  | 5 -> (let res = (get_val registers operand) mod 8 in
         out := res::!out;
         pointer := !pointer + 2)
  | 6 -> (let res = registers.a / (pow 2 (get_val registers operand)) in
         registers.b <- res;
         pointer := !pointer + 2)
  | 7 -> (let res = registers.a / (pow 2 (get_val registers operand)) in
         registers.c <- res;
         pointer := !pointer + 2)
  | _ -> failwith "Oh no"
;;

let rec take n lst =
  match n, lst with
  | 0, _ | _, [] -> []
  | n, x :: xs -> x :: take (n - 1) xs
;;

let take_tail n lst =
  List.rev (take n (List.rev lst))
;;

let compare_tail_l lst1 lst2 n =
  let rev1 = List.rev lst1 in
  let rev2 = List.rev lst2 in
  let sub1 = take n rev1 in
  let sub2 = take n rev2 in
  sub1 = sub2
;;

let join_int str lst =
  String.concat str (List.map (string_of_int) lst)
;;

let run (registers, program) =
  let len = List.length program in
  let result = ref 0 in
  let rec loop prog t =
    let pointer = ref 0 in
    let out = ref [] in
    let len = List.length prog in
    while !pointer < (len - 1) do
      evaluate registers (List.nth prog !pointer, List.nth prog (!pointer + 1)) pointer out
    done;
    if join_int "," (List.rev !out) = t then
      if List.length !out = List.length program then
        true
      else
        false
    else (
          incr result;
          registers.a <- !result;
          loop program t
        )
  in
  for i = 1 to len do
    let tail = join_int "," (take_tail i program) in
    flush_all();
    let f = loop program tail in
    if not f then(
      result := !result * 8;
      registers.a <- !result;)
  done;

  !result
;;


let get_corrupt_val f =
  In_channel.with_open_bin f In_channel.input_all
  |> Str.split (Str.regexp "\n\n")
  |> fun arr -> (get_registers (List.hd arr), get_program (List.nth arr 1))
  |> run
;;

let result () = get_corrupt_val file;

