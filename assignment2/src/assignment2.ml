open List

(******************************)
(*** For debugging purposes ***)
(******************************)

(* print out an integer list *)
let rec print_int_list lst =
  match lst with
  | [] -> ()
  | [x] -> print_int x; print_newline ()
  | x :: xs -> print_int x; print_string "; "; print_int_list xs 

(* print out a string list *)
let rec print_string_list lst =
  match lst with
  | [] -> ()
  | [x] -> print_string x; print_newline ()
  | x :: xs -> print_string x; print_string "; "; print_string_list xs 
                                                                       
(* print out a list of integer lists *)
let print_int_list_list lst =
  List.iter print_int_list lst

(* print out a list of string lists *)
let print_string_list_list lst =
  List.iter print_string_list lst


(***********************)
(* Problem 1: cond_dup *)
(***********************)

let rec cond_dup l f =
  match l with
    [] -> []
  | x::xs -> if (f x) then x::x::(cond_dup xs f) else x::(cond_dup xs f)

(**********************)
(* Problem 2: n_times *)
(**********************)

let rec n_times (f, n, v) =
  if n <= 0 then v
  else n_times(f, n-1, f v)

(**********************)
(* Problem 3: zipwith *)
(**********************)

let rec zipwith f l1 l2 =
  match l1 with
    [] -> []
  | x::xs -> match l2 with
      [] -> []
    | y::ys -> (f x y)::(zipwith f xs ys)

(**********************)
(* Problem 4: buckets *)
(**********************)

let buckets p l =
  let rec helperBuckets p l = 
    match l with 
      [] -> []
    | x::xs -> let lists = List.partition (p x) xs in
      match lists with
        (a, b) -> (x::a)::(helperBuckets p b)
  in helperBuckets p l


(**************************)
(* Problem 5: fib_tailrec *)
(**************************)

let fib_tailrec n =
  let rec fib_tailrec_helper a b n = 
    if n = 0 then a
    else fib_tailrec_helper b (a+b) (n-1)
  in fib_tailrec_helper 0 1 n

(***********************)
(* Problem 6: sum_rows *)
(***********************)

let sum_rows (rows:int list list) : int list =
  let new_rows = List.filter (fun x -> x != []) rows in
  let add_row a = List.fold_left (fun x y -> x + y) 0 a in
  List.map (add_row) new_rows

(*****************)
(* Problem 7: ap *)
(*****************)

let ap fs args =
  List.fold_left (fun x y -> x @ map y args) [] fs

(***********************)
(* Problem 8: prefixes *)
(***********************)

let prefixes l =
  let mapper a b = a::b in
  let folder x y = [x]::(List.map (mapper x) y) in
  List.fold_right (folder) l []

(***********************)
(* Problem 9: powerset *)
(***********************)

let powerset l = 
  let mapper a b = a::b in
  let folder x y = [x]::(List.map (mapper x) y) @ y in
  List.fold_right folder l []

(**************************)
(* Problem 10: assoc_list *)
(**************************)

let assoc_list lst =
  let dupe_folder x y = if List.mem y x then x else y::x in
  let no_dupes = fold_left dupe_folder [] lst in
  let tuple_mapper x = (x, 0) in
  let tuple_map = List.map tuple_mapper no_dupes in
  let final_map a b = (match b with (p, q) ->
    if p = a then (p, q+1) else (p, q)) in
  let final_fold x y = List.map (final_map y) x in
  fold_left final_fold tuple_map lst
  
(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =

  let error_count = ref 0 in

  let cmp x y = if x < y then (-1) else if x = y then 0 else 1 in

  (* Testcases for cond_dup *)
  let _ =
    try
      assert (cond_dup [3;4;5] (fun x -> x mod 2 = 1) = [3;3;4;5;5]);
      assert (cond_dup [] (fun x -> x mod 2 = 1) = []);
      assert (cond_dup [1;2;3;4;5] (fun x -> x mod 2 = 0) = [1;2;2;3;4;4;5])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for n_times *)
  let _ =
    try
      assert (n_times((fun x-> x+1), 50, 0) = 50);
      assert (n_times ((fun x->x+1), 0, 1) = 1);
      assert (n_times((fun x-> x+2), 50, 0) = 100)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for zipwith *)
  let _ =
    try
      assert ([5;7] = (zipwith (+) [1;2;3] [4;5]));
      assert ([(1,5); (2,6); (3,7)] = (zipwith (fun x y -> (x,y)) [1;2;3;4] [5;6;7]))
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for buckets *)
  let _ =
    try
      assert (buckets (=) [1;2;3;4] = [[1];[2];[3];[4]]);
      assert (buckets (=) [1;2;3;4;2;3;4;3;4] = [[1];[2;2];[3;3;3];[4;4;4]]);
      assert (buckets (fun x y -> (=) (x mod 3) (y mod 3)) [1;2;3;4;5;6] = [[1;4];[2;5];[3;6]])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for fib_tailrec *)
  let _ =
    try
      assert (fib_tailrec 50 = 12586269025);
      assert (fib_tailrec 90 = 2880067194370816120)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for sum_rows *)
  let _ =
    try
      assert (sum_rows [[1;2]; [3;4]] = [3; 7]);
      assert (sum_rows [[5;6;7;8;9]; [10]] = [35; 10])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for ap *)
  let _ =
    let x = [5;6;7;3] in
    let b = [3] in
    let c = [] in
    let fs1 = [((+) 2) ; (( * ) 7)] in
    try
      assert  ([7;8;9;5;35;42;49;21] = ap fs1 x);
      assert  ([5;21] = ap fs1 b);
      assert  ([] = ap fs1 c);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for prefixes *)
  let _ =
    try
      assert (prefixes [1;2;3;4] = [[1]; [1;2]; [1;2;3]; [1;2;3;4]]);
      assert (prefixes [] = []);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for powerset *) 
  let _ =
    try
      assert (List.sort cmp (powerset [1;2;3]) = List.sort cmp [[1]; [1; 2]; [1; 2; 3]; [1; 3]; [2]; [2; 3]; [3]]);
      assert ([] = powerset [])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in 

  (* Testcases for assoc_list *)
  let _ =
    let y = ["a";"a";"b";"a"] in
    let z = [1;7;7;1;5;2;7;7] in
    let a = [true;false;false;true;false;false;false] in
    let b = [] in
    try
      assert ([("a",3);("b",1)] = List.sort cmp (assoc_list y));
      assert ([(1,2);(2,1);(5,1);(7,4)] = List.sort cmp (assoc_list z));
      assert ([(false,5);(true,2)] = List.sort cmp (assoc_list a));
      assert ([] = assoc_list b)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  Printf.printf ("%d out of 10 programming questions passed.\n") (10 - !error_count)

let _ = main()
