(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core

(*******************************************************************
 ** exceptions
 *******************************************************************)

exception EError of (string * string)

exception EBool of bool
exception EInt of int
exception EString of string
exception ESkip
exception EDone

let raise_bool b = raise (EBool b)
let raise_int i = raise (EInt i)


(*******************************************************************
 ** Basic
 *******************************************************************)

module Basic = struct

  (* TODO: replace Ternary by `bool option` to use monad *)
  type ternary =
    | True
    | False
    | Unkn

  exception ETern of ternary


  let pr_ternary = function
    | True -> "True"
    | False -> "False"
    | Unkn -> "Unkn"


  let negate_ternary tern =
    match tern with
    | True -> False
    | False -> True
    | Unkn -> Unkn


  let is_true (t: ternary) =
    t == True


  let is_false (t: ternary) =
    t == False


  let is_unknown (t: ternary) =
    t == Unkn

end;;


(*******************************************************************
 ** Extended List library
 *******************************************************************)

module List = struct


  let not_empty (l: 'a list) : bool =
    match l with
    | [] -> false
    | _ -> true


  let not_mem (l: 'a list) (e: 'a) ~equal : bool =
    not (List.mem l e ~equal)


  let is_inter (l1: 'a list) (l2: 'a list) ~equal : bool =
    List.exists ~f:(fun e -> List.mem l2 e ~equal) l1


  let not_inter (l1: 'a list) (l2: 'a list) ~equal : bool =
    List.for_all ~f:(fun e -> not (List.mem l2 e ~equal)) l1


  let is_subset (l1: 'a list) (l2: 'a list) ~equal : bool =
    List.for_all ~f:(fun e -> List.mem l2 e ~equal) l1


  let not_subset (l1: 'a list) (l2: 'a list) ~equal : bool =
    List.exists ~f:(fun e -> not (List.mem l2 e ~equal)) l1


  let exclude ~(f: 'a -> bool) l : 'a list =
    List.filter ~f:(fun x -> not (f x)) l


  let extract_nth (n: int) (lst: 'a list) : ('a * 'a list) option =
    let rec extract i head tail =
      if List.is_empty tail || i > n then None
      else if i = n then Some (List.hd_exn tail, head @ (List.tl_exn tail))
      else extract (i+1) (head @ [List.hd_exn tail]) (List.tl_exn tail) in
    extract 0 [] lst

  (* working with de-duplication *)
  (* FIXME: change _dedup functions by _sorting functions *)

  let insert_dedup (l: 'a list) (x: 'a) ~(equal: 'a -> 'a -> bool) =
    if List.mem l x ~equal then l
    else x::l


  let append_dedup (l: 'a list) (x: 'a) ~(equal: 'a -> 'a -> bool) =
    if List.mem l x ~equal then l
    else l @ [x]


  let concat_dedup (l1: 'a list) (l2: 'a list) ~(equal: 'a -> 'a -> bool) =
    List.fold_right ~f:(fun x acc ->
                         insert_dedup acc x ~equal) ~init:l2 l1


  let dedup (l: 'a list) ~(equal: 'a -> 'a -> bool) =
    List.fold_right ~f:(fun x acc ->
                         insert_dedup acc x ~equal) ~init:[] l


  let diff (l1: 'a list) (l2: 'a list) ~(equal: 'a -> 'a -> bool) =
    List.fold_right ~f:(fun x acc ->
                         if List.mem l2 x ~equal then acc
                         else x::acc) ~init:[] l1


  let diff_dedup (l1: 'a list) (l2: 'a list) ~(equal: 'a -> 'a -> bool) =
    List.fold_right ~f:(fun x acc ->
                         if List.mem l2 x ~equal then acc
                         else insert_dedup acc x ~equal) ~init:[] l1


  let remove (l: 'a list) (x: 'a) ~(equal: 'a -> 'a -> bool) =
    List.filter ~f:(fun u -> not (equal u x)) l

  (* sorting *)

  let sorti ~(compare: 'a -> 'a -> int) (lst: 'a list) : 'a list =
    List.sort ~compare lst


  let sortd ~(compare: 'a -> 'a -> int) (lst: 'a list) : 'a list =
    let compare x y = - (compare x y) in
    List.sort ~compare lst


  let insert_sorti_dedup (l: 'a list) (x: 'a) ~(compare: 'a -> 'a -> int) =
    let rec insert lst res = match lst with
      | [] -> res @ [x]
      | u::nlst ->
        let cmp = compare x u in
        if cmp < 0 then res @ [x] @ lst
        else if cmp = 0 then res @ lst
        else insert nlst (res @ [u]) in
    insert l []


  let concat_sorti_dedup (l1: 'a list) (l2: 'a list) ~(compare: 'a -> 'a -> int) =
    let rec concat lst1 lst2 res = match lst1, lst2 with
      | [], _ -> res @ lst2
      | _, [] -> res @ lst1
      | u::nlst1, v::nlst2 ->
        let cmp = compare u v in
        if cmp < 0 then concat nlst1 lst2 (res @ [u])
        else if cmp = 0 then concat nlst1 nlst2 (res @ [u])
        else concat lst1 nlst2 (res @ [v]) in
    concat l1 l2 []

  (* monadic support *)

  (* TODO: what is the most reasonable monadic version of List.exists? *)
  let exists_monad ~(f: 'a -> bool option) (l: 'a list) : bool option =
    let rec loop l =
      match l with
      | [] -> Some false
      | x::l' -> match f x with
        | Some true -> Some true
        | Some false -> loop l'
        | None -> match loop l' with
          | Some true -> Some true
          | _ -> None in
    loop l


  let filter_monad ~(f: 'a -> bool option) (l: 'a list) : 'a list =
    List.filter ~f:(fun x -> Option.value (f x) ~default:false) l

  (* let fold_left_monad ~(f: 'a -> bool option) ~(init: 'a) (l: 'a list) : 'a list = *)
  (*   List.fold_left *)
  (*     ~f:(fun x -> Option.value (f x) ~default:false) *)
  (*     ~init l *)

  (* include the original List module *)
  include List

end

(*******************************************************************
 ** Extended Hashtbl
 *******************************************************************)

module Hashtbl = struct


  let find_or_compute (tbl: ('a, 'b) Hashtbl.t) ~(key: 'a)
        ~(f: unit -> 'b) : 'b =
    match Hashtbl.find tbl key with
    | Some data -> data
    | None ->
      let data = f () in
      let _ = Hashtbl.set tbl ~key ~data in
      data


  let find_default (tbl: ('a, 'b) Hashtbl.t) (key: 'a) ~(default:'b) : 'b =
    match Hashtbl.find tbl key with
    | None -> default
    | Some v -> v


  (* include the original Hashtbl *)
  include Hashtbl

end

(*******************************************************************
 ** Extended String
 *******************************************************************)

module String = struct


  let not_empty (str: string) : bool =
    not (String.is_empty str)


  let is_infix ~(infix:string) (str: string) : bool =
    let idxs = String.substr_index_all ~may_overlap:false ~pattern:infix str in
    let len, sublen = String.length str, String.length infix in
    List.exists ~f:(fun idx -> (idx > 0) && (idx < len - sublen)) idxs


  let strip_newline (str: string) : string =
    String.strip ~drop:(fun c -> c == '\n') str


  let prefix_if_not_empty (s: string) ~(prefix: string) : string =
    if String.is_empty s then s
    else prefix ^ s


  let suffix_if_not_empty (s: string) ~(suffix: string) : string =
    if String.is_empty s then s
    else s ^ suffix


  let surround_if_not_empty (s: string) ~prefix ~suffix : string =
    if String.is_empty s then s
    else prefix ^ s ^ suffix


  let replace_if_empty (s: string) ~(replacer: string) : string =
    if String.is_empty s then replacer
    else s


  (* include the original String *)
  include String

end



(*******************************************************************
 ** Math
 *******************************************************************)

module Math = struct

  let max_ints xs = match xs with
    | [] -> raise (Failure "max_ints: empty list")
    | x::xs -> List.fold_left ~f:(fun acc x' -> max acc x') ~init:x xs

  let min_ints xs = match xs with
    | [] -> raise (Failure "min_ints: empty list")
    | x::xs -> List.fold_left ~f:(fun acc x' -> min acc x') ~init:x xs

  let rec gcd a b : int =
    if b = 0 then a
    else gcd b (a mod b)

  let gcd_ints xs = match xs with
    | [] -> raise (Failure "gcd_ints: empty list")
    | x::xs -> List.fold_left ~f:(fun acc x' -> gcd acc x') ~init:x xs

  let lcm a b: int =
    (a  / (gcd a b)) * b

  let lcm_ints xs = match xs with
    | [] -> raise (Failure "lcm_ints: empty list")
    | x::xs -> List.fold_left ~f:(fun acc x' -> lcm acc x') ~init:x xs

end;;

(*******************************************************************
 ** Combination
 *******************************************************************)

(** generic module for combination *)
module Comb = struct
  (** extract all possible combination of k elements from a list
      reference: https://ocaml.org/learn/tutorials/99problems.html *)
  let gen_combinations (k: int) list =
    let rec aux k acc emit = function
      | [] -> acc
      | h :: t ->
        if k = 1 then aux k (emit [h] acc) emit t else
          let new_emit x = emit (h :: x) in
          aux k (aux (k-1) acc new_emit t) emit t in
    let emit x acc = x :: acc in
    aux k [] emit list;;

  (** generate permutations of a list
      http://typeocaml.com/2015/05/05/permutation/ *)
  let gen_permutations list =
    let ins_all_positions x l =
      let rec aux prev acc = function
        | [] -> (prev @ [x]) :: acc |> List.rev
        | hd::tl as l -> aux (prev @ [hd]) ((prev @ [x] @ l) :: acc) tl in
      aux [] [] l in
    let rec permutations = function
      | [] -> []
      | x::[] -> [[x]] (* we must specify this edge case *)
      | x::xs ->
        List.fold_left ~f:(fun acc p ->
          acc @ ins_all_positions x p) ~init:[] (permutations xs) in
    permutations list

  (** extract all possible combinations with repetition
      of k element from a list:
      https://rosettacode.org/wiki/Combinations_with_repetitions#OCaml *)

  let gen_combinations_with_repetition (k: int) list =
    let rec gen_combs k xxs = match k, xxs with
      | 0,  _ -> [[]]
      | _, [] -> []
      | k, x::xs ->
        (List.map ~f:(fun ys -> x::ys) (gen_combs (k-1) xxs)) @ gen_combs k xs in
    gen_combs k list

  let gen_permutations_with_repetition (k: int) list =
    let rec extract k list =
      if (k <= 0) then [[]]
      else
        List.fold_left ~f:(fun acc x ->
          let nlist = (extract (k-1) list) in
          let nlist = List.map ~f:(fun l -> x::l) nlist in
          acc@nlist) ~init:[] list in
    extract k list

  let gen_cartesian (lst: 'a list list) : 'a list list =
    let combine xs ys =
      xs |>
      List.map ~f:(fun x -> List.map ~f:(fun y -> x @ [y]) ys) |>
      List.concat in
    let rec gen acc lst =
      match lst with
      | [] -> acc
      | x::xs -> gen (combine acc x) xs in
    match lst with
    | [] -> []
    | x::xs -> gen (List.map ~f:(fun y -> [y]) x) xs

  let gen_all_subsets (lst: 'a list) : 'a list list =
    List.fold_right ~f:(fun x rest ->
      rest @ List.map ~f:(fun ys -> x::ys) rest) lst ~init:[[]]

end;;


(*******************************************************************
 ** System
 *******************************************************************)

module System = struct

  let track_runtime (f : unit -> 'a) : ('a * float) =
    let time_begin = Unix.gettimeofday () in
    let res = f () in
    let time_end = Unix.gettimeofday () in
    let time = time_end -. time_begin in
    (res, time)

  let report_runtime ?(task="") (f : unit -> 'a)  : 'a =
    let res =
      if String.is_empty task then f ()
      else
        let time_begin = Unix.gettimeofday () in
        let res = f () in
        let time_end = Unix.gettimeofday () in
        let time = time_end -. time_begin in
        let _ = print_endline (task ^ ": " ^ (sprintf "%.3fs" time)) in
        res in
    res

  let record_runtime (f : unit -> 'a) (time: float ref) : 'a =
    let time_begin = Unix.gettimeofday () in
    let res = f () in
    let time_end = Unix.gettimeofday () in
    time := !time +. (time_end -. time_begin);
    res

  let remove_file_if_exists (filename: string) : unit =
    match Sys.file_exists filename with
    | `Yes -> Sys.remove filename
    | _ -> ()

  let mkdir_if_not_exists (dirname: string) : unit =
    let _ = match Sys.file_exists dirname with
      | `No -> Sys.command ("mkdir " ^ dirname)
      | _ -> match Sys.is_directory dirname with
        | `No ->
          let _ = print_endline ("'" ^ dirname ^ "' is an regular file. " ^
                                 "Discover needs to use this name to work.\n" ^
                                 "Please remove or rename it to continue.\n") in
          exit 0
        | _ -> 0 in
    ()

end;;


(*******************************************************************
 ** Finally, include all necessary libraries
 *******************************************************************)

include Basic
include Math
include System
