(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core

module Math = struct
  let max_ints xs =
    match xs with
    | [] -> raise (Failure "max_ints: empty list")
    | x :: xs -> List.fold_left ~f:(fun acc x' -> max acc x') ~init:x xs
  ;;

  let min_ints xs =
    match xs with
    | [] -> raise (Failure "min_ints: empty list")
    | x :: xs -> List.fold_left ~f:(fun acc x' -> min acc x') ~init:x xs
  ;;

  let rec gcd a b : int = if b = 0 then a else gcd b (a mod b)

  let gcd_ints xs =
    match xs with
    | [] -> raise (Failure "gcd_ints: empty list")
    | x :: xs -> List.fold_left ~f:(fun acc x' -> gcd acc x') ~init:x xs
  ;;

  let lcm a b : int = a / gcd a b * b

  let lcm_ints xs =
    match xs with
    | [] -> raise (Failure "lcm_ints: empty list")
    | x :: xs -> List.fold_left ~f:(fun acc x' -> lcm acc x') ~init:x xs
  ;;

    (** extract all possible combination of k elements from a list
      reference: https://ocaml.org/learn/tutorials/99problems.html *)
  let gen_combinations (k : int) list =
    let rec aux k acc emit = function
      | [] -> acc
      | h :: t ->
        if k = 1
        then aux k (emit [ h ] acc) emit t
        else (
          let new_emit x = emit (h :: x) in
          aux k (aux (k - 1) acc new_emit t) emit t) in
    let emit x acc = x :: acc in
    aux k [] emit list
  ;;

  (** generate permutations of a list
      http://typeocaml.com/2015/05/05/permutation/ *)
  let gen_permutations list =
    let ins_all_positions x l =
      let rec aux prev acc = function
        | [] -> (prev @ [ x ]) :: acc |> List.rev
        | hd :: tl as l -> aux (prev @ [ hd ]) ((prev @ [ x ] @ l) :: acc) tl
      in
      aux [] [] l in
    let rec permutations = function
      | [] -> []
      | [ x ] -> [ [ x ] ] (* we must specify this edge case *)
      | x :: xs ->
        List.fold_left
          ~f:(fun acc p -> acc @ ins_all_positions x p)
          ~init:[]
          (permutations xs) in
    permutations list
  ;;

  (** extract all possible combinations with repetition
      of k element from a list:
      https://rosettacode.org/wiki/Combinations_with_repetitions#OCaml *)

  let gen_combinations_with_repetition (k : int) list =
    let rec gen_combs k xxs =
      match k, xxs with
      | 0, _ -> [ [] ]
      | _, [] -> []
      | k, x :: xs ->
        List.map ~f:(fun ys -> x :: ys) (gen_combs (k - 1) xxs) @ gen_combs k xs
    in
    gen_combs k list
  ;;

  let gen_permutations_with_repetition (k : int) list =
    let rec extract k list =
      if k <= 0
      then [ [] ]
      else
        List.fold_left
          ~f:(fun acc x ->
            let nlist = extract (k - 1) list in
            let nlist = List.map ~f:(fun l -> x :: l) nlist in
            acc @ nlist)
          ~init:[]
          list in
    extract k list
  ;;

  let gen_cartesian (lst : 'a list list) : 'a list list =
    let combine xs ys =
      xs
      |> List.map ~f:(fun x -> List.map ~f:(fun y -> x @ [ y ]) ys)
      |> List.concat in
    let rec gen acc lst =
      match lst with
      | [] -> acc
      | x :: xs -> gen (combine acc x) xs in
    match lst with
    | [] -> []
    | x :: xs -> gen (List.map ~f:(fun y -> [ y ]) x) xs
  ;;

  let gen_all_subsets (lst : 'a list) : 'a list list =
    List.fold_right
      ~f:(fun x rest -> rest @ List.map ~f:(fun ys -> x :: ys) rest)
      lst
      ~init:[ [] ]
  ;;
end
