(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core

(*******************************************************************
 * Extending library List with new functionalities
 *******************************************************************)

module List = struct
  (*------------------------------
   * New functions handling list
   *-----------------------------*)

  let not_empty (l : 'a list) : bool =
    match l with
    | [] -> false
    | _ -> true
  ;;

  let not_mem (l : 'a list) (e : 'a) ~equal : bool = not (List.mem l e ~equal)

  let is_inter (l1 : 'a list) (l2 : 'a list) ~equal : bool =
    List.exists ~f:(fun e -> List.mem l2 e ~equal) l1
  ;;

  let not_inter (l1 : 'a list) (l2 : 'a list) ~equal : bool =
    List.for_all ~f:(fun e -> not (List.mem l2 e ~equal)) l1
  ;;

  let is_subset (l1 : 'a list) (l2 : 'a list) ~equal : bool =
    List.for_all ~f:(fun e -> List.mem l2 e ~equal) l1
  ;;

  let not_subset (l1 : 'a list) (l2 : 'a list) ~equal : bool =
    List.exists ~f:(fun e -> not (List.mem l2 e ~equal)) l1
  ;;

  let exclude ~(f : 'a -> bool) l : 'a list =
    List.filter ~f:(fun x -> not (f x)) l
  ;;

  let extract_nth (n : int) (lst : 'a list) : ('a * 'a list) option =
    let rec extract i head tail =
      if List.is_empty tail || i > n
      then None
      else if i = n
      then Some (List.hd_exn tail, head @ List.tl_exn tail)
      else extract (i + 1) (head @ [ List.hd_exn tail ]) (List.tl_exn tail)
    in
    extract 0 [] lst
  ;;

  (* working with de-duplication *)
  (* FIXME: change _dedup functions by _sorting functions *)

  let insert_dedup (l : 'a list) (x : 'a) ~(equal : 'a -> 'a -> bool) =
    if List.mem l x ~equal then l else x :: l
  ;;

  let append_dedup (l : 'a list) (x : 'a) ~(equal : 'a -> 'a -> bool) =
    if List.mem l x ~equal then l else l @ [ x ]
  ;;

  let concat_dedup (l1 : 'a list) (l2 : 'a list) ~(equal : 'a -> 'a -> bool) =
    List.fold_right ~f:(fun x acc -> insert_dedup acc x ~equal) ~init:l2 l1
  ;;

  let dedup (l : 'a list) ~(equal : 'a -> 'a -> bool) =
    List.fold_right ~f:(fun x acc -> insert_dedup acc x ~equal) ~init:[] l
  ;;

  let diff (l1 : 'a list) (l2 : 'a list) ~(equal : 'a -> 'a -> bool) =
    List.fold_right
      ~f:(fun x acc -> if List.mem l2 x ~equal then acc else x :: acc)
      ~init:[]
      l1
  ;;

  let diff_dedup (l1 : 'a list) (l2 : 'a list) ~(equal : 'a -> 'a -> bool) =
    List.fold_right
      ~f:(fun x acc ->
        if List.mem l2 x ~equal then acc else insert_dedup acc x ~equal)
      ~init:[]
      l1
  ;;

  let remove (l : 'a list) (x : 'a) ~(equal : 'a -> 'a -> bool) =
    List.filter ~f:(fun u -> not (equal u x)) l
  ;;

  (* sorting *)

  let sorti ~(compare : 'a -> 'a -> int) (lst : 'a list) : 'a list =
    List.sort ~compare lst
  ;;

  let sortd ~(compare : 'a -> 'a -> int) (lst : 'a list) : 'a list =
    let compare x y = -compare x y in
    List.sort ~compare lst
  ;;

  let insert_sorti_dedup (l : 'a list) (x : 'a) ~(compare : 'a -> 'a -> int) =
    let rec insert lst res =
      match lst with
      | [] -> res @ [ x ]
      | u :: nlst ->
        let cmp = compare x u in
        if cmp < 0
        then res @ [ x ] @ lst
        else if cmp = 0
        then res @ lst
        else insert nlst (res @ [ u ]) in
    insert l []
  ;;

  let concat_sorti_dedup
      (l1 : 'a list)
      (l2 : 'a list)
      ~(compare : 'a -> 'a -> int)
    =
    let rec concat lst1 lst2 res =
      match lst1, lst2 with
      | [], _ -> res @ lst2
      | _, [] -> res @ lst1
      | u :: nlst1, v :: nlst2 ->
        let cmp = compare u v in
        if cmp < 0
        then concat nlst1 lst2 (res @ [ u ])
        else if cmp = 0
        then concat nlst1 nlst2 (res @ [ u ])
        else concat lst1 nlst2 (res @ [ v ]) in
    concat l1 l2 []
  ;;

  (* monadic support *)

  (* TODO: what is the most reasonable monadic version of List.exists? *)
  let exists_monad ~(f : 'a -> bool option) (l : 'a list) : bool option =
    let rec loop l =
      match l with
      | [] -> Some false
      | x :: l' ->
        (match f x with
        | Some true -> Some true
        | Some false -> loop l'
        | None ->
          (match loop l' with
          | Some true -> Some true
          | _ -> None)) in
    loop l
  ;;

  let filter_monad ~(f : 'a -> bool option) (l : 'a list) : 'a list =
    List.filter ~f:(fun x -> Option.value (f x) ~default:false) l
  ;;

  (* let fold_left_monad ~(f: 'a -> bool option) ~(init: 'a) (l: 'a list) : 'a list = *)
  (*   List.fold_left *)
  (*     ~f:(fun x -> Option.value (f x) ~default:false) *)
  (*     ~init l *)

  (*-----------------------------------
   * Include the original List module
   *----------------------------------*)

  include List
end
