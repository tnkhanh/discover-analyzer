(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core


(*******************************************************************
 * Extending library String with new functionalities
 *******************************************************************)

module String = struct

  (*--------------------------------
   * New functions handling string
   *-------------------------------*)

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


  (*-------------------------------------
   * Include the existing String library
   *------------------------------------*)

  include String

end
