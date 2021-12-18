(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

val no_print : bool ref
val pr_bool : bool -> string
val pr_float : float -> string
val pr_int : int -> string
val pr_int64 : int64 -> string
val pr_str : string -> string
val pr_option : f:('a -> string) -> 'a option -> string
val pr_bool_option : bool option -> string

val pr_list
  :  ?sep:string ->
  ?obrace:string ->
  ?cbrace:string ->
  ?indent:string ->
  ?extra:string ->
  f:('a -> string) ->
  'a list ->
  string

val pr_list_square
  :  ?sep:string ->
  ?indent:string ->
  ?extra:string ->
  f:('a -> string) ->
  'a list ->
  string

val pr_list_curly
  :  ?sep:string ->
  ?indent:string ->
  ?extra:string ->
  f:('a -> string) ->
  'a list ->
  string

val pr_list_paren
  :  ?sep:string ->
  ?indent:string ->
  ?extra:string ->
  f:('a -> string) ->
  'a list ->
  string

val pr_list_plain
  :  ?sep:string ->
  ?indent:string ->
  ?extra:string ->
  f:('a -> string) ->
  'a list ->
  string

val pr_items
  :  ?bullet:string ->
  ?obrace:string ->
  ?cbrace:string ->
  ?extra:string ->
  f:('a -> string) ->
  'a list ->
  string

val pr_args : f:('a -> string) -> 'a list -> string
val pr_pair : f1:('a -> string) -> f2:('b -> string) -> 'a * 'b -> string

(*** Formatting strings *)

val beautiful_concat : ?column:int -> sep:string -> string list -> string
val beautiful_format_on_char : sep:char -> ?column:int -> string -> string

(*** Printing ***)

val print
  :  ?header:bool ->
  ?ruler:[< `Long | `Medium | `None | `Short > `None ] ->
  ?indent:int ->
  ?always:bool ->
  ?enable:bool ->
  ?marker:bool ->
  ?autoformat:bool ->
  string ->
  unit

val println
  :  ?header:bool ->
  ?ruler:[< `Long | `Medium | `None | `Short > `None ] ->
  ?indent:int ->
  ?always:bool ->
  ?enable:bool ->
  ?marker:bool ->
  ?autoformat:bool ->
  string ->
  unit

val hprint
  :  ?header:bool ->
  ?ruler:[< `Long | `Medium | `None | `Short > `None ] ->
  ?indent:int ->
  ?always:bool ->
  ?enable:bool ->
  ?marker:bool ->
  ?autoformat:bool ->
  string ->
  ('a -> string) ->
  'a ->
  unit

(*** Disable printing ***)

val nprint : 'a -> unit
val nprintln : 'a -> unit
val nhprint : 'a -> 'b -> unit

(*** Reporting warning  ***)

val warning : string -> unit
val hwarning : string -> ('a -> string) -> 'a -> unit

(** Report warning using format template similar to [printf] *)
val warningf
  :  ('a, out_channel, unit, unit, unit, unit) CamlinternalFormatBasics.format6 ->
  'a

(*** Reporting errors ***)

val error : ?log:string -> string -> 't
val herror : ?log:string -> string -> ('a -> string) -> 'a -> 't

(** Report error using format template similar to [printf] *)
val errorf
  :  ?log:string ->
  ('a, out_channel, unit, unit, unit, unit) CamlinternalFormatBasics.format6 ->
  'a

(*** Wrapping OCam printing functions ***)
val sprintf : ('a, unit, string) format -> 'a
val print_endline : string -> unit
val print_string : string -> unit
