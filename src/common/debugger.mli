(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

val mode_debug : bool ref
val mode_deep_debug : bool ref
val no_debug : bool ref
val mode_interactive : bool ref
val mode_interactive_prover : bool ref
val mode_interactive_verifier : bool ref
val saved_mode_debug : bool ref
val saved_mode_deep_debug : bool ref
val regex_debug_function : string ref
val mode_debug_function : bool ref
val regex_debug_working_function : string ref
val mode_debug_working_function : bool ref
val is_debug_mode : unit -> bool
val is_interactive_mode : unit -> bool
val enable_mode_debug : unit -> unit
val disable_mode_debug : unit -> unit
val enable_mode_deep_debug : unit -> unit
val save_mode_debug : unit -> unit
val restore_mode_debug : unit -> unit

(*** shallow debugging ***)

val debug
  :  ?header:bool ->
  ?ruler:[< `Long | `Medium | `None | `Short > `None ] ->
  ?indent:int ->
  ?always:bool ->
  ?enable:bool ->
  ?marker:bool ->
  string ->
  unit

val hdebug
  :  ?header:bool ->
  ?ruler:[< `Long | `Medium | `None | `Short > `None ] ->
  ?indent:int ->
  ?always:bool ->
  ?enable:bool ->
  ?marker:bool ->
  string ->
  ('a -> string) ->
  'a ->
  unit

(*** deep debugging ***)

val ddebug
  :  ?header:bool ->
  ?ruler:[< `Long | `Medium | `None | `Short > `None ] ->
  ?indent:int ->
  ?always:bool ->
  ?enable:bool ->
  ?marker:bool ->
  string ->
  unit

val hddebug
  :  ?header:bool ->
  ?ruler:[< `Long | `Medium | `None | `Short > `None ] ->
  ?indent:int ->
  ?always:bool ->
  ?enable:bool ->
  ?marker:bool ->
  string ->
  ('a -> string) ->
  'a ->
  unit

(*** disable debugging ***)

val ndebug : 'a -> unit
val nhdebug : 'a -> 'b -> 'c -> unit
val nhddebug : 'a -> 'b -> 'c -> unit

(*** interactive debugging ***)

val display_choices : string -> ('a -> string) -> 'a list -> string
val ask_decision : string -> string list -> Base.string
val nask_decision : 'a -> 'b -> unit
