(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)


(*** Reporting warning  ***)

val disable_warning : bool ref

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
