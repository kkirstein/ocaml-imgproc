(* vim: set ft=ocaml sw=2 ts=2: *)

(**
   Imgops
   Module with image processing functions for the SVC2 camera test
   in Uster production
  
   Author: kayuwe.kirstein
  
*)

open Owl.Dense.Ndarray

val mean_std_line :
  ?orient:[< `Hor | `Ver > `Hor ] -> S.arr -> float array * float array
(** [mean_std_line orient img] calculates both the mean values and standard deviation
    of pixel values along the given orientation [orient]. *)

val centroid_line :
  ?orient:[< `Hor | `Ver > `Hor ] ->
  S.arr ->
  (S.arr, [> `Invalid_dimensions of int ]) result
(** [centroid_line ~orient img] calculates centroid values (1. moment) of
    pixel values along the given orientation [orient].
    Returns [`Invalid_dimensions] error, if [img] is not 2 dimensional. *)

val profile_line :
  ?orient:[< `Hor | `Ver > `Hor ] ->
  int ->
  S.arr ->
  (S.arr, [> `Invalid_dimensions of int | `Invalid_index of int ]) result
(** [profile_line ~orient coord img] extracts pixel values at [coord] long the given
    orientation [orient].
    Returns [`Invalid_dimensions] error, if [img] is not 2 dimensional. *)

val avg_line :
  ?orient:[< `Hor | `Ver > `Hor ] ->
  ?avg:int ->
  int ->
  S.arr ->
  (S.arr, [> `Invalid_dimensions of int | `Invalid_index of int ]) result
(** [avg_line ~orient ?avg coord img] extracts pixel values at [coord] along the given
    orientation [orient] and performs additional averaging of pixel values
    along the other dimension.
    Returns [`Invalid_dimensions] error, if [img] is not 2 dimensional. *)
