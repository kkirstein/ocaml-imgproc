(* vim: set ft=ocaml sw=2 ts=2: *)

(**
    Ops
    Module with some basic image processing functions
    
    @deprecated These function will go to dedicated modules
    in future releases.
    
    Author: Kay-Uwe Kirstein <kay-uwe@kirsteinhome.ch>
  
*)

module type Ops = sig
  type ary_type

  val centroid_line :
    ?orient:[< `Hor | `Ver > `Hor ] ->
    ary_type ->
    (ary_type, [> `Invalid_dimensions of int ]) result
  (** [centroid_line ~orient img] calculates centroid values (1. moment) of
      pixel values along the given orientation [orient].
      Returns [`Invalid_dimensions] error, if [img] is not 2 dimensional. *)

  val profile_line :
    ?orient:[< `Hor | `Ver > `Hor ] ->
    int ->
    ary_type ->
    (ary_type, [> `Invalid_dimensions of int | `Invalid_index of int ]) result
  (** [profile_line ~orient coord img] extracts pixel values at [coord] long the given
      orientation [orient].
      Returns [`Invalid_dimensions] error, if [img] is not 2 dimensional. *)

  val avg_line :
    ?orient:[< `Hor | `Ver > `Hor ] ->
    ?avg:int ->
    int ->
    ary_type ->
    (ary_type, [> `Invalid_dimensions of int | `Invalid_index of int ]) result
  (** [avg_line ~orient ?avg coord img] extracts pixel values at [coord] along the given
      orientation [orient] and performs additional averaging of pixel values
      along the other dimension.
      Returns [`Invalid_dimensions] error, if [img] is not 2 dimensional. *)
end

module S : Ops with type ary_type := Owl.Dense.Ndarray.S.arr

module D : Ops with type ary_type := Owl.Dense.Ndarray.D.arr
