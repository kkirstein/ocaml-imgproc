(* vim: set ft=ocaml sw=2 ts=2: *)

(**
    Ndarray
    
    Module type definition for generic image processing
    for different Owl.Dense.Ndarray data types

*)

module type Ndarray = sig
  type arr

  type elt = float

  (* creation *)
  val init : int array -> (int -> elt) -> arr

  (* shape info & manipulation *)
  val shape : arr -> int array

  val num_dims : arr -> int

  val reshape : arr -> int array -> arr

  val transpose : ?axis:int array -> arr -> arr

  val split : ?axis:int -> int array -> arr -> arr array

  val get_slice : int list list -> arr -> arr

  (* conversion *)
  val to_array : arr -> elt array

  val of_array : elt array -> int array -> arr

  (* iterators *)
  val map_slice : ?axis:int -> (arr -> 'c) -> arr -> 'c array

  val iteri : (int -> elt -> unit) -> arr -> unit

  (* math *)
  val mean : ?axis:int -> ?keep_dims:bool -> arr -> arr

  val ( + ) : arr -> arr -> arr

  val ( *$ ) : arr -> elt -> arr
end
