(* vim: set ft=ocaml sw=2 ts=2: *)

(**
    Ndarray
    
    Module type definition for generic image processing
    for different Owl.Dense.Ndarray data types

*)

module type Ndarray = sig
  type elt = float

  (* include module signature of Owl.Dense.Ndarray *)
  include Owl_dense_ndarray_intf.Common with type elt := float

  (* operators *)
  val ( + ) : arr -> arr -> arr

  val ( *$ ) : arr -> elt -> arr
end
