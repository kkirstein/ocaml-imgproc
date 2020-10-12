(* vim: set ft=ocaml sw=2 ts=2: *)

(**
    Color module offers conversion functions between
    different color spaces, e.g. RGB, HSV or Grayscale
    based on Owl's Ndarray.
    
    Author: Kay-Uwe Kirstein <kay-uwe@kirsteinhome.ch>
  
  *)

include Ndarray_intf

module type Color = sig
  type ary_type

  val to_gray : ary_type -> (ary_type, [> `Invalid_dimension of int ]) result

  val to_gray' : ary_type -> (ary_type, [> `Invalid_dimension of int ]) result
end

module Make (A : Ndarray) : Color with type ary_type := A.arr = struct
  let to_gray img =
    let dims = A.shape img in
    match Array.length dims with
    | 2 -> Ok img
    | 3 ->
        let chans = A.split ~axis:2 [| 1; 1; 1 |] img in
        let r, g, b = (chans.(0), chans.(1), chans.(2)) in
        Ok A.((r *$ 0.2125) + (g *$ 0.7154) + (b *$ 0.0721))
    | x -> Error (`Invalid_dimension x)

  let to_gray' img =
    let dims = A.shape img in
    let gray_conv = to_gray img in
    Result.bind gray_conv (fun i -> Ok (A.reshape i [| dims.(0); dims.(1) |]))
end

module S = Make (Owl.Dense.Ndarray.S)
module D = Make (Owl.Dense.Ndarray.D)
