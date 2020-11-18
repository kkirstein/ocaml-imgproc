(* vim: set ft=ocaml sw=2 ts=2: *)

(**
    Color module offers conversion functions between
    different color spaces, e.g. RGB, HSV or Grayscale
    based on Owl's Ndarray.
    
    Author: Kay-Uwe Kirstein <kay-uwe@kirsteinhome.ch>
  
  *)

module type Color = sig
  type ary_type

  val rgb2gray : ary_type -> (ary_type, [> `Invalid_dimension of int ]) result
  (** [to_gray nd] converts given image data to grayscale. It works for
      image data with 3 color channels and returns the original image, if a single
      color channel is given. Otherwise an [`Invalid_dimension n] error is returned. *)

  val rgb2gray' : ary_type -> (ary_type, [> `Invalid_dimension of int ]) result
  (** [to_gray' nd] like [to_gray], but reduces the number of dimensions and
      returns a 2D-matrix. See {!to_gray} for more details. *)
end

module S : Color with type ary_type := Owl.Dense.Ndarray.S.arr
(** Implementation of Color, based on single precision data type.
    For documentation of the individual function see {!type: Color}. *)

module D : Color with type ary_type := Owl.Dense.Ndarray.D.arr
(** Implementation of Color, based on double precision data type.
    For documentation of the individual function see {!type: Color}. *)
