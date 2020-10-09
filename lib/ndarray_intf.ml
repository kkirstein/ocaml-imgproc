(** ndarray_intf
    
    Define module type for generic Owl.Dense.Ndarray module
    
    This is used to provide a module functor to support
    different Ndarray types in Imgproc
    *)

module type Ndarray = sig
  type arr

  type elt = float

  val init : int array -> (int -> elt) -> arr

  val shape : arr -> int array

  val num_dims : arr -> int

  val reshape : arr -> int array -> arr

  val transpose : ?axis:int array -> arr -> arr

  val get_slice : int list list -> arr -> arr

  val to_array : arr -> elt array

  val of_array : elt array -> int array -> arr

  val map_slice : ?axis:int -> (arr -> 'c) -> arr -> 'c array

  val iteri : (int -> elt -> unit) -> arr -> unit

  val mean : ?axis:int -> ?keep_dims:bool -> arr -> arr
end
