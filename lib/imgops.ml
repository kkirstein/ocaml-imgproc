(* vim: set ft=ocaml sw=2 ts=2: *)

(**
  Imgops
  Module with image processing functions for the SVC2 camera test
  in Uster production
 
  Author: kayuwe.kirstein
 
*)

module type Ndary = sig
  type arr

  type elt = float

  (* val init : int array -> (int -> elt) -> arr
*)
  val shape : arr -> int array

  val num_dims : arr -> int

  val get_slice : int list list -> arr -> arr

  val mean : ?axis:int -> arr -> arr

  val to_array : arr -> elt array

  val of_array : elt array -> int array -> arr

  val map_slice : ?axis:int -> (arr -> 'c) -> arr -> 'c array

  val transpose : ?axis:int array -> arr -> arr
end

module type Ops = sig
  type ary_type

  val centroid_line :
    ?orient:[< `Hor | `Ver > `Hor ] ->
    ary_type ->
    (ary_type, [> `Invalid_dimensions of int ]) result

  val profile_line :
    ?orient:[< `Hor | `Ver > `Hor ] ->
    int ->
    ary_type ->
    (ary_type, [> `Invalid_dimensions of int | `Invalid_index of int ]) result

  val avg_line :
    ?orient:[< `Hor | `Ver > `Hor ] ->
    ?avg:int ->
    int ->
    ary_type ->
    (ary_type, [> `Invalid_dimensions of int | `Invalid_index of int ]) result
end

module Make (A : Ndary) : Ops with type ary_type := A.arr = struct
  (* calculate centroid of 1d array *)
  let centroid ary =
    let arr = A.to_array ary in
    let _, v, m =
      Array.fold_left
        (fun (i, v, m) p -> (i + 1, v +. (float_of_int i *. p), m +. p))
        (1, 0.0, 0.0) arr
    in
    v /. m

  (* centroid or first moment of image *)
  let centroid_line ?(orient = `Hor) img =
    match A.num_dims img with
    | 2 -> (
        match orient with
        | `Hor ->
            let len = (A.shape img).(1) in
            let res =
              A.of_array
                (A.map_slice ~axis:0
                   (fun line -> centroid line)
                   (A.transpose img))
                [| 1; len |]
            in
            Ok res
        | `Ver ->
            let len = (A.shape img).(0) in
            let res =
              A.of_array
                (A.map_slice ~axis:0 (fun line -> centroid line) img)
                [| len; 1 |]
            in
            Ok res )
    | x -> Error (`Invalid_dimensions x)

  (* extract single line from given image *)
  let profile_line ?(orient = `Hor) coord img =
    match A.num_dims img with
    | 2 -> (
        let dims = A.shape img in
        let w, h = (dims.(1), dims.(0)) in
        match orient with
        | `Hor ->
            if coord < h then Ok (A.get_slice [ [ coord ]; [] ] img)
            else Error (`Invalid_index coord)
        | `Ver ->
            if coord < w then Ok (A.get_slice [ []; [ coord ] ] img)
            else Error (`Invalid_index coord) )
    | x -> Error (`Invalid_dimensions x)

  let avg_line ?(orient = `Hor) ?avg coord img =
    match A.num_dims img with
    | 2 -> (
        let dims = A.shape img in
        let w, h = (dims.(1), dims.(0)) in
        match avg with
        | Some s when s > 0 -> (
            let min, max = (coord - (s / 2), coord + (s / 2)) in
            match orient with
            | `Hor ->
                if min < 0 || max >= h then Error (`Invalid_index coord)
                else
                  let roi = A.get_slice [ [ min; max ]; [] ] img in
                  Ok (A.mean ~axis:0 roi)
            | `Ver ->
                if min < 0 || max >= w then Error (`Invalid_index coord)
                else
                  let roi = A.get_slice [ []; [ min; max ] ] img in
                  Ok (A.mean ~axis:1 roi) )
        | _ -> profile_line ~orient coord img )
    | x -> Error (`Invalid_dimensions x)
end

module S = Make (Owl.Dense.Ndarray.S)
module D = Make (Owl.Dense.Ndarray.D)
