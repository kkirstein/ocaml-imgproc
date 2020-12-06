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

  val adjust :
    ?min:float ->
    ?max:float ->
    ?channels:int array ->
    ary_type ->
    (ary_type, [> `Invalid_dimensions of int ]) result

  val rgb2gray : ary_type -> (ary_type, [> `Invalid_dimensions of int ]) result

  val rgb2gray' : ary_type -> (ary_type, [> `Invalid_dimensions of int ]) result

  val gray2rgb : ary_type -> (ary_type, [> `Invalid_dimensions of int ]) result

  val rgb2hsv :
    ary_type ->
    ( ary_type,
      [> `Invalid_dimensions of int | `Invalid_range of float * float ] )
    result

  val hsv2rgb :
    ary_type ->
    ( ary_type,
      [> `Invalid_dimensions of int | `Invalid_range of float * float ] )
    result
end

module Make (A : Ndarray) : Color with type ary_type := A.arr = struct
  let adjust_global ?(min = 0.0) ?(max = 1.0) nd =
    let scaling = (max -. min) /. (A.max' nd -. A.min' nd) in
    let offset = (A.min' nd *. scaling) -. min in
    A.map (fun x -> (x *. scaling) -. offset) nd

  let adjust ?(min = 0.0) ?(max = 1.0) ?channels nd =
    let num_dims = A.num_dims nd in
    match num_dims with
    | 1 | 2 -> Ok (adjust_global ~min ~max nd)
    | 3 ->
        let _num_chans = (A.shape nd).(2) in
        let chans =
          match channels with
          | Some cs -> cs
          | None -> Array.init num_dims Fun.id
        in
        let img_chans = A.split ~axis:2 [| 1; 1; 1 |] nd in
        Ok
          (Array.mapi
             (fun i c ->
               if Array.mem i chans then adjust_global ~min ~max c else c)
             img_chans
          |> A.concatenate ~axis:2)
    | x -> Error (`Invalid_dimensions x)

  let rgb2gray img =
    let dims = A.shape img in
    match Array.length dims with
    | 2 -> Ok img
    | 3 ->
        let chans = A.split ~axis:2 [| 1; 1; 1 |] img in
        let r, g, b = (chans.(0), chans.(1), chans.(2)) in
        Ok A.((r *$ 0.2125) + (g *$ 0.7154) + (b *$ 0.0721))
    | x -> Error (`Invalid_dimensions x)

  let rgb2gray' img =
    let dims = A.shape img in
    let gray_conv = rgb2gray img in
    Result.bind gray_conv (fun i -> Ok (A.reshape i [| dims.(0); dims.(1) |]))

  let gray2rgb img =
    let dims = A.shape img in
    match Array.length dims with
    | 2 -> Ok (A.repeat (A.expand ~hi:true img 3) [| 1; 1; 3 |])
    | 3 when dims.(3) = 1 ->
        let gray_mat = A.slice_left img [| dims.(0); dims.(1) |] in
        Ok (A.repeat (A.expand ~hi:true gray_mat 3) [| 1; 1; 3 |])
    | x -> Error (`Invalid_dimensions x)

  let rgb2hsv img =
    let min_val, max_val = A.minmax' img in
    if max_val > 1.0 && min_val < 0.0 then
      Error (`Invalid_range (min_val, max_val))
    else failwith "Not implemented"

  (* https://en.wikipedia.org/wiki/HSL_and_HSV#To_RGB *)

  let hsv2rgb img =
    let min_hue, max_hue = A.minmax' (A.get_slice [ []; []; [ 0 ] ] img)
    and min_val, max_val = A.minmax' (A.get_slice [ []; []; [ 1; 2 ] ] img) in
    if max_val > 1.0 && min_val < 0.0 then
      Error (`Invalid_range (min_val, max_val))
    else if max_hue > 360.0 && min_hue < 0.0 then
      Error (`Invalid_range (min_hue, max_hue))
    else
      let h = A.get_slice [ []; []; [ 0 ] ] img
      and s = A.get_slice [ []; []; [ 1 ] ] img
      and v = A.get_slice [ []; []; [ 2 ] ] img in
      let c = A.(v * s) and h' = A.(h /$ 60.0) in
      let _x = A.(c * (1.0 $- abs ((h' %$ 2.0) -$ 1.0))) in
      failwith "Not implemented"
end

module S = Make (Owl.Dense.Ndarray.S)
module D = Make (Owl.Dense.Ndarray.D)
