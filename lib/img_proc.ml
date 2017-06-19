(* vim: set ft=ocaml sw=2 ts=2: *)

(* image_processing.ml
 * Module for basic image processing functions
*)

open Bigarray


(* color mode of image data *)
type color_mode =
  | RGB
  | Gray

let channels_of_cmode = function
  | RGB   -> 3
  | Gray  -> 1

(* buffer to hold pixel data *)
type ('a, 'b) image_buffer =
  ('a, 'b, c_layout) Genarray.t

(* low-level image data type *)
type ('a, 'b) image = {
  width: int;
  height: int;
  channels: int;
  cmode: color_mode;
  data: ('a, 'b) image_buffer
}

(* high-level image data type *)
type t =
  | Int of (int, int8_unsigned_elt) image
  | Float of (float, float32_elt) image

(* some accessor functions *)
let width img =
  match img with
  | Int img'    -> img'.width
  | Float img'  -> img'.width
let height img =
  match img with
  | Int img'    -> img'.height
  | Float img'  -> img'.height
let channels img =
  match img with
  | Int img'    -> img'.channels
  | Float img'  -> img'.channels

(*$inject
  let string_of_cmode = function
    | RGB   -> "RGB"
    | Gray  -> "Gray"

  let img_pp img =
    match img with
    | Int i   -> Printf.sprintf "Int: {width: %d, height: %d, channels: %d, cmode: %s}"
                  i.width i.height i.channels (string_of_cmode i.cmode)
    | Float i -> Printf.sprintf "Float: {width: %d, height: %d, channels: %d, cmode: %s}"
                  i.width i.height i.channels (string_of_cmode i.cmode)
*)
(*$inject
  open Img_proc_test

  let img_rgb = match (img_data_demo 180 120 3 "int") with Int img' -> img'
  let img_rgb_f = match (img_data_demo 180 120 3 "float") with Float img' -> img'
*)

(** check image for given color mode and number of channels
  * throws an exception if given constraints are violated *)
let check_int ?channels ?color_mode img =
  match img with
  | Int i   -> begin
      match channels, color_mode with
      | Some ch, Some cm  -> if ch <> i.channels then failwith "Wrong number of channels"
        else if cm <> i.cmode then failwith "Wrong color mode"
        else i
      | Some ch, None     -> if ch <> i.channels then failwith "Wrong number of channels"
        else i
      | None, Some cm     -> if cm <> i.cmode then failwith "Wrong color mode"
        else i
      | None, None ->  i
    end
  | _       -> failwith "Wrong data type, expected Int"
(*$= check_int
  img_rgb (check_int ~channels:3 ~color_mode:RGB (Int img_rgb))
  img_rgb (check_int ~color_mode:RGB (Int img_rgb))
  img_rgb (check_int ~channels:3 (Int img_rgb))
  img_rgb (check_int (Int img_rgb))
*)
(*$T check_int
  try ignore(check_int ~color_mode:Gray (Int img_rgb)); false with Failure _ -> true
  try ignore(check_int ~channels:2 (Int img_rgb)); false with Failure _ -> true
  try ignore(check_int (Float img_rgb_f)); false with Failure _ -> true
*)

let check_float ?channels ?color_mode img =
  match img with
  | Float i   -> begin
      match channels, color_mode with
      | Some ch, Some cm  -> if ch <> i.channels then failwith "Wrong number of channels"
        else if cm <> i.cmode then failwith "Wrong color mode"
        else i
      | Some ch, None     -> if ch <> i.channels then failwith "Wrong number of channels"
        else i
      | None, Some cm     -> if cm <> i.cmode then failwith "Wrong color mode"
        else i
      | None, None ->  i
    end
  | _       -> failwith "Wrong data type, expected Float"
(*$= check_float
  img_rgb_f (check_float ~channels:3 ~color_mode:RGB (Float img_rgb_f))
  img_rgb_f (check_float ~color_mode:RGB (Float img_rgb_f))
  img_rgb_f (check_float ~channels:3 (Float img_rgb_f))
  img_rgb_f (check_float (Float img_rgb_f))
*)
(*$T check_float
  try ignore(check_float ~color_mode:Gray (Float img_rgb_f)); false with Failure _ -> true
  try ignore(check_float ~channels:2 (Float img_rgb_f)); false with Failure _ -> true
  try ignore(check_float (Int img_rgb)); false with Failure _ -> true
*)

(* access pixel data *)
let get_pix_int1 img x y =
  let i = check_int ~channels:1 img in
  Bigarray.Array2.get (Bigarray.array2_of_genarray i.data) x y

let get_pix_float1 img x y =
  let i = check_float ~channels:1 img in
  Bigarray.Array2.get (Bigarray.array2_of_genarray i.data) x y

let get_pix_int3 img x y =
  let i = check_int ~channels:3 img in
  (Bigarray.Array3.get (Bigarray.array3_of_genarray i.data) x y 0,
   Bigarray.Array3.get (Bigarray.array3_of_genarray i.data) x y 1,
   Bigarray.Array3.get (Bigarray.array3_of_genarray i.data) x y 2)

let get_pix_float3 img x y =
  let i = check_float ~channels:3 img in
  (Bigarray.Array3.get (Bigarray.array3_of_genarray i.data) x y 0,
   Bigarray.Array3.get (Bigarray.array3_of_genarray i.data) x y 1,
   Bigarray.Array3.get (Bigarray.array3_of_genarray i.data) x y 2)
(*$= get_pix_int3 & ~printer:(fun (r, g, b) -> Printf.sprintf "{r: %d, g: %d, b: %d}" r g b)
  (0, 1, 2) (get_pix_int3 (Int img_rgb) 0 0)
  (3, 4, 5) (get_pix_int3 (Int img_rgb) 1 0)
  (28, 29, 30) (get_pix_int3 (Int img_rgb) 0 1)
  (25, 26, 27) (get_pix_int3 (Int img_rgb) 179 0)
  (4, 5, 6) (get_pix_int3 (Int img_rgb) 0 119)
  (29, 30, 31) (get_pix_int3 (Int img_rgb) 179 119)
*)
