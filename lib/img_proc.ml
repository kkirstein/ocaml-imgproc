(* vim: set ft=ocaml sw=2 ts=2: *)

(* image_processing.ml
 * Module for basic image processing functions
*)

open Bigarray


(* color mode of image data *)
type color_mode =
	| RGB
  | Gray

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
  let img_rgb = {width = 180; height = 120; channels = 3; cmode = RGB;
                  data = Bigarray.Genarray.create Bigarray.Int8_unsigned Bigarray.C_layout [|120; 180; 3|]}
  let img_rgb_f = {width = 180; height = 120; channels = 3; cmode = RGB;
                  data = Bigarray.Genarray.create Bigarray.Float32 Bigarray.C_layout [|120; 180; 3|]}
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


(* convert RGB image to grayscale *)
let rgb_to_gray_int (r, g, b) =
	int_of_float (0.299*.(float_of_int r) +. 0.587*.(float_of_int g) +. 0.114*.(float_of_int b))

let rgb_to_gray_float (r, g, b) =
	0.299*.r +. 0.587*.g +. 0.114*.b

let gray_to_rgb gray =
	(gray, gray, gray)


(* color conversion for integer images *)
let convert_int_3_1 ~converter ~src_mode ~dest_mode img =
	let img' = check_int ~color_mode:src_mode ~channels:3 img in
	let buf = array3_of_genarray img'.data in
	let new_buf = Array2.create Bigarray.Int8_unsigned c_layout img'.width img'.height in
	(for x = 0 to (img'.width - 1) do
		for y = 0 to (img'.height - 1) do
			let new_val = converter Array3.(get buf x y 0, get buf x y 1, get buf x y 2) in
			Array2.set new_buf x y new_val
		done
	done);
	Int {width = img'.width; height = img'.height; channels = 1; cmode = dest_mode;
		data = genarray_of_array2 new_buf}

let convert_int_1_3 ~converter ~src_mode ~dest_mode img =
	let img' = check_int ~color_mode:src_mode ~channels:1 img in
	let buf = array2_of_genarray img'.data in
	let new_buf = Array3.create Bigarray.Int8_unsigned c_layout img'.width img'.height 3 in
	(for x = 0 to (img'.width - 1) do
		for y = 0 to (img'.height - 1) do
			let (a, b, c) = converter Array2.(get buf x y) in
			Array3.set new_buf x y 0 a;
			Array3.set new_buf x y 1 b;
			Array3.set new_buf x y 2 c
		done
	done);
	Int {width = img'.width; height = img'.height; channels = 3; cmode = dest_mode;
		data = genarray_of_array3 new_buf}

(* color conversion for float images *)
let convert_float_3_1 ~converter ~src_mode ~dest_mode img =
	let img' = check_float ~color_mode:src_mode ~channels:3 img in
	let buf = array3_of_genarray img'.data in
	let new_buf = Array2.create Bigarray.Float32 c_layout img'.width img'.height in
	(for x = 0 to (img'.width - 1) do
		for y = 0 to (img'.height - 1) do
			let new_val = converter Array3.(get buf x y 0, get buf x y 1, get buf x y 2) in
			Array2.set new_buf x y new_val
		done
	done);
	Float {width = img'.width; height = img'.height; channels = 1; cmode = dest_mode;
		data = genarray_of_array2 new_buf}

let convert_float_1_3 ~converter ~src_mode ~dest_mode img =
	let img' = check_float ~color_mode:src_mode ~channels:1 img in
	let buf = array2_of_genarray img'.data in
	let new_buf = Array3.create Bigarray.Float32 c_layout img'.width img'.height 3 in
	(for x = 0 to (img'.width - 1) do
		for y = 0 to (img'.height - 1) do
			let (a, b, c) = converter Array2.(get buf x y) in
			Array3.set new_buf x y 0 a;
			Array3.set new_buf x y 1 b;
			Array3.set new_buf x y 2 c
		done
	done);
	Float {width = img'.width; height = img'.height; channels = 3; cmode = dest_mode;
		data = genarray_of_array3 new_buf}


(*
let convert_color_rgb_gray img =
  match img with
    | Int img'    -> begin
      let buf = array3_of_genarray img'.data in
      let new_buf = Array2.create Bigarray.Int8_unsigned c_layout img'.width img'.height in
      (for x = 0 to (img'.width - 1) do
        for y = 0 to (img'.height - 1) do
          let new_val = rgb_to_gray_int Array3.(get buf x y 0, get buf x y 1, get buf x y 2) in
					Array2.set new_buf x y new_val
        done
      done);
      Int {width = img'.width; height = img'.height; channels = 1; cmode = Gray;
        data = genarray_of_array2 new_buf}
      end
    | Float img'  -> begin
      let buf = array3_of_genarray img'.data in
      let new_buf = Array2.create Bigarray.Float32 c_layout img'.width img'.height in
      (for x = 0 to img'.height do
        for y = 0 to img'.width do
					let new_val = rgb_to_gray_float Array3.(get buf 0 x y, get buf 1 x y, get buf 2 x y) in
					Array2.set new_buf x y new_val
        done
      done);
      Float {width = img'.width; height = img'.height; channels = 1; cmode = Gray;
        data = genarray_of_array2 new_buf}
    end
*)

(** convert color mode to given destination mode
 * the image data is copied into a new image struct *)
let convert_color ~src_mode ~dest_mode img =
  match (src_mode, dest_mode) with
  | (RGB, Gray) -> (match img with
										| Int _ 	-> convert_int_3_1 ~converter:rgb_to_gray_int
 																	~src_mode:RGB ~dest_mode:Gray img
										| Float _	-> convert_float_3_1 ~converter:rgb_to_gray_float
 																	~src_mode:RGB ~dest_mode:Gray img)
	| (Gray, RGB) -> (match img with
										| Int _ 	-> convert_int_1_3 ~converter:gray_to_rgb
 																	~src_mode:Gray ~dest_mode:RGB img
										| Float _	-> convert_float_1_3 ~converter:gray_to_rgb
 																	~src_mode:Gray ~dest_mode:RGB img)
  | _           -> failwith "The given color conversion is not supported."
