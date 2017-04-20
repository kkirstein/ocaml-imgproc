(* vim: set ft=ocaml sw=2 ts=2: *)

(* img_proc_color.ml
 * Color-mode conversion of images.
 *
*)

open Img_proc

open Bigarray

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
	Img_proc.Int {width = img'.width; height = img'.height; channels = 1; cmode = dest_mode;
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
	Img_proc.Int {width = img'.width; height = img'.height; channels = 3; cmode = dest_mode;
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
	Img_proc.Float {width = img'.width; height = img'.height; channels = 1; cmode = dest_mode;
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
	Img_proc.Float {width = img'.width; height = img'.height; channels = 3; cmode = dest_mode;
		data = genarray_of_array3 new_buf}

(** convert color mode to given destination mode
 * the image data is copied into a new image struct *)
let convert_color ~src_mode ~dest_mode img =
  match (src_mode, dest_mode) with
  | (RGB, Gray) -> (match img with
										| Img_proc.Int _ 	  -> convert_int_3_1 ~converter:rgb_to_gray_int
 																	          ~src_mode:RGB ~dest_mode:Gray img
										| Img_proc.Float _	-> convert_float_3_1 ~converter:rgb_to_gray_float
 																	          ~src_mode:RGB ~dest_mode:Gray img)
	| (Gray, RGB) -> (match img with
										| Img_proc.Int _ 	  -> convert_int_1_3 ~converter:gray_to_rgb
 																	          ~src_mode:Gray ~dest_mode:RGB img
										| Img_proc.Float _	-> convert_float_1_3 ~converter:gray_to_rgb
 																	          ~src_mode:Gray ~dest_mode:RGB img)
  | _           -> failwith "The given color conversion is not supported."
