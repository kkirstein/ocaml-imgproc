(* vim: set ft=ocaml sw=2 ts=2: *)

(* img_proc_io.ml
 * Reading & writing of images.
 * This are simple wrappers around the Stb_image module
*)

(* sub.module to wrap IO functions from the Stb_image & Stb_image_write packages *)
module Stb_io = struct

  open Stb_image
  open Stb_image_write

  (* Pixel layout as return by Stb_image:
  // An output image with N components has the following components interleaved
  // in this order in each pixel:
  //
  //     N=#comp     components
  //       1           grey
  //       2           grey, alpha
  //       3           red, green, blue
  //       4           red, green, blue, alpha
  *)

  (* load image from file *)
  let load ?channels filename =
    match load ?channels filename with
      | Ok img  -> Img_proc.(Int {width = img.width; height = img.height;
                    channels = img.channels; cmode = RGB;
                    data = Bigarray.reshape (Bigarray.genarray_of_array1 img.data) [|img.width; img.height; img.channels|]})
      | Error (`Msg msg)  -> failwith msg

  (* write image to file *)
  let write_png filename img =
    let open Img_proc in
    match img with
      | Img_proc.Int img' -> let data = Bigarray.reshape_1 img'.data (img'.channels*img'.width*img'.height) in
                              png filename ~w:img'.width ~h:img'.height ~c:img'.channels data
      | Img_proc.Float _  -> failwith "Float images are currently not supported"

  let write_bmp filename img =
    match img with
      | Img_proc.Int img' -> let data = Img_proc.(Bigarray.reshape_1 img'.data (img'.channels*img'.width*img'.height)) in
                              Img_proc.(bmp filename ~w:img'.width ~h:img'.height ~c:img'.channels data)
      | Img_proc.Float _  -> failwith "Float images are currently not supported"

end

(* module Npm to write anymap formats *)
module Npm = struct

  open Img_proc

  let write_ppm filename img =
    let img' = match img with
      | Int img'  -> img'
      | Float _   -> failwith "Float images are not supported for PPM format" in
    let oc = open_out filename in
    try
      Printf.fprintf oc "P3\n";
      Printf.fprintf oc "%d %d %d\n" img'.width img'.height 255;
      let data = Img_proc.(Bigarray.reshape_1 img'.data (img'.channels*img'.width*img'.height)) in
      for i = 0 to (img'.width*img'.height - 1) do
        Printf.fprintf oc "%d %d %d"
          (Bigarray.Array1.get data (3*i))
          (Bigarray.Array1.get data (3*i + 1))
          (Bigarray.Array1.get data (3*i + 2));
        if ((i+1) mod 4) = 0 then output_char oc '\n' else output_char oc ' '
      done;
      close_out oc;
    with e ->
      close_out_noerr oc;
      raise e

  let write_pgm filename img =
    let img' = match img with
      | Int img'  -> img'
      | Float _   -> failwith "Float images are not supported for PPM format" in
    let oc = open_out filename in
    try
      Printf.fprintf oc "P2\n";
      Printf.fprintf oc "%d %d %d\n" img'.width img'.height 255;
      let data = Img_proc.(Bigarray.reshape_1 img'.data (img'.channels*img'.width*img'.height)) in
      for i = 0 to (img'.width*img'.height - 1) do
        output_string oc (string_of_int (Bigarray.Array1.get data i));
        if ((i+1) mod 16) = 0 then output_char oc '\n' else output_char oc ' '
      done;
      close_out oc;
    with e ->
      close_out_noerr oc;
      raise e

  (* generic wrapper for graymap & pixmap formats *)
  let write_pnm filename img =
    let img' = match img with
      | Int img'  -> img'
      | Float _   -> failwith "Float images are not supported for PPM format" in
  match img'.channels with
    | 3 -> write_ppm filename img
    | 1 -> write_pgm filename img
    | _ -> failwith "Only images with 1 or 3 channels are supported for PNM"

end

(* Supported image file formats *)
type fileformat =
  | PNG
  | BMP
  | PNM

let fix_array w h ary =
  let new_ary = Bigarray.Array1.create Bigarray.Int8_unsigned Bigarray.C_layout (w*h*3) in
  for idx = 0 to (w*h - 1) do
    Bigarray.Array1.set new_ary (idx*3) (Bigarray.Array1.get ary idx);
    Bigarray.Array1.set new_ary (idx*3+1) (Bigarray.Array1.get ary idx);
    Bigarray.Array1.set new_ary (idx*3+2) (Bigarray.Array1.get ary idx)
  done;
  new_ary

(* read image data from file *)
let load filename = Stb_io.load filename

(* write image data to file *)
let write ~format filename img =
  match format with
  | PNG -> Stb_io.write_png filename img
  | BMP -> Stb_io.write_bmp filename img
  | PNM -> Npm.write_pnm filename img
  (* | _   -> failwith "Format not supported" *)
