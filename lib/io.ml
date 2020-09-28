(**
  Imgproc module offers some basic image processing functions
  for image data given as Owl's Ndarray *)

(* useful string representations *)
let print_img_info img =
  let open Owl.Dense.Ndarray.Generic in
  let dims = shape img in
  match Array.length dims with
  | 3 -> Ok (Printf.printf "w:%d h:%d c:%d\n" dims.(1) dims.(0) dims.(2))
  | 2 -> Ok (Printf.printf "w:%d h:%d\n" dims.(1) dims.(0))
  | x -> Error (`Invalid_dimension x)

let error_msg = function
  | `Invalid_dimension x -> "Invalid dimension " ^ string_of_int x
  | `IO_error str -> "IO Error: " ^ str

(* color conversion *)
let to_grayscale img =
  let open Owl.Dense.Ndarray in
  let dims = Generic.shape img in
  match Array.length dims with
  | 2 -> Ok img
  | 3 ->
      let chans = Generic.split ~axis:2 [| 1; 1; 1 |] img in
      let r, g, b = (chans.(0), chans.(1), chans.(2)) in
      Ok
        Generic.(
          reshape
            ((r *$ 0.2125) + (g *$ 0.7154) + (b *$ 0.0721))
            [| dims.(0); dims.(1) |])
  | x -> Error (`Invalid_dimension x)

(* convert given image buffer to Ndarray and vice-versa *)
let image_to_ndarray img =
  let open Owl.Dense.Ndarray in
  let w = Stb_image.width img in
  let h = Stb_image.height img in
  let c = Stb_image.channels img in
  match c with
  | 1 ->
      let nd = S.init [| h; w |] (fun i -> float_of_int img.data.{i} /. 255.) in
      Ok nd
  | 3 ->
      let nd = S.init [| h; w; 3 |] (fun i -> float_of_int img.data.{i} /. 255.) in
      Ok nd
  | x -> Error (`Invalid_dimension x)

let ndarray_to_image nd =
  let open Owl.Dense.Ndarray in
  match S.num_dims nd with
  | 2 ->
      let dims = S.shape nd in
      let w, h = (dims.(1), dims.(0)) in
      let nd_1d = S.reshape nd [| w * h |] in
      let buf = Bigarray.(Array1.create int8_unsigned c_layout (w * h)) in
      S.iteri (fun i x -> buf.{i} <- int_of_float (x *. 255.0)) nd_1d;
      Ok buf
  | 3 ->
      let dims = S.shape nd in
      let w, h, c = (dims.(1), dims.(0), dims.(2)) in
      let nd_1d = S.reshape nd [| w * h * c |] in
      let buf = Bigarray.(Array1.create int8_unsigned c_layout (w * h * c)) in
      S.iteri (fun i x -> buf.{i} <- int_of_float (x *. 255.0)) nd_1d;
      Ok buf
  | x -> Error (`Invalid_dimension x)

(* file IO *)
let save ?(fmt = `PNG) file_path img =
  let open Owl.Dense.Ndarray in
  let exporter = match fmt with `PNG -> Stb_image_write.png in
  match S.num_dims img with
  | 2 ->
      let dims = S.shape img in
      let w, h, c = (dims.(1), dims.(0), 1) in
      let img_buf = ndarray_to_image img in
      Result.bind img_buf (fun buf -> Ok (exporter file_path ~w ~h ~c buf))
  | 3 ->
      let dims = S.shape img in
      let w, h, c = (dims.(1), dims.(0), dims.(2)) in
      let img_buf = ndarray_to_image img in
      Result.bind img_buf (fun buf -> Ok (exporter file_path ~w ~h ~c buf))
  | x -> Error (`Invalid_dimension x)

let load file_path =
  match Stb_image.load file_path with
  | Ok img_buf -> image_to_ndarray img_buf
  | Error (`Msg err) -> Error (`IO_error err)
