(* vim: set ft=ocaml sw=2 ts=2: *)

open Imgproc
open Owl.Dense.Ndarray

(* Testable types *)
let error_cmp a b =
  match (a, b) with
  | `Invalid_dimension x, `Invalid_dimension y when x = y -> true
  | `IO_error s1, `IO_error s2 when s1 = s2 -> true
  | _ -> false

let error_type = Alcotest.testable (Fmt.of_to_string Io.error_msg) error_cmp

(* Tests *)
let test_image_folder = "../../../test_images"

let rgb_file = Filename.concat test_image_folder "fruits.png"

let gray_file = Filename.concat test_image_folder "fruits_gray.png"

let test_file = Filename.concat test_image_folder "test.png"

(* ------------------------------------------------------------------------- *)
let test_images () =
  Alcotest.(check bool) "RGB image" true (Sys.file_exists rgb_file);
  Alcotest.(check bool) "Grayscale image" true (Sys.file_exists gray_file)

(* ------------------------------------------------------------------------- *)
let test_load_rgb () =
  let rgb_img = Io.load rgb_file |> Result.get_ok in
  Alcotest.(check (array int))
    "load RGB image" [| 480; 512; 3 |] (Generic.shape rgb_img)

(* ------------------------------------------------------------------------- *)
let test_load_gray () =
  let gray_img = Io.load gray_file |> Result.get_ok in
  Alcotest.(check (array int))
    "load Grayscale image" [| 480; 512 |] (Generic.shape gray_img)

(* ------------------------------------------------------------------------- *)
let test_save_rgb () =
  let rgb_img = Io.load rgb_file |> Result.get_ok in
  (* Alcotest.(check (result unit error_type))
     "save RGB image" (Ok ())
     (Io.save test_file rgb_img); *)
  Alcotest.(check bool)
    "save RGB image" true
    (Io.save test_file rgb_img |> Result.is_ok);
  let test_img = Io.load test_file |> Result.get_ok in
  Alcotest.(check bool)
    "compare with original image" true
    Generic.(rgb_img = test_img)

(* ------------------------------------------------------------------------- *)
let test_save_gray () =
  let gray_img = Io.load gray_file |> Result.get_ok in
  (* Alcotest.(check (result unit error_type))
     "save RGB image" (Ok ())
     (Io.save test_file rgb_img); *)
  Alcotest.(check bool)
    "save grayscale image" true
    (Io.save test_file gray_img |> Result.is_ok);
  let test_img = Io.load test_file |> Result.get_ok in
  Alcotest.(check bool)
    "compare with original image" true
    Generic.(gray_img = test_img)

(* Test set *)
let test_set =
  [
    ("test images", `Quick, test_images);
    ("load RGB image", `Quick, test_load_rgb);
    ("load grascale image", `Quick, test_load_gray);
    ("save RGB image", `Quick, test_save_rgb);
    ("save grayscale image", `Quick, test_save_gray);
  ]
