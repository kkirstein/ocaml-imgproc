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
  let rgb_img_s = Io.S.load rgb_file |> Result.get_ok in
  let rgb_img_d = Io.D.load rgb_file |> Result.get_ok in
  Alcotest.(check (array int))
    "load RGB image (single)" [| 480; 512; 3 |] (Generic.shape rgb_img_s);
  Alcotest.(check (array int))
    "load RGB image (double)" [| 480; 512; 3 |] (Generic.shape rgb_img_d)

(* ------------------------------------------------------------------------- *)
let test_load_gray () =
  let gray_img_s = Io.S.load gray_file |> Result.get_ok in
  let gray_img_d = Io.D.load gray_file |> Result.get_ok in
  Alcotest.(check (array int))
    "load Grayscale image (single)" [| 480; 512 |] (Generic.shape gray_img_s);
  Alcotest.(check (array int))
    "load Grayscale image (double)" [| 480; 512 |] (Generic.shape gray_img_d)

(* ------------------------------------------------------------------------- *)
let test_save_rgb () =
  let rgb_img_s = Io.S.load rgb_file |> Result.get_ok in
  let rgb_img_d = Io.D.load rgb_file |> Result.get_ok in
  (* Alcotest.(check (result unit error_type))
     "save RGB image" (Ok ())
     (Io.save test_file rgb_img); *)
  Alcotest.(check bool)
    "save RGB image (single)" true
    (Io.S.save test_file rgb_img_s |> Result.is_ok);
  let test_img = Io.S.load test_file |> Result.get_ok in
  Alcotest.(check bool)
    "compare with original image (single)" true
    Generic.(rgb_img_s = test_img);
  Alcotest.(check bool)
    "save RGB image (double)" true
    (Io.D.save test_file rgb_img_d |> Result.is_ok);
  let test_img = Io.D.load test_file |> Result.get_ok in
  Alcotest.(check bool)
    "compare with original image (double)" true
    Generic.(rgb_img_d = test_img)

(* ------------------------------------------------------------------------- *)
let test_save_gray () =
  let gray_img_s = Io.S.load gray_file |> Result.get_ok in
  let gray_img_d = Io.D.load gray_file |> Result.get_ok in
  Alcotest.(check bool)
    "save grayscale image (single)" true
    (Io.S.save test_file gray_img_s |> Result.is_ok);
  let test_img = Io.S.load test_file |> Result.get_ok in
  Alcotest.(check bool)
    "compare with original image (single)" true
    Generic.(gray_img_s = test_img);
  Alcotest.(check bool)
    "save grayscale image (double)" true
    (Io.D.save test_file gray_img_d |> Result.is_ok);
  let test_img = Io.D.load test_file |> Result.get_ok in
  Alcotest.(check bool)
    "compare with original image (double)" true
    Generic.(gray_img_d = test_img)

(* Test set *)
let test_set =
  [
    ("test images", `Quick, test_images);
    ("load RGB image", `Quick, test_load_rgb);
    ("load grascale image", `Quick, test_load_gray);
    ("save RGB image", `Quick, test_save_rgb);
    ("save grayscale image", `Quick, test_save_gray);
  ]
