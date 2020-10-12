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

(* ------------------------------------------------------------------------- *)
let test_to_gray_s () =
  let rgb_img = Io.S.load rgb_file |> Result.get_ok in
  let gray_img = Io.S.load gray_file |> Result.get_ok in
  let to_gray = Color.S.to_gray rgb_img in
  Alcotest.(check bool) "to_gray returns Ok _" true (Result.is_ok to_gray);
  let to_gray_img = Result.get_ok to_gray in
  Alcotest.(check (array int))
    "to_gray returns single channel img" [| 480; 512; 1 |]
    (Generic.shape to_gray_img);
  Alcotest.(check bool)
    "to_gray returns grayscale values" true
    Generic.(approx_equal ~eps:0.05 to_gray_img gray_img)

let test_to_gray_d () =
  let rgb_img = Io.D.load rgb_file |> Result.get_ok in
  let gray_img = Io.D.load gray_file |> Result.get_ok in
  let to_gray = Color.D.to_gray rgb_img in
  Alcotest.(check bool) "to_gray returns Ok _" true (Result.is_ok to_gray);
  let to_gray_img = Result.get_ok to_gray in
  Alcotest.(check (array int))
    "to_gray returns single channel img" [| 480; 512; 1 |]
    (Generic.shape to_gray_img);
  Alcotest.(check bool)
    "to_gray returns grayscale values" true
    Generic.(approx_equal ~eps:0.05 to_gray_img gray_img)

(* ------------------------------------------------------------------------- *)
let test_to_gray'_s () =
  let rgb_img = Io.S.load rgb_file |> Result.get_ok in
  let gray_img = Io.S.load gray_file |> Result.get_ok in
  let to_gray = Color.S.to_gray' rgb_img in
  Alcotest.(check bool) "to_gray returns Ok _" true (Result.is_ok to_gray);
  let to_gray_img = Result.get_ok to_gray in
  Alcotest.(check (array int))
    "to_gray returns single channel img" [| 480; 512 |]
    (Generic.shape to_gray_img);
  Alcotest.(check bool)
    "to_gray returns grayscale values" true
    Generic.(approx_equal ~eps:0.05 to_gray_img gray_img)

let test_to_gray'_d () =
  let rgb_img = Io.D.load rgb_file |> Result.get_ok in
  let gray_img = Io.D.load gray_file |> Result.get_ok in
  let to_gray = Color.D.to_gray' rgb_img in
  Alcotest.(check bool) "to_gray returns Ok _" true (Result.is_ok to_gray);
  let to_gray_img = Result.get_ok to_gray in
  Alcotest.(check (array int))
    "to_gray returns single channel img" [| 480; 512 |]
    (Generic.shape to_gray_img);
  Alcotest.(check bool)
    "to_gray returns grayscale values" true
    Generic.(approx_equal ~eps:0.05 to_gray_img gray_img)

(* Test set *)
let test_set =
  [
    ("test to_gray (S)", `Quick, test_to_gray_s);
    ("test to_gray (D)", `Quick, test_to_gray_d);
    ("test to_gray' (S)", `Quick, test_to_gray'_s);
    ("test to_gray' (D)", `Quick, test_to_gray'_d);
  ]
