(* vim: set ft=ocaml sw=2 ts=2: *)

open Imgproc

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
module type Dut = sig
  include Color.Color

  val rgb_img : ary_type

  val gray_img : ary_type

  val shape : ary_type -> int array

  val approx_equal : ?eps:float -> ary_type -> ary_type -> bool
end

module Make (Dut : Dut) = struct
  let test_rgb2gray () =
    let to_gray = Dut.rgb2gray Dut.rgb_img in
    Alcotest.(check bool) "rgb2gray returns Ok _" true (Result.is_ok to_gray);
    let to_gray_img = Result.get_ok to_gray in
    Alcotest.(check (array int))
      "rgb2gray returns single channel img" [| 480; 512; 1 |]
      (Dut.shape to_gray_img);
    Alcotest.(check bool)
      "rgb2gray returns grayscale values" true
      (Dut.approx_equal ~eps:0.05 to_gray_img Dut.gray_img)

  let test_rgb2gray' () =
    let to_gray = Dut.rgb2gray' Dut.rgb_img in
    Alcotest.(check bool) "rgb2gray' returns Ok _" true (Result.is_ok to_gray);
    let to_gray_img = Result.get_ok to_gray in
    Alcotest.(check (array int))
      "rgb2gray' returns 2d img" [| 480; 512 |] (Dut.shape to_gray_img);
    Alcotest.(check bool)
      "rgb2gray' returns grayscale values" true
      (Dut.approx_equal ~eps:0.05 to_gray_img Dut.gray_img)

  let test_gray2rgb () =
    let to_rgb = Dut.gray2rgb Dut.gray_img in
    let to_rgb2 = Result.bind (Dut.rgb2gray' Dut.gray_img) Dut.gray2rgb in
    Alcotest.(check bool) "gray2rgb returns Ok _" true (Result.is_ok to_rgb);
    let to_rgb_img = Result.get_ok to_rgb in
    let to_rgb_img2 = Result.get_ok to_rgb2 in
    Alcotest.(check (array int))
      "gray2rgb returns three channel image" [| 480; 512; 3 |]
      (Dut.shape to_rgb_img);
    Alcotest.(check bool)
      "gray2rgb also works for 2d input" true
      Dut.(approx_equal to_rgb_img to_rgb_img2)
end

(* ------------------------------------------------------------------------- *)
module S = Make (struct
  include Color.S

  type ary_type = Owl.Dense.Ndarray.S.arr

  let rgb_img = Io.S.load rgb_file |> Result.get_ok

  let gray_img = Io.S.load gray_file |> Result.get_ok

  let shape = Owl.Dense.Ndarray.S.shape

  let approx_equal = Owl.Dense.Ndarray.S.approx_equal
end)

module D = Make (struct
  include Color.D

  type ary_type = Owl.Dense.Ndarray.D.arr

  let rgb_img = Io.D.load rgb_file |> Result.get_ok

  let gray_img = Io.D.load gray_file |> Result.get_ok

  let shape = Owl.Dense.Ndarray.D.shape

  let approx_equal = Owl.Dense.Ndarray.D.approx_equal
end)

(* Test set *)
let test_set =
  [
    ("test rgb2gray (S)", `Quick, S.test_rgb2gray);
    ("test rgb2gray (D)", `Quick, D.test_rgb2gray);
    ("test rgb2gray' (S)", `Quick, S.test_rgb2gray');
    ("test rgb2gray' (D)", `Quick, D.test_rgb2gray');
    ("test gray2rgb (S)", `Quick, S.test_gray2rgb);
    ("test gray2rgb (D)", `Quick, D.test_gray2rgb);
  ]