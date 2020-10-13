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

let test_file = Filename.concat test_image_folder "test.png"

(* ------------------------------------------------------------------------- *)
let test_images () =
  Alcotest.(check bool) "RGB image" true (Sys.file_exists rgb_file);
  Alcotest.(check bool) "Grayscale image" true (Sys.file_exists gray_file)

(* ------------------------------------------------------------------------- *)
module type Dut = sig
  include Io.Io

  val shape : ary_type -> int array

  val equal : ary_type -> ary_type -> bool
end

module Make (Dut : Dut) = struct
  let test_load_rgb () =
    let rgb_img = Dut.load rgb_file |> Result.get_ok in
    Alcotest.(check (array int))
      "load RGB image" [| 480; 512; 3 |] (Dut.shape rgb_img)

  let test_load_gray () =
    let gray_img_s = Dut.load gray_file |> Result.get_ok in
    Alcotest.(check (array int))
      "load Grayscale image" [| 480; 512 |] (Dut.shape gray_img_s)

  let test_save_rgb () =
    let rgb_img = Dut.load rgb_file |> Result.get_ok in
    Alcotest.(check bool)
      "save RGB image" true
      (Dut.save test_file rgb_img |> Result.is_ok);
    let test_img = Dut.load test_file |> Result.get_ok in
    Alcotest.(check bool)
      "compare with original image" true
      (Dut.equal rgb_img test_img)

  let test_save_gray () =
    let gray_img = Dut.load gray_file |> Result.get_ok in
    Alcotest.(check bool)
      "save grayscale image" true
      (Dut.save test_file gray_img |> Result.is_ok);
    let test_img = Dut.load test_file |> Result.get_ok in
    Alcotest.(check bool)
      "compare with original image" true
      (Dut.equal gray_img test_img)
end

(* ------------------------------------------------------------------------- *)
module S = Make (struct
  include Io.S

  type ary_type = Owl.Dense.Ndarray.S.arr

  let shape = Owl.Dense.Ndarray.S.shape

  let equal = Owl.Dense.Ndarray.S.equal
end)

module D = Make (struct
  include Io.D

  type ary_type = Owl.Dense.Ndarray.D.arr

  let shape = Owl.Dense.Ndarray.D.shape

  let equal = Owl.Dense.Ndarray.D.equal
end)

(* ------------------------------------------------------------------------- *)
(* Test set *)
let test_set =
  [
    ("test images", `Quick, test_images);
    ("load RGB image (S)", `Quick, S.test_load_rgb);
    ("load RGB image (D)", `Quick, D.test_load_rgb);
    ("load grascale image (S)", `Quick, S.test_load_gray);
    ("load grascale image (D)", `Quick, D.test_load_gray);
    ("save RGB image (S)", `Quick, S.test_save_rgb);
    ("save RGB image (D)", `Quick, D.test_save_rgb);
    ("save grayscale image (S)", `Quick, S.test_save_gray);
    ("save grayscale image (D)", `Quick, D.test_save_gray);
  ]
