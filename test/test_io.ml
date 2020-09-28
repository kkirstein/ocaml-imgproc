(* vim: set ft=ocaml sw=2 ts=2: *)

open Imgproc
open Owl.Dense.Ndarray

(* Testable types *)

(* Tests *)
let test_image_folder = "../../../test_images"

let rgb_file = Filename.concat test_image_folder "fruits.png"
let gray_file = Filename.concat test_image_folder "fruits_gray.png"

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

(* Test set *)
let test_set =
  [
    ("test images", `Quick, test_images);
    ("load RGB image", `Quick, test_load_rgb);
    ("load grascale image", `Quick, test_load_gray);
  ]
