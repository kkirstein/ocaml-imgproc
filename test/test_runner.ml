(* vim: set ft=ocaml sw=2 ts=2: *)

let () =
  Alcotest.run "Imgproc"
    [ ("Imgproc.Io", Test_io.test_set); ("Imgproc.Color", Test_color.test_set) ]
