(* vim: set ft=ocaml sw=2 ts=2: *)

(* image_processing.ml
 * Module for test infrastructure of img_proc library
*)

open Img_proc

let img_data_demo w h c dtype =
  match dtype with
  | "int" -> begin
      let img = {width = w; height = h; channels = c; cmode = RGB;
                 data = Bigarray.(Genarray.create Int8_unsigned C_layout [|w; h; c|])}
      in
      for z = 0 to c - 1 do
        for y = 0 to h - 1 do
          for x = 0 to w - 1 do
            let value = (x * c + y * w * c + z) mod 256 in
            Bigarray.(Array3.set (array3_of_genarray img.data) x y z value)
          done
        done
      done;
      Int img
    end
  | "float" -> begin
      let img = {width = w; height = h; channels = c; cmode = RGB;
                 data = Bigarray.(Genarray.create Float32 C_layout [|w; h; c|])}
      in
      for z = 0 to c - 1 do
        for y = 0 to h - 1 do
          for x = 0 to w - 1 do
            let value = (float (x * c + y * w * c + z)) /. (float (w * h * c))
            in
            Bigarray.(Array3.set (array3_of_genarray img.data) x y z value)
          done
        done
      done;
      Float img
    end
  | dt -> failwith ("Unsupported type identifiy " ^ dt)
