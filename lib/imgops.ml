(* vim: set ft=ocaml sw=2 ts=2: *)

(**
  Imgops
  Module with image processing functions for the SVC2 camera test
  in Uster production
 
  Author: kayuwe.kirstein
 
*)

open Owl.Dense.Ndarray

(* calculate centroid of 1d array *)
let centroid ary =
  let arr = S.to_array ary in
  let _, v, m =
    Array.fold_left
      (fun (i, v, m) p -> (i + 1, v +. (float_of_int i *. p), m +. p))
      (1, 0.0, 0.0) arr
  in
  v /. m

(* centroid or first moment of image *)
let centroid_line ?(orient = `Hor) img =
  match S.num_dims img with
  | 2 -> (
      match orient with
      | `Hor ->
          let len = (S.shape img).(1) in
          let res =
            S.of_array
              (S.map_slice ~axis:0
                 (fun line -> centroid line)
                 (S.transpose img))
              [| 1; len |]
          in
          Ok res
      | `Ver ->
          let len = (S.shape img).(0) in
          let res =
            S.of_array
              (S.map_slice ~axis:0 (fun line -> centroid line) img)
              [| len; 1 |]
          in
          Ok res )
  | x -> Error (`Invalid_dimensions x)

(* extract single line from given image *)
let profile_line ?(orient = `Hor) coord img =
  match S.num_dims img with
  | 2 -> (
      let dims = S.shape img in
      let w, h = (dims.(1), dims.(0)) in
      match orient with
      | `Hor ->
          if coord < h then Ok (S.get_slice [ [ coord ]; [] ] img)
          else Error (`Invalid_index coord)
      | `Ver ->
          if coord < w then Ok (S.get_slice [ []; [ coord ] ] img)
          else Error (`Invalid_index coord) )
  | x -> Error (`Invalid_dimensions x)

let avg_line ?(orient = `Hor) ?avg coord img =
  match S.num_dims img with
  | 2 -> (
      let dims = S.shape img in
      let w, h = (dims.(1), dims.(0)) in
      match avg with
      | Some s when s > 0 -> (
          let min, max = (coord - (s / 2), coord + (s / 2)) in
          match orient with
          | `Hor ->
              if min < 0 || max >= h then Error (`Invalid_index coord)
              else
                let roi = S.get_slice [ [ min; max ]; [] ] img in
                Ok (S.mean ~axis:0 roi)
          | `Ver ->
              if min < 0 || max >= w then Error (`Invalid_index coord)
              else
                let roi = S.get_slice [ []; [ min; max ] ] img in
                Ok (S.mean ~axis:1 roi) )
      | _ -> profile_line ~orient coord img )
  | x -> Error (`Invalid_dimensions x)
