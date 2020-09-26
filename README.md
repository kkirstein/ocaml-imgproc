# ocaml-imgproc

An image processing library written in OCaml.

## Motivation

This image processing library uses the [Owl](https://ocaml.xyz) package for scientific computing in OCaml as base.
Think of it like the Python package [scikit-image](https://scikit-image.org/) is building upon `scipy` and `numpy`.

## Status

Pretty much pre-alpha...

There are IO function to read and write image data as `Owl` `Ndarray`s. Those are mainly wrappers around
[stb_image](https://opam.ocaml.org/packages/) and [stb_image_write](https://opam.ocaml.org/packages/stb_image_write/).

## License

MIT license

