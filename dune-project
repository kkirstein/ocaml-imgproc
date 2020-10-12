(lang dune 2.7)

(name imgproc)

(version 0.1.1-dev)

(generate_opam_files true)

(source (github kkirstein/ocaml-imgproc))
(license MIT)
(authors "Kay-Uwe Kirstein <kayuwe.kirstein@uster.com>")
(maintainers "Kay-Uwe Kirstein <kayuwe.kirstein@uster.com>")

(package
 (name imgproc)
 (synopsis "Common image processing functions for owl")
 (description "This package provides image processing
 facilities for owl, like loading & saving common image file formats
 and color conversion.")
 (depends
  (alcotest :with-test)
  (owl (>= 0.10.0))
  (stb_image (>= 0.5))
  (stb_image_write (>= 0.2))))
