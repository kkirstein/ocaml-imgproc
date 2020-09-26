(lang dune 2.7)

(name imgproc)

(version 0.1.0)

(generate_opam_files true)

(source (github kkirstein/ocaml-imgproc))
(license MIT)
(authors "Kay-Uwe Kirstein <kayuwe.kirstein@uster.com>")
(maintainers "Kay-Uwe Kirstein <kayuwe.kirstein@uster.com>")

(package
 (name imgproc)
 (synopsis "Common image processing functions")
 (description "Common image processing functions")
 (depends
  (alcotest :with-test)
  (owl (>= 0.9.0))
  (stb_image (>= 0.5))
  (stb_image_write (>= 0.2))))
