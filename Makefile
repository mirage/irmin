all:
	ocaml pkg/pkg.ml build -n irmin
	ocaml pkg/pkg.ml build -n irmin-git
	ocaml pkg/pkg.ml build -n irmin-http
	ocaml pkg/pkg.ml build -n irmin-mirage
	ocaml pkg/pkg.ml build -n irmin-unix --tests true
	ocaml pkg/pkg.ml test

clean:
	ocaml pkg/pkg.ml clean -n irmin
	ocaml pkg/pkg.ml clean -n irmin-git
	ocaml pkg/pkg.ml clean -n irmin-http
	ocaml pkg/pkg.ml clean -n irmin-mirage
	ocaml pkg/pkg.ml clean -n irmin-unix
