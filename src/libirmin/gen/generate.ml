let generate dirname =
  let prefix = "irmin" in
  let path basename = Filename.concat dirname basename in
  let ml_fd = open_out (path "irmin_bindings.ml") in
  let c_fd = open_out (path "irmin.c") in
  let h_fd = open_out (path "irmin.h") in
  let stubs = (module Libirmin_bindings.Stubs : Cstubs_inverted.BINDINGS) in
  let writeln fd s = output_string fd (s ^ "\n") in
  let types fd names =
    List.iter
      (fun n -> writeln fd (Printf.sprintf "typedef struct %s %s;" n n))
      names
  in
  (* Generate the ML module that links in the generated C. *)
  Cstubs_inverted.write_ml (Format.formatter_of_out_channel ml_fd) ~prefix stubs;

  (* Generate the C source file that exports OCaml functions. *)
  Format.fprintf
    (Format.formatter_of_out_channel c_fd)
    "#include \"irmin.h\"@\n%a"
    (Cstubs_inverted.write_c ~prefix)
    stubs;
  writeln c_fd
    {|
void irmin_init(int argc, char* argv[], char* envp[]){
  caml_startup(argv);
}

#ifndef IRMIN_NO_INIT
#ifdef __APPLE__
__attribute__((section("__DATA,__mod_init_func"))) typeof(irmin_init) *__init = irmin_init;
#else
__attribute__((section(".init_array"))) void (* p_irmin_init)(int,char*[],char*[]) = &irmin_init;
#endif
#endif
    |};

  (* Generate the C header file that exports OCaml functions. *)
  writeln h_fd "#pragma once";
  writeln h_fd "#include <stdbool.h>";
  writeln h_fd "#include <stdint.h>";
  types h_fd
    [
      "IrminType";
      "IrminValue";
      "IrminMetadata";
      "IrminContents";
      "IrminConfig";
      "IrminRepo";
      "Irmin";
      "IrminPath";
      "IrminCommitKey";
      "IrminKindedKey";
      "IrminTree";
      "IrminCommit";
      "IrminInfo";
      "IrminHash";
      "IrminString";
      "IrminPathList";
      "IrminCommitList";
      "IrminBranchList";
    ];
  writeln h_fd "void caml_startup(char *argv[]);";
  writeln h_fd "void caml_shutdown();";

  Cstubs_inverted.write_c_header
    (Format.formatter_of_out_channel h_fd)
    ~prefix stubs;
  writeln h_fd
    {|
#ifndef IRMIN_NO_AUTO
static void _irmin_cleanup(void *p) { if (p) { irmin_free(*(Irmin**)p); p = (void*)0;} };
#define AUTO __attribute__((cleanup(_irmin_cleanup)))
#endif
    |};

  close_out h_fd;
  close_out c_fd;
  close_out ml_fd

let () = generate Sys.argv.(1)
