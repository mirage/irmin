open Tsdl
open Tsdl_ttf
open Optint
open Sdl_util

type ctx = {
  sw : Eio.Switch.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  r : Sdl.renderer;
  w : Sdl.window;
  wr : Sdl.rect;
  f : Ttf.font;
  indexes : (string * Int63.t) list;
  store_path : Eio.Fs.dir_ty Eio.Path.t;
  mutable drag : (int * int) option;
  mutable current : int;
  mutable last_refresh : float;
  mutable updated : bool;
}

let get_window_rect () =
  let open Sdl.Rect in
  let bounds = get @@ Sdl.get_display_bounds 0 in
  let usable_bounds = get @@ Sdl.get_display_usable_bounds 0 in
  let uw = w usable_bounds in
  let uh = h usable_bounds in
  create ~x:(w bounds - uw) ~y:(Sdl.Rect.h bounds - uh) ~w:uw ~h:uh

let init_context ~sw ~fs store_path font_path i =
  let wr = get_window_rect () in
  let w =
    let open Sdl.Rect in
    get
    @@ Sdl.create_window ~x:(x wr) ~y:(y wr) ~w:(w wr) ~h:(h wr)
         "Tezos store explorer" Sdl.Window.opengl
  in
  let r =
    get @@ Sdl.create_renderer ~index:(-1) ~flags:Sdl.Renderer.accelerated w
  in
  let f = get @@ Ttf.open_font font_path 12 in
  let last_refresh = Unix.gettimeofday () in
  let indexes = Load_tree.load_index (Eio.Path.native_exn store_path) in
  let current = i in
  {
    sw;
    fs;
    r;
    w;
    wr;
    f;
    store_path;
    indexes;
    current;
    drag = None;
    last_refresh;
    updated = false;
  }

let delete_context ctx =
  Ttf.close_font ctx.f;
  Sdl.destroy_renderer ctx.r;
  Sdl.destroy_window ctx.w
