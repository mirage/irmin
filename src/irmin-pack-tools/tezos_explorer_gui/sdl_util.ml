open Tsdl

let get = function
  | Ok r -> r
  | Error (`Msg e) ->
      Sdl.log "Error: %s" e;
      exit 1

let int x = int_of_float (floor x)

let draw_point r c (x, y) =
  let () =
    get
    @@ Sdl.set_render_draw_color r (Sdl.Color.r c) (Sdl.Color.g c)
         (Sdl.Color.b c) (Sdl.Color.a c)
  in
  get @@ Sdl.render_draw_point r (int x) (int y)

let fill_rect r c (x, y) (w, h) =
  let () =
    get
    @@ Sdl.set_render_draw_color r (Sdl.Color.r c) (Sdl.Color.g c)
         (Sdl.Color.b c) (Sdl.Color.a c)
  in
  let rect = Sdl.Rect.create ~x:(int x) ~y:(int y) ~w:(int w) ~h:(int h) in
  get @@ Sdl.render_fill_rect r (Some rect)

let draw_rect r c (x, y) (w, h) =
  let () =
    get
    @@ Sdl.set_render_draw_color r (Sdl.Color.r c) (Sdl.Color.g c)
         (Sdl.Color.b c) (Sdl.Color.a c)
  in
  let rect = Sdl.Rect.create ~x:(int x) ~y:(int y) ~w:(int w) ~h:(int h) in
  get @@ Sdl.render_draw_rect r (Some rect)

let draw_line r (x0, y0) (x1, y1) =
  let () = get @@ Sdl.set_render_draw_color r 0x00 0x00 0x00 0xff in
  get @@ Sdl.render_draw_line r (int x0) (int y0) (int x1) (int y1)

open Tsdl_ttf

let render_text r texture dst = get @@ Sdl.render_copy ~dst r texture

let draw_text r f text color (c_x, c_y) =
  let s = get @@ Ttf.render_text_solid f text color in
  let ttf_w, ttf_h = Sdl.get_surface_size s in
  let ttx_t = get @@ Sdl.create_texture_from_surface r s in
  Sdl.free_surface s;
  let rect_text =
    Sdl.Rect.create
      ~x:(int @@ (c_x -. (float ttf_w /. 2.)))
      ~y:(int @@ (c_y -. (float ttf_h /. 2.)))
      ~w:ttf_w ~h:ttf_h
  in
  render_text r ttx_t rect_text;
  (ttf_w, ttf_h)

let white = Sdl.Color.create ~r:256 ~g:256 ~b:256 ~a:0xff
let light_grey = Sdl.Color.create ~r:200 ~g:200 ~b:200 ~a:0xff
let grey = Sdl.Color.create ~r:80 ~g:80 ~b:80 ~a:0xff
let black = Sdl.Color.create ~r:0 ~g:0 ~b:0 ~a:0xff
let red = Sdl.Color.create ~r:255 ~g:0 ~b:0 ~a:0xff
let green = Sdl.Color.create ~r:0 ~g:255 ~b:0 ~a:0xff
let blue = Sdl.Color.create ~r:0 ~g:0 ~b:255 ~a:0xff
let purple = Sdl.Color.create ~r:255 ~g:0 ~b:255 ~a:0xff
let lighten_color c = Sdl.Color.(create ~r:(r c) ~g:(r c) ~b:(r c)) ~a:0xff
