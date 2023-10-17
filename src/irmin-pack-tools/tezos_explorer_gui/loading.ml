open Tsdl
open Tsdl_ttf
open Sdl_util

type state = Load_tree | Gen_layout

type counter = {
  mutable entries : int;
  mutable commits : int;
  mutable inodes : int;
}

type t = {
  w : Sdl.window;
  r : Sdl.renderer;
  mutable state : state;
  mutable last_refresh : float;
  mutable current : counter;
  max : counter;
  f : Ttf.font;
}

let init f =
  let w =
    get
    @@ Sdl.create_window ~w:256 ~h:90 "Loading tree"
         Sdl.Window.(opengl + popup_menu)
  in
  let r =
    get @@ Sdl.create_renderer ~index:(-1) ~flags:Sdl.Renderer.accelerated w
  in
  let current = { entries = 0; commits = 0; inodes = 0 } in
  let max = { entries = 0; commits = 0; inodes = 0 } in
  let last_refresh = Unix.gettimeofday () in
  { w; r; state = Load_tree; last_refresh; current; max; f }

let set_state t state =
  t.state <- state;
  t.current <- { entries = 0; commits = 0; inodes = 0 }

let get_state_text t =
  match t.state with
  | Load_tree -> "loading tree      (1/2)"
  | Gen_layout -> "generating layout (2/2)"

let get_progress t =
  let current = t.current.commits + t.current.inodes + t.current.entries in
  let max = t.max.commits + t.max.inodes + t.max.entries in
  if max > 0 then float current /. float max else 0.

let update_frame t =
  let ww, wh = Sdl.get_window_size t.w in
  let texture =
    get
    @@ Sdl.create_texture t.r
         (Sdl.get_window_pixel_format t.w)
         Sdl.Texture.access_target ~w:ww ~h:wh
  in
  (* setup texture *)
  let () = get @@ Sdl.set_render_target t.r (Some texture) in
  let () = get @@ Sdl.set_render_draw_color t.r 0xf0 0xf0 0xf0 0xff in
  let () = get @@ Sdl.render_clear t.r in
  let () = get @@ Sdl.set_render_draw_color t.r 0x00 0x00 0x00 0x00 in
  let () = Sdl.render_present t.r in
  (* write text *)
  let state_text = get_state_text t in
  let _progress = get_progress t in
  let state = Fmt.str "state: %s" state_text in
  let head_lines =
    [ Fmt.str "commits:"; Fmt.str "inodes:"; Fmt.str "entries:" ]
  in
  let currents =
    [
      Fmt.str "%d" t.current.commits;
      Fmt.str "%d" t.current.inodes;
      Fmt.str "%d" t.current.entries;
    ]
  in
  let maxes =
    [
      Fmt.str "/ %d@." t.max.commits;
      Fmt.str "/ %d@." t.max.inodes;
      Fmt.str "/ %d@." t.max.entries;
    ]
  in
  let show_text ?(x = 0) (h, max_w) text =
    let s = get @@ Ttf.render_text_solid t.f text black in
    let ttf_w, ttf_h = Sdl.get_surface_size s in
    let ttx_t = get @@ Sdl.create_texture_from_surface t.r s in
    Sdl.free_surface s;
    let rect_text = Sdl.Rect.create ~x ~y:h ~w:ttf_w ~h:ttf_h in
    let () = get @@ Sdl.render_copy ~dst:rect_text t.r ttx_t in
    (h + ttf_h, max (x + ttf_w) max_w)
  in
  let h, _ = show_text (0, 0) state in
  let _, max_w = List.fold_left show_text (h, 0) head_lines in
  let _, max_w = List.fold_left (show_text ~x:(max_w + 10)) (h, 0) currents in
  let h, _ = List.fold_left (show_text ~x:(max_w + 10)) (h, 0) maxes in
  (* progress bar *)
  let progress_w = int (get_progress t *. float ww) in
  let progress_h = wh - h in
  let rect_progress = Sdl.Rect.create ~x:0 ~y:h ~w:progress_w ~h:progress_h in
  let () = get @@ Sdl.set_render_draw_color t.r 0x00 0xff 0x00 0xff in
  let () = get @@ Sdl.render_fill_rect t.r (Some rect_progress) in
  let rect_progress_border = Sdl.Rect.create ~x:0 ~y:h ~w:ww ~h:progress_h in
  let () = get @@ Sdl.set_render_draw_color t.r 0x00 0x00 0x00 0x00 in
  let () = get @@ Sdl.render_draw_rect t.r (Some rect_progress_border) in
  (* reset render target *)
  let () = get @@ Sdl.set_render_target t.r None in
  (* copy texture *)
  let () = get @@ Sdl.render_copy t.r texture in
  (* present *)
  let () = Sdl.render_present t.r in
  ()

let check_close_event () =
  let e = Sdl.Event.create () in
  while Sdl.poll_event (Some e) do
    match Sdl.Event.(enum (get e typ)) with
    | `Window_event -> (
        match Sdl.Event.(window_event_enum (get e window_event_id)) with
        | `Close -> raise Exit
        | _ -> ())
    | _ -> ()
  done

let update t =
  let framerate = 1. /. 20. in
  let now = Unix.gettimeofday () in
  let wait_time = (t.last_refresh +. framerate -. now) *. 1000. in
  if wait_time < 0. then (
    check_close_event ();
    update_frame t;
    t.last_refresh <- Unix.gettimeofday ())

let destroy t = Sdl.destroy_window t.w
