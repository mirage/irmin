open Tsdl
open Tsdl_ttf
open Sdl_util
open Layout
open Context

let get_tree_rect w wr =
  let bst, bsl, bsb, bsr = get @@ Sdl.get_window_borders_size w in
  let open Sdl.Rect in
  let tw, th = (w wr - bsl - bsr, h wr - bst - bsb) in
  let w = tw in
  let h = th in
  let x = 0 in
  let y = 0 in
  create ~x ~y ~w ~h

let generate_tree ctx d =
  let tr = get_tree_rect ctx.w ctx.wr in
  let loading = Loading.init ctx.f in
  Loading.update loading;
  (* load tree *)
  let commit_info = List.nth ctx.indexes ctx.current in
  let last_commit_addr =
    if ctx.current = 0 then Optint.Int63.of_int (-1)
    else snd @@ List.nth ctx.indexes (ctx.current - 1)
  in
  let tree =
    Load_tree.load_tree loading ctx.store_path ~max_depth:d commit_info
      last_commit_addr
  in
  (* layout *)
  let layout = layout ctx loading tree in
  (* extract *)
  let (tree_w, tree_h), render = Prettree.extract layout in
  let scale_w = float_of_int (Sdl.Rect.w tr) /. tree_w in
  let scale_h = float_of_int (Sdl.Rect.h tr) /. tree_h in
  let box =
    {
      min_w = 0.;
      max_w = tree_w;
      min_h = 0.;
      max_h = tree_h;
      scale_w;
      scale_h;
      zoom = 0.9;
    }
  in
  Loading.destroy loading;
  (render, box)

let generate_tree_texture ctx tree box =
  (* create texture *)
  let tr = get_tree_rect ctx.w ctx.wr in
  let t =
    get
    @@ Sdl.create_texture ctx.r
         (Sdl.get_window_pixel_format ctx.w)
         Sdl.Texture.access_target ~w:(Sdl.Rect.w tr) ~h:(Sdl.Rect.h tr)
  in
  (* setup texture *)
  let () = get @@ Sdl.set_render_target ctx.r (Some t) in
  let () = get @@ Sdl.set_render_draw_color ctx.r 0xff 0xff 0xff 0xff in
  let () = get @@ Sdl.render_clear ctx.r in
  let () = get @@ Sdl.set_render_draw_color ctx.r 0x00 0x00 0x00 0x00 in
  (* render *)
  let _ = tree (0., 0.) box in
  let () = get @@ Sdl.set_render_target ctx.r None in
  t

let wait_shown () =
  let e = Sdl.Event.create () in
  let _ = get @@ Sdl.wait_event (Some e) in
  while
    Sdl.Event.(
      enum (get e typ) <> `Window_event
      || window_event_enum (get e window_event_id) <> `Shown)
  do
    let _ = get @@ Sdl.wait_event (Some e) in
    ()
  done

let refresh_rate ctx =
  let framerate = 1. /. 60. in
  let now = Unix.gettimeofday () in
  let wait_time = (ctx.last_refresh +. framerate -. now) *. 1000. in
  if wait_time > 0. then Sdl.delay (Int32.of_float wait_time);
  ctx.last_refresh <- Unix.gettimeofday ()

type texture = {
  mutable data : Layout.texture_data;
  mutable render :
    Prettree.v2 ->
    Layout.texture_data ->
    (bool * (float * float) * (float * float)) * Layout.texture_data;
  mutable texture : Sdl.texture;
}

let set_texture t texture =
  Sdl.destroy_texture t.texture;
  t.texture <- texture

let main store_path font_path i d =
  let () = get @@ Sdl.init Sdl.Init.(video + events) in
  let () = get @@ Ttf.init () in
  let ctx = init_context store_path font_path i in
  (* wait for the window to be showned *)
  wait_shown ();
  try
    (* generate tree *)
    let render, data = generate_tree ctx d in
    let texture = generate_tree_texture ctx render data in
    let tree_texture = { data; render; texture } in
    (* main loop *)
    while true do
      (* fetch events *)
      let e = Sdl.Event.create () in
      while Sdl.poll_event (Some e) do
        match Sdl.Event.(enum (get e typ)) with
        | `Quit -> raise Exit
        | `Mouse_button_down ->
            let button = Sdl.Event.(get e mouse_button_button) in
            if button = Sdl.Button.left then
              let _, pos = Sdl.get_mouse_state () in
              ctx.drag <- Some pos
        | `Mouse_button_up ->
            let button = Sdl.Event.(get e mouse_button_button) in
            if button = Sdl.Button.left then ctx.drag <- None
        | `Mouse_motion -> (
            match ctx.drag with
            | None -> ()
            | Some (x, y) ->
                let data = tree_texture.data in
                let _, (x', y') = Sdl.get_mouse_state () in
                ctx.drag <- Some (x', y');
                let move_x = float (x - x') /. data.scale_w /. data.zoom in
                let move_y = float (y - y') /. data.scale_h /. data.zoom in
                tree_texture.data <-
                  {
                    data with
                    min_w = data.min_w +. move_x;
                    max_w = data.max_w +. move_x;
                    min_h = data.min_h +. move_y;
                    max_h = data.max_h +. move_y;
                  };
                ctx.updated <- true)
        | `Mouse_wheel ->
            let wheel_zoom = Sdl.Event.(get e mouse_wheel_y) in
            let data = tree_texture.data in
            tree_texture.data <-
              {
                data with
                zoom = min (max (data.zoom +. (float wheel_zoom /. 10.)) 0.9) 4.;
              };
            ctx.updated <- true
        | `Key_up ->
            let key = Sdl.Event.(get e keyboard_keycode) in
            if key = Sdl.K.left then (
              ctx.current <- max 0 (ctx.current - 1);
              let render, data = generate_tree ctx d in
              tree_texture.data <- data;
              tree_texture.render <- render;
              let texture = generate_tree_texture ctx render data in
              set_texture tree_texture texture);
            if key = Sdl.K.right then (
              ctx.current <- min (ctx.current + 1) (List.length ctx.indexes - 1);
              let render, box = generate_tree ctx d in
              tree_texture.data <- box;
              tree_texture.render <- render;
              let texture = generate_tree_texture ctx render box in
              set_texture tree_texture texture);
            ()
        | _ -> ()
      done;
      if ctx.updated then (
        let texture =
          generate_tree_texture ctx tree_texture.render tree_texture.data
        in
        set_texture tree_texture texture;
        ctx.updated <- false);
      (* clear screen *)
      let () = get @@ Sdl.set_render_draw_color ctx.r 0xff 0xff 0xff 0xff in
      let () = get @@ Sdl.render_clear ctx.r in
      (* render tree *)
      let () = get @@ Sdl.render_copy ctx.r tree_texture.texture in
      (* present *)
      let () = Sdl.render_present ctx.r in
      (* framerate delay *)
      refresh_rate ctx;
      ()
    done
  with Exit ->
    delete_context ctx;
    Ttf.quit ();
    Sdl.quit ();
    exit 0

(* cmdliner *)

open Cmdliner

let store_path =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"store path" ~doc:"path to the store")

let font_path =
  Arg.(
    required
    & pos 1 (some string) None
    & info [] ~docv:"font path" ~doc:"path to the text font")

let commit =
  Arg.(
    required
    & pos 2 (some int) None
    & info [] ~docv:"id commit" ~doc:"if of the commit")

let depth =
  Arg.(
    value & opt int (-1) & info [ "d"; "depth" ] ~docv:"depth" ~doc:"max depth")

let main_cmd =
  let doc = "a gui for tezos store exploration" in
  let info = Cmd.info "graphics" ~doc in
  Cmd.v info Term.(const main $ store_path $ font_path $ commit $ depth)

let () = exit (Cmd.eval ~catch:false main_cmd)
