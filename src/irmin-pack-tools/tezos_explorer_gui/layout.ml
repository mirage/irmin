open Tree
open Sdl_util
open Context

type texture_data = {
  min_w : float;
  max_w : float;
  min_h : float;
  max_h : float;
  scale_w : float;
  scale_h : float;
  zoom : float;
}

let must_be_shown (x, y) (size_w, size_h) t =
  x +. size_w >= t.min_w
  && x <= t.max_w +. (t.zoom /. t.zoom)
  && y +. size_h >= t.min_h
  && y <= t.max_h +. (t.zoom /. t.zoom)

let scale_text_rect ttx_r (scale_w, scale_h) =
  let open Tsdl in
  let text_w = float (Sdl.Rect.w ttx_r) in
  let text_h = float (Sdl.Rect.h ttx_r) in
  let corrected_w = min scale_w text_w in
  let corrected_h = min scale_h text_h in
  Sdl.Rect.(
    create
      ~x:(x ttx_r + (int @@ ((text_w -. corrected_w) /. 2.)))
      ~y:(y ttx_r) ~w:(int corrected_w) ~h:(int corrected_h))

let render_rect renderer color size (ttx_t, ttx_r, ttx_width) current (x, y) t =
  let scale_w, scale_h =
    (t.scale_w *. t.zoom *. size, t.scale_h *. t.zoom *. size)
  in
  let x', y' = ((x -. t.min_w) *. scale_w, (y -. t.min_h) *. scale_h) in
  let scale_w = scale_w *. ttx_width in
  let must_be_shown = must_be_shown (x, y) (size *. ttx_width, size) t in
  if must_be_shown then
    if min scale_w scale_h < 1. then draw_point renderer color (x', y')
    else (
      if not current then
        fill_rect renderer light_grey (x', y') (scale_w, scale_h);
      draw_rect renderer color (x', y') (scale_w, scale_h);
      let center = (x' +. (scale_w /. 2.), y' +. (scale_h /. 2.)) in
      let ttx_r = scale_text_rect (ttx_r center) (scale_w, scale_h) in
      render_text renderer ttx_t ttx_r);
  ( ( must_be_shown,
      (x' +. (scale_w /. 2.), y'),
      (x' +. (scale_w /. 2.), y' +. scale_h) ),
    t )

let render_link renderer ((b1, _, bottom), _) ((b2, top, _), _) =
  if b1 || b2 then draw_line renderer bottom top

let get_text_texture ctx text =
  let open Tsdl in
  let open Tsdl_ttf in
  let s = get @@ Ttf.render_text_solid ctx.f text black in
  let ttf_w, ttf_h = Sdl.get_surface_size s in
  let text_texture = get @@ Sdl.create_texture_from_surface ctx.r s in
  Sdl.free_surface s;
  let text_rect (c_x, c_y) =
    Sdl.Rect.create
      ~x:(int @@ (c_x -. (float ttf_w /. 2.)))
      ~y:(int @@ (c_y -. (float ttf_h /. 2.)))
      ~w:ttf_w ~h:ttf_h
  in
  (text_texture, text_rect, float ttf_w /. 10.)

let layout ctx loading =
  let rec layout_rec { depth = _; path; obj; current } =
    let open Prettree in
    Loading.update loading;
    let size = 1. in
    match obj with
    | Leaf ->
        loading.current.entries <- loading.current.entries + 1;
        let text_texture, text_rect, text_width = get_text_texture ctx path in
        Prettree.make
          (size *. text_width, size)
          (fun pos t ->
            render_rect ctx.r blue size
              (text_texture, text_rect, text_width)
              current pos t)
    | Commit None ->
        loading.current.commits <- loading.current.commits + 1;
        let text_texture, text_rect, text_width = get_text_texture ctx path in
        Prettree.make
          (size *. text_width, size)
          (fun pos t ->
            render_rect ctx.r red size
              (text_texture, text_rect, text_width)
              current pos t)
    | Commit (Some child) ->
        loading.current.commits <- loading.current.commits + 1;
        Prettree.vert
        @@
        let open Prettree.Syntax in
        let+ parent =
          let text_texture, text_rect, text_width = get_text_texture ctx path in
          Prettree.make
            (size *. text_width, size)
            (fun pos t ->
              render_rect ctx.r red size
                (text_texture, text_rect, text_width)
                current pos t)
        and+ () = Prettree.padding 1.
        and+ child = layout_rec child in
        fun t ->
          let parent_info = parent t in
          let child_info = child t in
          render_link ctx.r parent_info child_info;
          parent_info
    | Inode i -> (
        loading.current.inodes <- loading.current.inodes + 1;
        match i with
        | Values None ->
            let text_texture, text_rect, text_width =
              get_text_texture ctx path
            in
            Prettree.make
              (size *. text_width, size)
              (fun pos t ->
                render_rect ctx.r green size
                  (text_texture, text_rect, text_width)
                  current pos t)
        | Values (Some l) ->
            Prettree.vert
            @@
            let open Prettree.Syntax in
            let+ parent =
              let text_texture, text_rect, text_width =
                get_text_texture ctx path
              in
              Prettree.make
                (size *. text_width, size)
                (fun pos t ->
                  render_rect ctx.r green size
                    (text_texture, text_rect, text_width)
                    current pos t)
            and+ () = Prettree.padding 1.
            and+ l = horz (list ~padding:size (List.map layout_rec l)) in
            fun scale ->
              let parent_pos = parent scale in
              List.iter
                (fun child -> render_link ctx.r parent_pos (child scale))
                l;
              parent_pos
        | Tree None ->
            let text_texture, text_rect, text_width =
              get_text_texture ctx path
            in
            Prettree.make
              (size *. text_width, size)
              (fun pos t ->
                render_rect ctx.r purple size
                  (text_texture, text_rect, text_width)
                  current pos t)
        | Tree (Some l) ->
            Prettree.vert
            @@
            let open Prettree.Syntax in
            let+ parent =
              let text_texture, text_rect, text_width =
                get_text_texture ctx path
              in
              Prettree.make
                (size *. text_width, size)
                (fun pos t ->
                  render_rect ctx.r purple size
                    (text_texture, text_rect, text_width)
                    current pos t)
            and+ () = Prettree.padding 1.
            and+ l = horz (list ~padding:size (List.map layout_rec l)) in
            fun scale ->
              let parent_pos = parent scale in
              List.iter
                (fun child -> render_link ctx.r parent_pos (child scale))
                l;
              parent_pos)
  in
  Loading.set_state loading Gen_layout;
  layout_rec
