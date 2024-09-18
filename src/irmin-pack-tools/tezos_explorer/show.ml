open Notty
open Notty_unix
open Import
open Files
module Files = Make (Irmin_tezos.Conf) (Irmin_tezos.Schema)

type entry_content = {
  hash : string;
  kind : Kind.t;
  off : Int63.t;
  length : int;
  contents : string;
}

type entry_ctx = { last : info; next : info }
and info = { commit : int list; contents : int list; inode : int list }

type idxs = { entry : int; off_info : off_info; off_pack : Int63.t }
and off_info = { off_last : Int63.t; off_next : Int63.t }

type history = { entry : int; off : Int63.t; kind : Kind.t }

type context = {
  info_last_fd : Unix.file_descr;
  info_next_fd : Unix.file_descr;
  idxs : idxs list;
  fm : Files.File_manager.t;
  dispatcher : Files.Dispatcher.t;
  dict : Files.File_manager.Dict.t;
  max_entry : int;
  max_offset : Int63.t;
  history : history Ring.t;
  mutable entry : int;
  mutable entry_ctx : entry_ctx;
  mutable entry_content : entry_content;
  mutable x : int;
  mutable y : int;
}

let buffer = Bytes.create (4096 * 4096)

let get_entry c off =
  let hash, kind, length, contents =
    Files.decode_entry c.dispatcher buffer off
  in
  let hash = Result.get_ok @@ Base64.encode hash in
  { hash; kind; off; length; contents }

let read ~fd ~buffer ~fd_offset ~buffer_offset ~length =
  let rec aux fd_offset buffer_offset length read_count =
    let r =
      Index_unix.Syscalls.pread ~fd ~fd_offset ~buffer ~buffer_offset ~length
    in
    let read_count = read_count + r in
    if r = 0 then read_count (* end of file *)
    else if r = length then read_count
    else
      (aux [@tailcall])
        (Int63.add fd_offset (Int63.of_int r))
        (buffer_offset + r) (length - r) read_count
  in
  aux fd_offset buffer_offset length 0

let load_idxs fd =
  let idx = ref 0 in
  let _ =
    read ~fd ~buffer ~fd_offset:Int63.zero ~buffer_offset:0
      ~length:Varint.max_encoded_size
  in
  let buf = Bytes.unsafe_to_string buffer in
  let max_entry = Varint.decode_bin buf idx in
  let idxs =
    List.init max_entry (fun i ->
        let _ =
          read ~fd ~buffer ~fd_offset:(Int63.of_int !idx) ~buffer_offset:0
            ~length:(Varint.max_encoded_size * 3)
        in
        let buffer = Bytes.unsafe_to_string buffer in
        let i' = ref 0 in
        let off_last_info = Int63.of_int @@ Varint.decode_bin buffer i' in
        let off_next_info = Int63.of_int @@ Varint.decode_bin buffer i' in
        let off_pack = Int63.of_int @@ Varint.decode_bin buffer i' in
        idx := !i' + !idx;
        let off_info = { off_last = off_last_info; off_next = off_next_info } in
        { entry = i; off_info; off_pack })
  in
  (max_entry, idxs)

let load_entry fd_last fd_next off_info =
  let _ =
    read ~fd:fd_last ~buffer ~fd_offset:off_info.off_last ~buffer_offset:0
      ~length:(Varint.max_encoded_size * 7)
  in
  let buf = Bytes.unsafe_to_string buffer in
  let idx = ref 0 in
  let flag = Varint.decode_bin buf idx in
  let n = Int.logand flag 0b11 in
  let commit = List.init n (fun _ -> Varint.decode_bin buf idx) in
  let n = Int.shift_right_logical (Int.logand flag 0b1100) 2 in
  let contents = List.init n (fun _ -> Varint.decode_bin buf idx) in
  let n = Int.shift_right_logical (Int.logand flag 0b110000) 4 in
  let inode = List.init n (fun _ -> Varint.decode_bin buf idx) in
  let last = { commit; contents; inode } in
  let _ =
    read ~fd:fd_next ~buffer ~fd_offset:off_info.off_next ~buffer_offset:0
      ~length:(Varint.max_encoded_size * 7)
  in
  let buf = Bytes.unsafe_to_string buffer in
  let idx = ref 0 in
  let flag = Varint.decode_bin buf idx in
  let n = Int.logand flag 0b11 in
  let commit = List.init n (fun _ -> Varint.decode_bin buf idx) in
  let n = Int.shift_right_logical (Int.logand flag 0b1100) 2 in
  let contents = List.init n (fun _ -> Varint.decode_bin buf idx) in
  let n = Int.shift_right_logical (Int.logand flag 0b110000) 4 in
  let inode = List.init n (fun _ -> Varint.decode_bin buf idx) in
  let next = { commit; contents; inode } in
  { last; next }

let reload_context c i =
  let idxs = List.nth c.idxs i in
  if i <> c.entry then
    Ring.add c.history
      {
        entry = c.entry;
        off = c.entry_content.off;
        kind = c.entry_content.kind;
      };
  c.entry <- i;
  c.entry_ctx <- load_entry c.info_last_fd c.info_next_fd idxs.off_info;
  c.entry_content <- get_entry c idxs.off_pack

let reload_context_with_off c off =
  let idxs =
    Option.get
    @@ List.find_opt (fun idxs -> Int63.equal off idxs.off_pack) c.idxs
  in
  if idxs.entry <> c.entry then
    Ring.add c.history
      {
        entry = c.entry;
        off = c.entry_content.off;
        kind = c.entry_content.kind;
      };
  c.entry <- idxs.entry;
  c.entry_ctx <- load_entry c.info_last_fd c.info_next_fd idxs.off_info;
  c.entry_content <- get_entry c off

module Menu = struct
  let button_attr b b' =
    match (b, b') with
    | true, true -> A.(bg lightwhite ++ fg black)
    | true, false -> A.(fg lightwhite)
    | false, true -> A.(fg @@ gray 16)
    | false, false -> A.(fg @@ gray 8)

  let button s a h =
    let attr = button_attr a h in
    I.string attr s

  let back_str =
    [|
      ("◂──", "Go back by 1000 ");
      ("◂─", "Go back by 10 ");
      ("◂", "Go back by 1 ");
    |]

  let forth_str =
    [|
      ("▸", "Go forth by 1 ");
      ("─▸", "Go forth by 10 ");
      ("──▸", "Go forth by 1000 ");
    |]

  let gen_entry_buttons r c str =
    let l = [ 1; 10; 1000 ] in
    let f i c = reload_context c i in
    Array.mapi
      (fun i (button_txt, tooltip) ->
        let i = if r then 2 - i else i in
        let tooltip = tooltip ^ if i <> 0 then "entries" else "entry" in
        let i = List.nth l i in
        if r && c.entry - i >= 0 then
          let e = c.entry - i in
          (button button_txt true, f e, tooltip, Some e)
        else if (not r) && c.entry + i < c.max_entry then
          let e = c.entry + i in
          (button button_txt true, f e, tooltip, Some e)
        else (button button_txt false, (fun _ -> ()), tooltip, None))
      str

  let gen_buttons r s l str =
    let f i c = if i <> -1 then reload_context c i in
    Array.mapi
      (fun i (button_txt, tooltip) ->
        let i = if r then 2 - i else i in
        let tooltip = tooltip ^ s ^ if i <> 0 then "s" else "" in
        if i < List.length l then
          let e = List.nth l i in
          (button button_txt true, f e, tooltip, Some e)
        else (button button_txt false, (fun _ -> ()), tooltip, None))
      str

  let text_button s = (button s true, (fun _ -> ()), s, None)

  let b c =
    [|
      Array.concat
        [
          gen_entry_buttons true c back_str;
          [| text_button "Entry  " |];
          gen_entry_buttons false c forth_str;
        ];
      Array.concat
        [
          gen_buttons true "commit" c.entry_ctx.last.commit back_str;
          [| text_button "Commit " |];
          gen_buttons false "commit" c.entry_ctx.next.commit forth_str;
        ];
      Array.concat
        [
          gen_buttons true "content" c.entry_ctx.last.contents back_str;
          [| text_button "Content" |];
          gen_buttons false "content" c.entry_ctx.next.contents forth_str;
        ];
      Array.concat
        [
          gen_buttons true "inode" c.entry_ctx.last.inode back_str;
          [| text_button "Inode  " |];
          gen_buttons false "inode" c.entry_ctx.next.inode forth_str;
        ];
    |]

  let buttons b ~x_off ~y_off x y =
    let _, b =
      Array.fold_left
        (fun (y', acc) a ->
          let l =
            List.rev
            @@ snd
            @@ Array.fold_left
                 (fun (x', acc) (f, _, _, _) ->
                   (x' + 1, (f (x' = x && y' = y) |> I.pad ~l:1 ~t:0) :: acc))
                 (0, []) a
          in
          (y' + 1, I.hcat l :: acc))
        (0, []) b
    in
    I.(pad ~l:x_off ~t:y_off @@ vcat (List.rev b))

  let bound m x = (x + m) mod m

  let move b c = function
    | `Left -> c.x <- bound (Array.length b.(c.y)) (c.x - 1)
    | `Right -> c.x <- bound (Array.length b.(c.y)) (c.x + 1)
    | `Up ->
        c.y <- bound (Array.length b) (c.y - 1);
        c.x <- bound (Array.length b.(c.y)) c.x
    | `Down ->
        c.y <- bound (Array.length b) (c.y + 1);
        c.x <- bound (Array.length b.(c.y)) c.x
end

module Button = struct
  type 'a t = { x : int; y : int; w : int; h : int; f : 'a }

  let on_press (x, y) b =
    if x >= b.x && x < b.x + b.w && y >= b.y && y < b.y + b.h then Some b.f
    else None

  let pad b x y = { b with x = b.x + x; y = b.y + y }
end

let box h w =
  let open I in
  let bar = String.concat "" @@ List.init (w + 2) (fun _ -> "━") in
  let t_bar = "┏" ^ bar ^ "┓" in
  let m_bar = "┃" ^ String.make (w + 2) ' ' ^ "┃" in
  let b_bar = "┗" ^ bar ^ "┛" in
  let middle =
    I.vcat (List.init h (fun _ -> string A.(fg white ++ st bold) m_bar))
  in
  string A.(fg white ++ st bold) t_bar
  <-> middle
  <-> string A.(fg white ++ st bold) b_bar

let double_box h1 h2 w =
  let open I in
  let bar = String.concat "" @@ List.init (w + 2) (fun _ -> "━") in
  let t_bar = "┏" ^ bar ^ "┓" in
  let m_bar = "┃" ^ String.make (w + 2) ' ' ^ "┃" in
  let mf_bar = "┣" ^ bar ^ "┫" in
  let b_bar = "┗" ^ bar ^ "┛" in
  let middle1 =
    I.vcat (List.init h1 (fun _ -> string A.(fg white ++ st bold) m_bar))
  in
  let middle2 =
    I.vcat (List.init h2 (fun _ -> string A.(fg white ++ st bold) m_bar))
  in
  string A.(fg white ++ st bold) t_bar
  <-> middle1
  <-> string A.(fg white ++ st bold) mf_bar
  <-> middle2
  <-> string A.(fg white ++ st bold) b_bar

let position_text c i =
  match i with
  | None -> I.empty
  | Some i ->
      let d = i - c.entry in
      let entry_txt = if d = -1 || d = 1 then "entry" else "entries" in
      let { off_pack; _ } = List.nth c.idxs i in
      let content = get_entry c off_pack in
      let open I in
      let color, text =
        match content.kind with
        | Commit_v1 | Commit_v2 -> (A.red, "Commit")
        | Dangling_parent_commit -> (A.magenta, "Dangling commit")
        | Contents -> (A.lightblue, "Contents")
        | Inode_v1_unstable | Inode_v1_stable | Inode_v2_root | Inode_v2_nonroot
          ->
            (A.green, "Inode")
      in
      let arrow =
        if d < 0 then
          string A.(fg color ++ st bold) text
          <|> string A.(fg lightwhite ++ st bold) " ◀━━━▪"
        else
          string A.(fg lightwhite ++ st bold) "▪━━━▶ "
          <|> string A.(fg color ++ st bold) text
      in
      arrow
      <-> void 0 1
      <-> strf ~attr:A.(fg lightwhite ++ st bold) "by %#d %s" (abs d) entry_txt
      <-> void 0 1
      <-> strf
            ~attr:A.(fg lightwhite ++ st bold)
            "to offset %#d" (Int63.to_int off_pack)
      |> pad ~l:30 ~t:1

let get_parent_commit c i =
  let { off_info; _ } = List.nth c.idxs i in
  let ctx = load_entry c.info_last_fd c.info_next_fd off_info in
  List.nth ctx.next.commit 0

let show_commit c (commit : Files.Commit.Commit_direct.t) =
  let open I in
  let node_txt = string A.(fg lightred ++ st bold) "Node:" in
  let addr_show ?(parent = true) (addr : Files.Commit.Commit_direct.address) =
    let parent_commit ~parent c i =
      if parent then
        let parent_commit = get_parent_commit c i in
        let img =
          if c.entry == parent_commit then
            strf ~attr:A.(fg lightgreen ++ st bold) "(Added by current commit)"
          else
            strf
              ~attr:A.(fg lightmagenta ++ st bold)
              "(Added by commit at entry %#d)" parent_commit
        in
        ( img,
          [
            Button.
              {
                x = 0;
                y = 1;
                w = I.width img;
                h = 1;
                f = (fun c -> reload_context c parent_commit);
              };
          ] )
      else (empty, [])
    in
    match addr with
    | Offset addr -> (
        let hit_or_miss =
          List.find_opt
            (fun { off_pack; _ } -> Int63.equal addr off_pack)
            c.idxs
        in
        match hit_or_miss with
        | None ->
            ( I.strf
                ~attr:A.(fg lightwhite ++ st bold)
                "Dangling entry (off %#d)" (Int63.to_int addr),
              [] )
        | Some { entry; off_pack; _ } ->
            let img, parent_img, parent_buttons =
              let content = get_entry c off_pack in
              let open I in
              let color, text =
                match content.kind with
                | Commit_v1 | Commit_v2 -> (A.red, "Commit")
                | Dangling_parent_commit -> (A.magenta, "Dangling commit")
                | Contents -> (A.lightblue, "Contents")
                | Inode_v1_unstable | Inode_v1_stable | Inode_v2_root
                | Inode_v2_nonroot ->
                    (A.green, "Inode")
              in
              let parent_img, parent_buttons = parent_commit ~parent c entry in
              ( strf ~attr:A.(fg lightwhite ++ st bold) "Entry %#d (" entry
                <|> string A.(fg color ++ st bold) text
                <|> strf
                      ~attr:A.(fg lightwhite ++ st bold)
                      ", off %#d)" (Int63.to_int addr),
                parent_img,
                parent_buttons )
            in
            ( img <-> parent_img,
              Button.
                {
                  x = 0;
                  y = 0;
                  w = I.width img;
                  h = 1;
                  f = (fun c -> reload_context_with_off c addr);
                }
              :: parent_buttons ))
    | Hash _hash ->
        (I.string A.(fg lightwhite ++ st bold) "Hash <should not happen>", [])
  in
  let node, node_button = addr_show commit.node_offset in
  let parents_txt = I.string A.(fg lightred ++ st bold) "Parents:" in
  let parents, parents_buttons =
    match commit.parent_offsets with
    | [] -> (I.string A.(fg lightwhite ++ st bold) "none", [])
    | parents ->
        let l_img, l_buttons =
          List.split
            (List.mapi
               (fun i addr ->
                 let node, node_button = addr_show ~parent:false addr in
                 (node, List.map (fun b -> Button.pad b 0 i) node_button))
               parents)
        in
        (I.hcat l_img, l_buttons)
  in
  let info_txt = I.string A.(fg lightred ++ st bold) "Info:" in
  let info = commit.info in
  let date =
    Option.get
    @@ Ptime.of_span
    @@ Ptime.Span.of_int_s (Int64.to_int @@ Files.Store.Info.date info)
  in
  let info =
    let open I in
    string A.(fg lightwhite ++ st bold) "Author:"
    <-> string A.(fg lightwhite ++ st bold) "Message:"
    <-> string A.(fg lightwhite ++ st bold) "Date:"
    <|> void 1 0
    <|> (string A.(fg lightwhite ++ st bold) (Files.Store.Info.author info)
        <-> string A.(fg lightwhite ++ st bold) (Files.Store.Info.message info)
        <-> strf
              ~attr:A.(fg lightwhite ++ st bold)
              "%a" (Ptime.pp_human ()) date)
  in
  let open I in
  let img = node_txt <-> (void 2 0 <|> node) <-> void 0 1 <-> parents_txt in
  ( img
    <-> (void 2 0 <|> parents)
    <-> void 0 1
    <-> info_txt
    <-> (void 2 0 <|> info),
    List.append
      (List.map (fun b -> Button.pad b 2 1) node_button)
      (List.map
         (fun b -> Button.pad b 2 (I.height img))
         (List.flatten parents_buttons)) )

let show_inode c (inode : Files.Inode.compress) =
  let open I in
  let addr_show (addr : Files.Inode.Compress.address) =
    let parent_commit c i =
      let current_parent_commit = List.nth c.entry_ctx.next.commit 0 in
      let parent_commit = get_parent_commit c i in
      let img =
        if current_parent_commit == parent_commit then
          strf ~attr:A.(fg lightgreen ++ st bold) "(Added by parent commit)"
        else
          strf
            ~attr:A.(fg lightmagenta ++ st bold)
            "(Added by commit at entry %#d)" parent_commit
      in
      ( img,
        [
          Button.
            {
              x = 0;
              y = 1;
              w = I.width img;
              h = 1;
              f = (fun c -> reload_context c parent_commit);
            };
        ] )
    in
    match addr with
    | Offset addr ->
        let hit_or_miss =
          List.find_opt
            (fun { off_pack; _ } -> Int63.equal addr off_pack)
            c.idxs
        in
        let img, parent_img, parent_buttons =
          match hit_or_miss with
          | None ->
              ( I.strf
                  ~attr:A.(fg lightwhite ++ st bold)
                  "Dangling entry (off %#d)" (Int63.to_int addr),
                I.empty,
                [] )
          | Some { entry; off_pack; _ } ->
              let content = get_entry c off_pack in
              let open I in
              let color, text =
                match content.kind with
                | Commit_v1 | Commit_v2 -> (A.red, "Commit")
                | Dangling_parent_commit -> (A.magenta, "Dangling commit")
                | Contents -> (A.lightblue, "Contents")
                | Inode_v1_unstable | Inode_v1_stable | Inode_v2_root
                | Inode_v2_nonroot ->
                    (A.green, "Inode")
              in
              let parent_img, parent_buttons = parent_commit c entry in
              ( I.strf ~attr:A.(fg lightwhite ++ st bold) "Entry %#d (" entry
                <|> I.string A.(fg color ++ st bold) text
                <|> I.strf
                      ~attr:A.(fg lightwhite ++ st bold)
                      ", off %#d)" (Int63.to_int addr),
                parent_img,
                parent_buttons )
        in

        ( img <-> parent_img,
          Button.
            {
              x = 0;
              y = 0;
              w = I.width img;
              h = 1;
              f = (fun c -> reload_context_with_off c addr);
            }
          :: parent_buttons )
    | Hash _hash ->
        (I.string A.(fg lightwhite ++ st bold) "Hash <should not happen>", [])
  in
  let name (n : Files.Inode.Compress.name) =
    match n with
    | Indirect dict_key ->
        let key = Files.File_manager.Dict.find c.dict dict_key in
        strf
          ~attr:A.(fg lightwhite ++ st bold)
          "Indirect key: \'%a\' (%#d)" (Fmt.option Fmt.string) key dict_key
    | Direct step ->
        strf ~attr:A.(fg lightwhite ++ st bold) "Direct key: %s" step
  in
  let value i (v : Files.Inode.Compress.value) =
    let v, v_buttons =
      match v with
      | Contents (n, addr, ()) ->
          let content, content_button = addr_show addr in
          let img1 = string A.(fg lightred ++ st bold) "Contents:" in
          let img2 = name n in
          ( img1 <-> (void 2 0 <|> (img2 <-> content)),
            List.map
              (fun b -> Button.pad b 2 (I.height img1 + I.height img2))
              content_button )
      | Node (n, addr) ->
          let node, node_button = addr_show addr in
          let img1 = string A.(fg lightred ++ st bold) "Node:" in
          let img2 = name n in
          ( img1 <-> (void 2 0 <|> (img2 <-> node)),
            List.map
              (fun b -> Button.pad b 2 (I.height img1 + I.height img2))
              node_button )
    in
    let img = strf ~attr:A.(fg lightred ++ st bold) "Value %#d:" i in
    ( img <-> (void 2 0 <|> v),
      List.map (fun b -> Button.pad b 2 (I.height img)) v_buttons )
  in
  let ptr i (p : Files.Inode.Compress.ptr) =
    let ptr, ptr_button = addr_show p.hash in
    let img = strf ~attr:A.(fg lightred ++ st bold) "Ptr %#d:" i <|> void 2 0 in
    (img <|> ptr, List.map (fun b -> Button.pad b (I.width img) i) ptr_button)
  in
  let tree (t : Files.Inode.Compress.tree) =
    let t_img, t_buttons = List.split (List.mapi ptr t.entries) in
    let img =
      string A.(fg lightred ++ st bold) "Tree:"
      <-> (void 2 0
          <|> strf ~attr:A.(fg lightwhite ++ st bold) "Depth: %#d" t.depth)
    in
    ( img <-> vcat t_img,
      List.map (fun b -> Button.pad b 0 (I.height img)) (List.flatten t_buttons)
    )
  in
  let v (tv : Files.Inode.Compress.v) s =
    let tv, tv_buttons =
      match tv with
      | Values l ->
          let v, v_buttons = List.split (List.mapi value l) in
          let _, v_buttons =
            List.fold_left2
              (fun (i, acc) img b ->
                (i + I.height img, List.map (fun b -> Button.pad b 0 i) b :: acc))
              (0, []) v v_buttons
          in
          (vcat v, List.flatten v_buttons)
      | Tree t -> tree t
    in
    let img =
      string A.(fg lightred ++ st bold) "Tagged:"
      <-> (void 2 0 <|> string A.(fg lightwhite ++ st bold) s)
      <-> void 0 1
    in
    (img <-> tv, List.map (fun b -> Button.pad b 0 (I.height img)) tv_buttons)
  in
  match inode.tv with
  | V1_stable tv -> v tv "Stable"
  | V1_unstable tv -> v tv "Unstable"
  | V2_root tv -> v tv.v "Root"
  | V2_nonroot tv -> v tv.v "Non root"

let kind_color (kind : Kind.t) =
  match kind with
  | Commit_v1 | Commit_v2 -> A.red
  | Dangling_parent_commit -> A.magenta
  | Contents -> A.lightblue
  | Inode_v1_unstable | Inode_v1_stable | Inode_v2_root | Inode_v2_nonroot ->
      A.green

let show_entry_content ~x_off ~y_off c =
  let open I in
  let hash =
    I.string A.(fg lightred ++ st bold) "Hash:"
    <-> (void 2 0 <|> I.string A.(fg lightwhite ++ st bold) c.entry_content.hash)
  in
  let kind =
    I.string A.(fg lightred ++ st bold) "Kind:"
    <-> (void 2 0
        <|> I.strf
              ~attr:A.(fg (kind_color c.entry_content.kind) ++ st bold)
              "%a" Kind.pp c.entry_content.kind)
  in
  match c.entry_content.kind with
  | Inode_v2_root | Inode_v2_nonroot ->
      let decoded = I.string A.(fg lightred ++ st bold) "Decoded:" in
      let inode, inode_buttons =
        show_inode c
        @@ Files.Inode.decode_bin_compress c.entry_content.contents (ref 0)
      in
      let hex = I.string A.(fg lightred ++ st bold) "Hexdump:" in
      let entry_header = Files.Hash.hash_size + 1 in
      let contents_len =
        String.length c.entry_content.contents - entry_header
      in
      let contents =
        String.sub c.entry_content.contents entry_header contents_len
      in
      let contents = Hex.hexdump_s @@ Hex.of_string contents in
      let contents = String.split_on_char '\n' contents in
      let entry_hexdump =
        I.vcat
        @@ List.map
             (fun s ->
               let s = Printf.sprintf "%S" s in
               I.string A.(fg lightwhite ++ st bold) s)
             contents
      in
      let img = hash <-> void 0 1 <-> kind <-> void 0 1 <-> decoded in
      ( img
        <-> (void 2 0 <|> inode)
        <-> void 0 1
        <-> hex
        <-> (void 2 0 <|> entry_hexdump)
        |> I.pad ~l:x_off ~t:y_off,
        List.map
          (fun b -> Button.pad b (x_off + 2) (y_off + I.height img))
          inode_buttons )
  | Commit_v2 | Dangling_parent_commit ->
      let open I in
      let entry_header = Files.Hash.hash_size + 2 in
      let contents_len =
        String.length c.entry_content.contents - entry_header
      in
      let contents =
        String.sub c.entry_content.contents entry_header contents_len
      in
      let commit, commit_button =
        show_commit c @@ Files.Commit.decode_bin_compress contents (ref 0)
      in
      let decoded = I.string A.(fg lightred ++ st bold) "Decoded:" in
      let hex = I.string A.(fg lightred ++ st bold) "Hexdump:" in
      let contents = Hex.hexdump_s @@ Hex.of_string contents in
      let contents = String.split_on_char '\n' contents in
      let entry_hexdump =
        I.vcat
        @@ List.map
             (fun s ->
               let s = Printf.sprintf "%S" s in
               I.string A.(fg lightwhite ++ st bold) s)
             contents
      in
      let img = hash <-> void 0 1 <-> kind <-> void 0 1 <-> decoded in
      ( img
        <-> (void 2 0 <|> commit)
        <-> void 0 1
        <-> hex
        <-> (void 2 0 <|> entry_hexdump)
        |> I.pad ~l:x_off ~t:y_off,
        List.map
          (fun b -> Button.pad b (x_off + 2) (y_off + I.height img))
          commit_button )
  | _ ->
      let entry_header = Files.Hash.hash_size + 1 in
      let contents_len =
        String.length c.entry_content.contents - entry_header
      in
      let contents =
        String.sub c.entry_content.contents entry_header contents_len
      in
      let decoded = I.string A.(fg lightred ++ st bold) "Decoded:" in
      let entry_decoded = I.string A.(fg lightwhite ++ st bold) "n/a" in
      let hex = I.string A.(fg lightred ++ st bold) "Hexdump:" in
      let contents = Hex.hexdump_s @@ Hex.of_string contents in
      let contents = String.split_on_char '\n' contents in
      let entry_hexdump =
        I.vcat
        @@ List.map
             (fun s ->
               let s = Printf.sprintf "%S" s in
               I.string A.(fg lightwhite ++ st bold) s)
             contents
      in
      ( hash
        <-> void 0 1
        <-> kind
        <-> void 0 1
        <-> decoded
        <-> (void 2 0 <|> entry_decoded)
        <-> void 0 1
        <-> hex
        <-> (void 2 0 <|> entry_hexdump)
        |> I.pad ~l:x_off ~t:y_off,
        [] )

let show_history c =
  let open I in
  let f = function
    | None -> (string A.(fg black) "           ", [])
    | Some { entry; off; kind } ->
        let color, text =
          match kind with
          | Commit_v1 | Commit_v2 -> (A.red, "Commit")
          | Dangling_parent_commit -> (A.magenta, "Dangling commit")
          | Contents -> (A.lightblue, "Contents")
          | Inode_v1_unstable | Inode_v1_stable | Inode_v2_root
          | Inode_v2_nonroot ->
              (A.green, "Inode")
        in
        let img =
          string A.(fg color ++ st bold) text
          <|> I.strf
                ~attr:A.(fg lightwhite ++ st bold)
                " (entry %#d, offset %#d)" entry (Int63.to_int off)
        in
        ( img,
          [
            Button.
              {
                x = 0;
                y = 0;
                w = I.width img;
                h = 1;
                f = (fun c -> reload_context c entry);
              };
          ] )
  in
  let history, history_buttons =
    List.map f (Ring.to_list c.history) |> List.split
  in
  let history_text = vcat history in
  let history_box = double_box 1 (height history_text) (width history_text) in
  ( void 2 1
    <|> (void 1 1
        <-> string A.(fg lightwhite ++ st bold) "History"
        <-> void 1 1
        <-> history_text)
    </> history_box,
    List.mapi (fun i b -> Button.pad b 2 (i + 3)) (List.flatten history_buttons)
  )

let entry_pos c l t =
  let open I in
  string A.(fg lightyellow ++ st bold) "Entry:"
  <|> void 1 0
  <|> strf ~attr:A.(fg lightwhite ++ st bold) "%#d/%#d" c.entry (c.max_entry - 1)
  </> void 30 0
  <|> string A.(fg lightyellow ++ st bold) "Offset:"
  <|> void 1 0
  <|> strf
        ~attr:A.(fg lightwhite ++ st bold)
        "%#d/%#d"
        (Int63.to_int c.entry_content.off)
        (Int63.to_int (Int63.pred c.max_offset))
  |> pad ~l ~t

let rec loop t c =
  let buttons = Menu.b c in
  let _, _, tooltip, move = buttons.(c.y).(c.x) in
  let menu_text = Menu.buttons buttons ~x_off:1 ~y_off:1 c.x c.y in
  let menu_box =
    double_box (I.height menu_text - 1) 1 (I.width menu_text - 2)
  in
  let tooltip =
    I.string A.(fg lightwhite ++ st bold) tooltip |> I.pad ~l:2 ~t:6
  in
  let history, history_buttons = show_history c in
  let history_pad = fst (Term.size t) - I.width history in
  let history = history |> I.pad ~l:history_pad in
  let history_buttons =
    List.map (fun b -> Button.pad b history_pad 0) history_buttons
  in
  let position_text = position_text c move in
  let position_box = box (I.height menu_text + 1) (I.width position_text - 2) in
  let entries, entries_buttons = show_entry_content ~x_off:2 ~y_off:10 c in
  let l =
    [
      menu_text;
      tooltip;
      menu_box;
      history;
      position_text;
      position_box;
      entries;
      entry_pos c 2 8;
    ]
  in
  let b = I.zcat l in
  Term.image t b;
  match Term.event t with
  | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [ `Ctrl ]) -> ()
  | `Key (`Arrow d, _) ->
      Menu.move buttons c d;
      loop t c
  | `Key (`Enter, _) ->
      let _, f, _, _ = buttons.(c.y).(c.x) in
      f c;
      loop t c
  | `Mouse (`Press _, pos, _) ->
      let l =
        List.filter_map (Button.on_press pos) (entries_buttons @ history_buttons)
      in
      List.iter (fun f -> f c) l;
      loop t c
  | _ -> loop t c

let main ~sw ~fs store_path info_last_path info_next_path index_path =
  let conf = Irmin_pack.Conf.init ~sw ~fs Eio.Path.(fs / store_path) in
  let fm =
    Files.File_manager.open_ro ~sw ~fs conf |> Files.Errs.raise_if_error
  in
  let dispatcher = Files.Dispatcher.v fm |> Files.Errs.raise_if_error in
  let max_offset = Files.Dispatcher.end_offset dispatcher in
  let dict = Files.File_manager.dict fm in
  let info_last_fd =
    Unix.openfile info_last_path Unix.[ O_RDONLY; O_CLOEXEC ] 0o644
  in
  let info_next_fd =
    Unix.openfile info_next_path Unix.[ O_RDONLY; O_CLOEXEC ] 0o644
  in
  let idx_fd = Unix.openfile index_path Unix.[ O_RDONLY; O_CLOEXEC ] 0o644 in
  let max_entry, idxs = load_idxs idx_fd in
  Unix.close idx_fd;
  let history = Ring.make 50 in
  let { entry; off_info; off_pack } = List.hd idxs in
  let entry_ctx = load_entry info_last_fd info_next_fd off_info in
  let entry_content =
    Obj.magic "TODO: cyclical deps between entry_content and context"
  in
  let context =
    {
      info_last_fd;
      info_next_fd;
      idxs;
      fm;
      dispatcher;
      dict;
      max_entry;
      max_offset;
      x = 3;
      y = 0;
      history;
      entry;
      entry_ctx;
      entry_content;
    }
  in
  context.entry_content <- get_entry context off_pack;
  loop (Term.create ()) context;
  Unix.close info_last_fd;
  Unix.close info_next_fd
