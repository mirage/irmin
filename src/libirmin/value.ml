module Make (I : Cstubs_inverted.INTERNAL) = struct
  open Util.Make (I)

  let () =
    fn "value_unit" (void @-> returning value) (fun () -> Root.create_value ())

  let () =
    fn "value_int"
      (int64_t @-> returning value)
      (fun i -> Root.create_value (Int64.to_int i))

  let () =
    fn "value_float" (double @-> returning value) (fun i -> Root.create_value i)

  let () =
    fn "value_bool" (bool @-> returning value) (fun b -> Root.create_value b)

  let () =
    fn "value_clone" (value @-> returning value) (fun x -> Root.create_value x)

  let () =
    fn "value_get_string"
      (value @-> returning irmin_string)
      (fun value ->
        let obj = Ctypes.Root.get value |> Obj.repr in
        if Obj.tag obj = Obj.string_tag then Root.create_string (Obj.obj obj)
        else null)

  let () =
    fn "value_get_int"
      (value @-> returning int64_t)
      (fun x ->
        let obj = Ctypes.Root.get x |> Obj.repr in
        if Obj.is_int obj then Int64.of_int (Obj.obj obj) else Int64.zero)

  let () =
    fn "value_get_bool"
      (value @-> returning bool)
      (fun x ->
        let obj = Ctypes.Root.get x |> Obj.repr in
        if Obj.is_int obj then Obj.obj obj else false)

  let () =
    fn "value_get_float"
      (value @-> returning double)
      (fun x ->
        let obj = Ctypes.Root.get x |> Obj.repr in
        if Obj.is_int obj then Obj.obj obj else 0.)

  let () =
    fn "value_bytes"
      (ptr char @-> int64_t @-> returning value)
      (fun s length ->
        let length = get_length length s in
        Root.create_value (Bytes.of_string (string_from_ptr s ~length)))

  let () =
    fn "value_string"
      (ptr char @-> int64_t @-> returning value)
      (fun s length ->
        let length = get_length length s in
        Root.create_value (string_from_ptr s ~length))

  let () =
    fn "value_list_new"
      (void @-> returning value)
      (fun () -> Root.create_value [])

  let () =
    fn "value_list_add"
      (value @-> value @-> returning void)
      (fun (type a) list x ->
        let a : a list = Root.get_value list in
        let b : a = Root.get_value x in
        Root.set_value list (a @ [ b ]))

  let () =
    fn "value_list_hd"
      (value @-> returning value)
      (fun list ->
        let list = Root.get_value list in
        match list with [] -> null | list -> Root.create_value (List.hd list))

  let () =
    fn "value_list_tl"
      (value @-> returning value)
      (fun list ->
        let list = Root.get_value list in
        Root.create_value (List.tl list))

  let () =
    fn "value_list_get"
      (value @-> uint64_t @-> returning value)
      (fun (type a) arr i ->
        let i = UInt64.to_int i in
        let x : a list = Root.get_value arr in
        Root.create_value (List.nth x i))

  let () =
    fn "value_list_length"
      (value @-> returning uint64_t)
      (fun (type a) x ->
        let x : a list = Root.get_value x in
        List.length x |> UInt64.of_int)

  let () =
    fn "value_array_new"
      (uint64_t @-> value @-> returning value)
      (fun i x ->
        let x = Root.get_value x in
        Root.create_value (Array.make (UInt64.to_int i) x))

  let () =
    fn "value_array_set"
      (value @-> uint64_t @-> value @-> returning void)
      (fun (type a) arr i x ->
        let i = UInt64.to_int i in
        let arr : a array = Root.get_value arr in
        let x : a = Root.get_value x in
        arr.(i) <- x)

  let () =
    fn "value_array_get"
      (value @-> uint64_t @-> returning value)
      (fun (type a) arr i ->
        let i = UInt64.to_int i in
        let arr : a array = Root.get_value arr in
        Root.create_value arr.(i))

  let () =
    fn "value_array_length"
      (value @-> returning uint64_t)
      (fun (type a) arr ->
        let arr : a array = Root.get_value arr in
        Array.length arr |> UInt64.of_int)

  let () =
    fn "value_option"
      (value @-> returning value)
      (fun value ->
        if is_null value then Root.create_value None
        else
          let x = Root.get_value value in
          Root.create_value (Some x))

  let () =
    fn "value_pair"
      (value @-> value @-> returning value)
      (fun a b ->
        let a = Root.get_value a in
        let b = Root.get_value b in
        Root.create_value (a, b))

  let () =
    fn "value_triple"
      (value @-> value @-> value @-> returning value)
      (fun a b c ->
        let a = Root.get_value a in
        let b = Root.get_value b in
        let c = Root.get_value c in
        Root.create_value (a, b, c))

  let () =
    fn "value_to_string"
      (ty @-> value @-> returning irmin_string)
      (fun ty value ->
        let t = Root.get_ty ty in
        let v = Root.get_value value in
        let s = Irmin.Type.to_string t v in
        Root.create_string s)

  let () =
    fn "value_of_string"
      (ty @-> ptr char @-> int64_t @-> returning value)
      (fun ty s length ->
        catch' (fun () ->
            let length = get_length length s in
            let ty = Root.get_ty ty in
            let s = string_from_ptr s ~length in
            match Irmin.Type.(of_string ty) s with
            | Ok x -> Root.create_value x
            | Error (`Msg e) ->
                let () = Util.error_msg := Some e in
                null))

  let () =
    fn "value_to_bin"
      (ty @-> value @-> returning irmin_string)
      (fun ty value ->
        catch' (fun () ->
            let t = Root.get_ty ty in
            let v = Root.get_value value in
            let s = Irmin.Type.(unstage (to_bin_string t)) v in
            Root.create_string s))

  let () =
    fn "value_of_bin"
      (ty @-> ptr char @-> int64_t @-> returning value)
      (fun ty s length ->
        catch' (fun () ->
            let length = get_length length s in
            let ty = Root.get_ty ty in
            let s = string_from_ptr s ~length in
            match Irmin.Type.(unstage (of_bin_string ty)) s with
            | Ok x -> Root.create_value x
            | Error (`Msg e) ->
                let () = Util.error_msg := Some e in
                null))

  let () =
    fn "value_to_json"
      (ty @-> value @-> returning irmin_string)
      (fun ty value ->
        catch' (fun () ->
            let t = Root.get_ty ty in
            let v = Root.get_value value in
            let s = Irmin.Type.(to_json_string t) v in
            Root.create_string s))

  let () =
    fn "value_of_json"
      (ty @-> ptr char @-> int64_t @-> returning value)
      (fun ty s length ->
        let length = get_length length s in
        let ty = Root.get_ty ty in
        let s = string_from_ptr s ~length in
        match Irmin.Type.(of_json_string ty) s with
        | Ok x -> Root.create_value x
        | Error (`Msg e) ->
            let () = Util.error_msg := Some e in
            null)

  let () =
    fn "value_equal"
      (ty @-> value @-> value @-> returning bool)
      (fun ty a b ->
        catch false (fun () ->
            let ty = Root.get_ty ty in
            let a = Root.get_value a in
            let b = Root.get_value b in
            Irmin.Type.(unstage (equal ty)) a b))

  let () =
    fn "value_compare"
      (ty @-> value @-> value @-> returning int)
      (fun ty a b ->
        let ty = Root.get_ty ty in
        let a = Root.get_value a in
        let b = Root.get_value b in
        Irmin.Type.(unstage (compare ty)) a b)

  let () = fn "value_free" (value @-> returning void) free

  let () =
    fn "string_new"
      (ptr char @-> int64_t @-> returning irmin_string)
      (fun ptr i ->
        catch' (fun () ->
            let i = Int64.to_int i in
            let length = if i < 0 then strlen ptr else i in
            let s = string_from_ptr ptr ~length in
            Root.create_string s))

  let () =
    fn "string_data"
      (irmin_string @-> returning (ptr char))
      (fun s ->
        if is_null s then coerce (ptr void) (ptr char) null
        else
          let s : string = Root.get_string s in
          coerce string (ptr char) s)

  let () =
    fn "string_length"
      (irmin_string @-> returning uint64_t)
      (fun s ->
        if is_null s then UInt64.zero
        else
          let s : string = Root.get_string s in
          String.length s |> UInt64.of_int)

  let () = fn "string_free" (irmin_string @-> returning void) free
end
