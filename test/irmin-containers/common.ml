open Lwt.Infix

let merge_into_exn (type s) (module S : Irmin.S with type t = s) store ~into =
  S.merge_into ~info:Irmin.Info.none store ~into >|= function
  | Error (`Conflict msg) ->
      Alcotest.failf "Encountered a conflict while merging: %s" msg
  | Ok () -> ()
