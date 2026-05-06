include Store_properties_intf

(* Alias to Irmin (eio)'s [Closed] so exceptions raised by the underlying
   Eio backend through [Lwt_eio.run_eio] match [Lwt.catch] handlers on
   [Irmin_lwt.Closed]. *)
exception Closed = Irmin.Closed
