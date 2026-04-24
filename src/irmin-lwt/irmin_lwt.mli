(** Lwt compatibility layer for Irmin 4.

    This package lets Irmin 3 (Lwt-based) consumers continue to use a monadic
    [Lwt.t] API while the backend is Irmin 4 (direct-style Eio). It is a
    transitional shim: new code should use [Irmin] directly.

    See [doc/migration-from-irmin-3.md] for a migration walkthrough. *)
