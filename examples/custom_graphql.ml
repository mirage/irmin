(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(* Example of using irmin-graphql with custom types *)

open Lwt.Syntax

module Car = struct
  type color = Black | White | Other of string [@@deriving irmin]

  type t = {
    license : string;
    year : int32;
    make_and_model : string * string;
    color : color;
    owner : string;
  }
  [@@deriving irmin]

  let merge = Irmin.Merge.(option (idempotent t))
end

module Store = Irmin_git_unix.Mem.KV (Car)

module Custom_types = struct
  module Defaults = Irmin_graphql.Server.Default_types (Store)
  module Path = Defaults.Path
  module Metadata = Defaults.Metadata
  module Hash = Defaults.Hash
  module Branch = Defaults.Branch
  module Commit_key = Defaults.Commit_key
  module Contents_key = Defaults.Contents_key
  module Node_key = Defaults.Node_key

  module Contents = struct
    open Graphql_lwt

    let color_values =
      Schema.
        [
          enum_value "BLACK" ~value:Car.Black;
          enum_value "WHITE" ~value:Car.White;
        ]

    let schema_typ =
      Schema.(
        obj "Car"
          ~fields:
            [
              field "license" ~typ:(non_null string) ~args:[]
                ~resolve:(fun _ car -> car.Car.license);
              field "year" ~typ:(non_null int) ~args:[] ~resolve:(fun _ car ->
                  Int32.to_int car.Car.year);
              field "make" ~typ:(non_null string) ~args:[]
                ~resolve:(fun _ car -> fst car.Car.make_and_model);
              field "model" ~typ:(non_null string) ~args:[]
                ~resolve:(fun _ car -> snd car.Car.make_and_model);
              field "color" ~typ:(non_null string) ~args:[]
                ~resolve:(fun _ car -> car.Car.license);
              field "owner" ~typ:(non_null string) ~args:[]
                ~resolve:(fun _ car -> car.Car.owner);
            ])

    let color = Schema.Arg.enum "Color" ~values:color_values

    let arg_typ =
      Schema.Arg.(
        obj "CarInput"
          ~fields:
            [
              arg "license" ~typ:(non_null string);
              arg "year" ~typ:(non_null int);
              arg "make" ~typ:(non_null string);
              arg "model" ~typ:(non_null string);
              arg "color" ~typ:(non_null color);
              arg "owner" ~typ:(non_null string);
            ]
          ~coerce:(fun license year make model color owner ->
            {
              Car.license;
              year = Int32.of_int year;
              make_and_model = (make, model);
              color;
              owner;
            }))
  end
end

module Remote = struct
  let remote = Some Store.remote
end

module Server =
  Irmin_graphql_unix.Server.Make_ext (Store) (Remote) (Custom_types)

let main () =
  Eio.Switch.run @@ fun sw ->
  Config.init ();
  let config = Irmin_git.config Config.root in
  let repo = Store.Repo.v ~sw config in
  let server = Server.v repo in
  let src = "localhost" in
  let port = 9876 in
  Lwt_eio.run_lwt @@ fun () ->
  let* ctx = Conduit_lwt_unix.init ~src () in
  let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
  let on_exn exn = Printf.printf "on_exn: %s" (Printexc.to_string exn) in
  Printf.printf "Visit GraphiQL @ http://%s:%d/graphql\n%!" src port;
  Cohttp_lwt_unix.Server.create ~on_exn ~ctx ~mode:(`TCP (`Port port)) server

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ -> main ()
