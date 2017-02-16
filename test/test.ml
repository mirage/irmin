(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let misc = [
  Test_git.misc;
  Test_link.misc;
]

let suite k =
  [
    `Quick , Test_memory.suite k;
    `Quick , Test_fs.suite k;
    `Quick , Test_git.suite k;
  ]

let () =
  if Array.length Sys.argv = 3 && Sys.argv.(1) = "serve" then
    let n = int_of_string Sys.argv.(2) in
    Logs.set_reporter (Test_common.reporter ~prefix:"S" ());
    Test_http.serve n
  else
    Test_store.run "irmin" ~misc
      (suite `String @ suite `Json @ Test_http.suites)
