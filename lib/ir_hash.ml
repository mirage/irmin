(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Sexplib.Std

exception Invalid of string

module type S = sig
  include Ir_hum.S
  val digest: Cstruct.t -> t
  val has_kind: [> `SHA1] -> bool
  val to_raw: t -> Cstruct.t
  val of_raw: Cstruct.t -> t
end

(* (from uuidm) *)
(* sha-1 digest. Based on pseudo-code of RFC 3174.
   Slow and ugly but does the job. *)
let sha_1 s =                            
  let sha_1_pad s = 
    let len = String.length s in
    let blen = 8 * len in
    let rem = len mod 64 in
    let mlen = if rem > 55 then len + 128 - rem else len + 64 - rem in
    let m = String.create mlen in 
    String.blit s 0 m 0 len;
    String.fill m len (mlen - len) '\x00';
    m.[len] <- '\x80';
    if Sys.word_size > 32 then begin
      m.[mlen - 8] <- Char.unsafe_chr (blen lsr 56 land 0xFF);
      m.[mlen - 7] <- Char.unsafe_chr (blen lsr 48 land 0xFF);
      m.[mlen - 6] <- Char.unsafe_chr (blen lsr 40 land 0xFF);
      m.[mlen - 5] <- Char.unsafe_chr (blen lsr 32 land 0xFF);
    end;
    m.[mlen - 4] <- Char.unsafe_chr (blen lsr 24 land 0xFF);
    m.[mlen - 3] <- Char.unsafe_chr (blen lsr 16 land 0xFF);
    m.[mlen - 2] <- Char.unsafe_chr (blen lsr 8 land 0xFF);
    m.[mlen - 1] <- Char.unsafe_chr (blen land 0xFF);
    m
  in
  (* Operations on int32 *)
  let ( &&& ) = ( land ) in
  let ( lor ) = Int32.logor in
  let ( lxor ) = Int32.logxor in
  let ( land ) = Int32.logand in
  let ( ++ ) = Int32.add in
  let lnot = Int32.lognot in
  let sr = Int32.shift_right in
  let sl = Int32.shift_left in
  let cls n x = (sl x n) lor (Int32.shift_right_logical x (32 - n)) in
  (* Start *)
  let m = sha_1_pad s in
  let w = Array.make 16 0l in
  let h0 = ref 0x67452301l in
  let h1 = ref 0xEFCDAB89l in
  let h2 = ref 0x98BADCFEl in
  let h3 = ref 0x10325476l in
  let h4 = ref 0xC3D2E1F0l in
  let a = ref 0l in
  let b = ref 0l in
  let c = ref 0l in
  let d = ref 0l in
  let e = ref 0l in
  for i = 0 to ((String.length m) / 64) - 1 do             (* For each block *) 
    (* Fill w *)
    let base = i * 64 in
    for j = 0 to 15 do 
      let k = base + (j * 4) in
      w.(j) <- sl (Int32.of_int (Char.code m.[k])) 24 lor
               sl (Int32.of_int (Char.code m.[k + 1])) 16 lor
               sl (Int32.of_int (Char.code m.[k + 2])) 8 lor
               (Int32.of_int (Char.code m.[k + 3]))
    done;
    (* Loop *)
    a := !h0; b := !h1; c := !h2; d := !h3; e := !h4;
    for t = 0 to 79 do 
      let f, k = 
        if t <= 19 then (!b land !c) lor ((lnot !b) land !d), 0x5A827999l else
        if t <= 39 then !b lxor !c lxor !d, 0x6ED9EBA1l else
        if t <= 59 then 
	  (!b land !c) lor (!b land !d) lor (!c land !d), 0x8F1BBCDCl 
	else
        !b lxor !c lxor !d, 0xCA62C1D6l
      in
      let s = t &&& 0xF in
      if (t >= 16) then begin
	  w.(s) <- cls 1 begin 
	    w.((s + 13) &&& 0xF) lxor 
	    w.((s + 8) &&& 0xF) lxor 
	    w.((s + 2) &&& 0xF) lxor
	    w.(s)
	  end
      end;
      let temp = (cls 5 !a) ++ f ++ !e ++ w.(s) ++ k in
      e := !d;
      d := !c;
      c := cls 30 !b;
      b := !a;
      a := temp;
    done;
    (* Update *)
    h0 := !h0 ++ !a;
    h1 := !h1 ++ !b;
    h2 := !h2 ++ !c;
    h3 := !h3 ++ !d;
    h4 := !h4 ++ !e
  done;
  let h = String.create 20 in
  let i2s h k i =
    h.[k] <- Char.unsafe_chr ((Int32.to_int (sr i 24)) &&& 0xFF);
    h.[k + 1] <- Char.unsafe_chr ((Int32.to_int (sr i 16)) &&& 0xFF);
    h.[k + 2] <- Char.unsafe_chr ((Int32.to_int (sr i 8)) &&& 0xFF);
    h.[k + 3] <- Char.unsafe_chr ((Int32.to_int i) &&& 0xFF);
  in
  i2s h 0 !h0;
  i2s h 4 !h1;
  i2s h 8 !h2;
  i2s h 12 !h3;
  i2s h 16 !h4;
  h

module SHA1 = struct

  type t = Cstruct.t

  let to_hex t =
    let `Hex h = Hex.of_string (Cstruct.to_string t) in
    h

  let len = 20

  let to_raw t = t
  let of_raw t =
    if Cstruct.len t = len then t
    else
      let str = Cstruct.to_string t in
      raise (Invalid (Printf.sprintf "%s (%d)" str (String.length str)))

  let hex_len = 40

  let of_hex hex =
    if String.length hex = hex_len then
      Cstruct.of_string (Hex.to_string (`Hex hex))
    else
      raise (Invalid hex)

  let to_json x = `String (to_hex x)

  let of_json = function
    | `String x -> of_hex x
    | j -> Ezjsonm.parse_error j "Hash.of_json"

  let size_of = Tc.Cstruct.size_of
  let write = Tc.Cstruct.write
  let read buf =
    let t = Tc.Cstruct.read buf in
    if Cstruct.len t <> len then raise (Invalid (Cstruct.to_string t))
    else t

  let hash = Hashtbl.hash
  let equal = (=)
  let compare = Pervasives.compare

  let digest buf = Cstruct.to_string buf |> sha_1 |> Cstruct.of_string
  let to_hum = to_hex
  let of_hum = of_hex

  let has_kind = function
    | `SHA1 -> true
    | _ -> false

end
