(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2015 Mounir Nasr Allah <mounir@nasrallah.co>
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

open Lwt
open Irmin


module Log = Log.Make(struct let section = "AOI" end)


		     
module type LINK = sig
    type key
    type value	   
    type t

    val create: config -> 'a Task.f -> ('a -> t) Lwt.t
    val read: t -> key -> value option Lwt.t
    val add: t -> key -> value -> unit Lwt.t
    val iter: t -> (key -> value Lwt.t -> unit Lwt.t) -> unit Lwt.t
    val mem: t -> key -> bool Lwt.t
  end
			  

module type LINK_MAKER =
  functor (K:Irmin.Hash.S) ->
  LINK with type key = K.t and type value = K.t

					      
module MEM (K: Irmin.Hash.S) = Irmin_mem.AO_LINK(K)

module FS (K: Irmin.Hash.S) = Irmin_unix.Irmin_fs.AO_LINK(K)
				
					      
module AOI (L: LINK_MAKER) (S:AO_MAKER_RAW) (K: Irmin.Hash.S) (V: Tc.S0) = struct
    
    module AO = S(K)(Irmin.Contents.Cstruct)
    module PI = L(K)
		 
    type key = K.t
    type value = V.t
		   
    type t = {ao: AO.t; pi: PI.t}

    let to_cstruct x = Tc.write_cstruct (module V) x
    let of_cstruct x = Tc.read_cstruct (module V) x

    let create config task =
      PI.create config task >>=
	(fun x ->
	 AO.create config task >>=
	   (fun y ->
	    return (fun a -> {ao = y a; pi = x a})))
							    
    let task t =
      AO.task t.ao

    let read t index =
      PI.read t.pi index >>= fun key -> 
      match key with
      | None -> return_none
      | Some k -> AO.read t.ao k >>= function
		  | None -> return_none
		  | Some v -> return (Some (of_cstruct v))
     

    let read_exn t index =
      PI.read t.pi index >>= fun key ->
      match key with 
      | None -> fail Not_found 
      | Some k -> try
		  AO.read_exn t.ao k >>=
		    function v -> return (of_cstruct v)
		with
		| Not_found -> fail Not_found


    let mem t index =
      try
	PI.mem t.pi index
      with
      | Not_found -> return_false 


    let add t v =
      let value = to_cstruct v in
      let index = K.digest value in
      AO.add t.ao value >>=
        (fun x ->
	 let _ = PI.add t.pi index x in
	 Lwt.return index)

	  
    let iter t fn =
      PI.iter t.pi (fun k v ->
	       try 
		 v >>= fun x ->
		 AO.read_exn t.ao x >>= fun y ->
	         let value = of_cstruct y in
		 fn k (return value)
	       with
	       | Not_found -> fail Not_found
	      )
       
end

  
module Make (L: LINK_MAKER) (AO: AO_MAKER_RAW) (RW:RW_MAKER) = Irmin.Make (AOI(L)(AO)) (RW)
