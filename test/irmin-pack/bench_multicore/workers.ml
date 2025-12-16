let run ~d_mgr ~nb ?(finally = Fun.id) tasks =
  let sem = Eio.Semaphore.make 0 in
  let at = Atomic.make 0 in
  let worker () =
    Eio.Semaphore.acquire sem;
    let rec go ~count () =
      let i = Atomic.fetch_and_add at 1 in
      if i >= Array.length tasks then ( (* Format.printf "did %#i@." count; *) )
      else
        let task = tasks.(i) in
        task ();
        go ~count:(count + 1) ()
    in
    go ~count:0 ()
  in
  Eio.Switch.run @@ fun sw ->
  let fibers =
    worker
    :: List.init (nb - 1) (fun _ ->
           let mut = Eio.Semaphore.make 0 in
           ( Eio.Fiber.fork ~sw @@ fun () ->
             Eio.Domain_manager.run d_mgr @@ fun () ->
             worker ();
             Eio.Semaphore.release mut );
           fun () -> Eio.Semaphore.acquire mut)
  in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to nb do
    Eio.Semaphore.release sem
  done;
  Eio.Fiber.all fibers;
  finally ();
  let t1 = Unix.gettimeofday () in
  let elapsed = 1000.0 *. (t1 -. t0) in
  elapsed
