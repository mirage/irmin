val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val pp_results :
  (string, (string, Bechamel.Analyze.OLS.t) Hashtbl.t) Hashtbl.t Fmt.t
