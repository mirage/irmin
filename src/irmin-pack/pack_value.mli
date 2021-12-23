(** This module defines abstractions over entries in the pack file, which are
    encoded as follows:

    {v
      ┌────────┬────────┬──────────────┬─────────┐
      │  Hash  │  Kind  │  Len(Value)? │  Value  │
      └────────┴────────┴──────────────┴─────────┘
      ┆<┄ H ┄┄>┆<┄ K ┄┄>┆<┄┄┄┄ L? ┄┄┄┄>┆<┄┄ V ┄┄>┆
      ┆<┄┄┄┄┄┄┄┄┄┄┄ entry length, E ┄┄┄┄┄┄┄┄┄┄┄┄>┆
    v}

    The fields are as follows:

    - [Hash]: the (fixed-length) hash of the data stored in this entry;

    - [Kind]: the {i kind} of data being stored (contents, nodes, commits etc.),
      encoded as a single "magic" character;

    - [Len(Value)]: an optional length header for the [Value] section of the
      entry ({i not} including the length of the length header itself), encoded
      using a variable-length encoding (LEB128). The presence of a length header
      is determined by the [Kind] character.

    - [Value]: the data itself.

    The length of the overall pack {i entry}, as referenced in the {!Pack_index}
    or in a direct {!Pack_key.t}, is equal to [E = H + K + L + V]. *)

include Pack_value_intf.Sigs
(** @inline *)
