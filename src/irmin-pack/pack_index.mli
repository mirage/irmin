module Make (K : Irmin.Hash.S) :
  Index.S with type key = K.t and type value = int64 * int * char
