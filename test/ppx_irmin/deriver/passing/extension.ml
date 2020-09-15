type 'a typ = 'a Irmin.Type.t

module Simple = struct
  let (_ : (int * string) list typ) = [%typ: (int * string) list]
end

module Alias = struct
  type t = unit

  let t = Irmin.Type.unit

  let (_ : unit typ) = [%typ: t]
end

module Sum = struct
  let (_ : [ `Foo | `Bar of string ] typ) = [%typ: [ `Foo | `Bar of string ]]
end

module Params = struct
  let __ : type a. a typ -> a list typ = [%typ: 'a list]

  let __ : type a b. a typ -> b typ -> (a * b * a) typ = [%typ: 'a * _ * 'a]

  let __ : type a b. a typ -> b typ -> (a, b) result typ = [%typ: (_, _) result]
end

module Namespace = struct
  let (_ : string typ) = [%irmin.typ: string]
end
