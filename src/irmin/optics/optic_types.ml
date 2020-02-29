open Brands

type ('s, 't, 'a, 'b, 'm, 'feat) t = {
  f : 'w. ('feat, 'w) Dictionary.t -> ('a, 'b, 'w) app2 -> ('s, 't, 'w) app2;
}

module Aliases = struct
  open Subtyping

  type nonrec ('s, 't, 'a, 'b, 'm) lens = ('s, 't, 'a, 'b, 'm, lens) t

  type nonrec ('s, 't, 'a, 'b, 'm) prism = ('s, 't, 'a, 'b, 'm, prism) t

  type nonrec ('s, 't, 'a, 'b, 'm) optional = ('s, 't, 'a, 'b, 'm, optional) t

  module Mono = struct
    type nonrec ('s, 'a, 'm, 'feat) t = ('s, 's, 'a, 'a, 'm, 'feat) t

    type nonrec ('s, 'a, 'm) lens = ('s, 's, 'a, 'a, 'm) lens

    type nonrec ('s, 'a, 'm) prism = ('s, 's, 'a, 'a, 'm) prism

    type nonrec ('s, 'a, 'm) optional = ('s, 's, 'a, 'a, 'm) optional
  end
end
