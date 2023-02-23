module Int63 = struct
  include Optint.Int63

  let t = Irmin.Type.int63
end
