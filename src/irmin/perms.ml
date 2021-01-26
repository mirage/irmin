module Read = struct
  type t = [ `Read ]
end

module Write = struct
  type t = [ `Write ]
end

module Read_write = struct
  type t = [ Read.t | Write.t ]
end

type read = Read.t
type write = Write.t
type read_write = Read_write.t
