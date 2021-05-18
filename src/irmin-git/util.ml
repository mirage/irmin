let append_string s byt off =
  let ls = String.length s in
  Bytes.blit_string s 0 byt off ls;
  off + ls
