#!/bin/zsh

# Compile and build every file in the directory
# You will need irmin, git,lwt

for i in *.ml do
    ocamlbuild -pkgs lwt.unix,irmin.unix,git -r ${i/%.ml/.byte}
done
