
set title 'size'
set xlabel 'Time (ns)'
set ylabel "size (MiB)"
set datafile separator ","
set grid
set term png
set output 'out/size-1117.png'
plot '/home/cuihtlauac/caml/irmin-eio/_metrics/53f99e86-e56a-4c04-b184-59086c4bc70d/metrics/bench.data' using 1:3 t "size"
