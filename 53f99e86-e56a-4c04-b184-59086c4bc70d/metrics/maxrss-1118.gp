
set title 'maxrss'
set xlabel 'Time (ns)'
set ylabel "maxrss (MiB)"
set datafile separator ","
set grid
set term png
set output 'out/maxrss-1118.png'
plot '/home/cuihtlauac/caml/irmin-eio/_metrics/53f99e86-e56a-4c04-b184-59086c4bc70d/metrics/bench.data' using 1:4 t "maxrss"
