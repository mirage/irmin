# Usage: gnuplot -e "datafile='path/to/data.csv'; outfile='path/to/output.png'" bench.gp
# Or run from bench_multicore directory with default files

if (!exists("datafile")) datafile = 'bench.csv'
if (!exists("outfile")) outfile = 'bench.png'

set terminal pngcairo size 800,600 enhanced font 'Verdana,10'
set output outfile
set datafile separator ","
set title 'Task runtimes and system energy consumption per domains'
set xlabel 'Domains'
set ylabel 'Task runtimes (ms)'
set y2label 'Energy consumed (J)'
set y2tics
set grid
set key top right
plot datafile using 1:2:3:4 with yerrorbars title 'Min/Max time' lc rgb 'black' pt 0 lw 1, \
     datafile using 1:2 with points title 'Median time' lc rgb 'red' pt 5 ps 1, \
     datafile using 1:8 axes x1y2 with points title 'Energy' lc rgb 'green' pt 5 ps 1
