# Gnuplot script for inline contents improvement percentage
set terminal pngcairo size 1000,550 enhanced font 'Arial,12'
set output 'bench_inline_improvement.png'

set title "Read Latency Improvement with Inlining (threshold=48 bytes)" font 'Arial,14'
set xlabel "Distribution" offset 0,-2
set ylabel "Latency Reduction (%)"

set style data histogram
set style histogram cluster gap 1
set style fill solid 0.8 border -1
set boxwidth 0.8

set xtics rotate by -45 right offset -1,-0.5 nomirror
set bmargin 9
set grid ytics
set key off

set yrange [-10:70]

# Color bars based on positive/negative improvement
plot 'bench_inline_plot.dat' using 4:xtic(1) with boxes lc rgb '#70AD47' title 'Improvement %', \
     0 with lines lc rgb '#000000' lw 1 notitle
