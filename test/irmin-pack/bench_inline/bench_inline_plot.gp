# Gnuplot script for inline contents benchmark comparison
set terminal pngcairo size 1200,700 enhanced font 'Arial,12'
set output 'bench_inline_comparison.png'

set title "Read Latency p99 (µs): No Inlining vs Inlining (threshold=48 bytes)" font 'Arial,14'
set xlabel "Distribution" offset 0,-2
set ylabel "Read p99 Latency (µs)"

set style data histogram
set style histogram cluster gap 1
set style fill solid 0.8 border -1
set boxwidth 0.9

# Offset xtic labels down and to the right to avoid bar overlap
set xtics rotate by -45 right offset -1,-0.5 nomirror
set bmargin 10
set grid ytics
set key top right

set yrange [0:220]

# Colors: blue for no-inline, green for inline
set style line 1 lc rgb '#4472C4'
set style line 2 lc rgb '#70AD47'

plot 'bench_inline_plot.dat' using 2:xtic(1) title 'No Inlining' ls 1, \
     '' using 3 title 'Inlining (48 bytes)' ls 2
