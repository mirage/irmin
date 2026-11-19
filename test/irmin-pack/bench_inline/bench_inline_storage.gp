# Gnuplot script for storage impact from inlining
# Usage: gnuplot bench_inline_storage.gp

set terminal pngcairo size 1200,800 enhanced font 'Arial,11'
set output 'bench_inline_storage.png'

set multiplot layout 2,1 title "Storage Consumption Impact from Inlining (threshold=48 bytes)" font 'Arial,14'

# Top plot: Absolute storage comparison
set tmargin at screen 0.92
set bmargin at screen 0.55

set style data histogram
set style histogram clustered gap 1
set style fill solid 0.8 border -1
set boxwidth 0.9

set ylabel "Storage (KB)" font 'Arial,12'
set yrange [0:*]
set grid y
set key top right

set xtics rotate by -45 right font 'Arial,10'

plot 'bench_inline_storage.dat' using ($3/1024):xtic(2) title 'No Inlining' lc rgb '#4472C4', \
     '' using ($4/1024) title 'Inlined (48B)' lc rgb '#ED7D31'

# Bottom plot: Percentage change with color coding
set tmargin at screen 0.45
set bmargin at screen 0.08

set ylabel "Storage Change (%)" font 'Arial,12'
set yrange [-10:10]
set y2label "Inlined Contents (%)" font 'Arial,12'
set y2range [0:100]
set y2tics
set grid y

set style fill solid 0.7

set key top left

# Color positive (increase) red, negative (decrease) green
plot 'bench_inline_storage.dat' using 1:5:(($5<0)?0x70AD47:0xC00000):xtic(2) with boxes lc rgb variable title 'Storage Change', \
     '' using 1:6 with linespoints lc rgb '#4472C4' pt 7 ps 1.2 lw 2 axes x1y2 title 'Inlined Ratio'

unset multiplot
