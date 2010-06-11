set autoscale
unset log
unset label
set xtic auto
set ytic auto
set title "Min/Max Depths / Nodes"
set xlabel "#Nodes"
set ylabel "#Depth"
plot "out.dat" using 1:3 title "Min Depth" with lines, \
     "out.dat" using 1:4 title "Max Depth" with lines, \
     "out.dat" using 1:5 title "Depth Diff" with lines
