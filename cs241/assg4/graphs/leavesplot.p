set autoscale
unset log
unset label
set xtic auto
set ytic auto
set title "Number of Leaves / Nodes"
set xlabel "#Nodes"
set ylabel "#Leaves"
plot "out.dat" using 1:2 title "Leaves" with lines
