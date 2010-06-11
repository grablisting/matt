set autoscale
unset log
unset label
set xtic auto
set ytic auto
set title "Left/Right Balance / Nodes"
set xlabel "#Nodes"
set ylabel "%Total Nodes"
plot "out.dat" using 1:6 title "Left Branch" with lines, \
     "out.dat" using 1:7 title "Right Branch" with lines
