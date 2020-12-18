set terminal pngcairo
set grid
set output "graph1.png"
plot "graph1.dat" with l lt 1 lw 2 title "Test"
set output "graph1_p.png"
plot "graph1.dat" with l lt 5 lw 2 title "Test", "graph1.dat" with p lt 1 pt 5 ps 0.2 title "points"
