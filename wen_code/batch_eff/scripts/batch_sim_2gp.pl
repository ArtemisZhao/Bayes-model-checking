@bb = (0, 0.2, 0.4, 0.6, 0.8, 1.0);
@rnse = (2, 4, 8, 64);

foreach $b (@bb){
	print "Rscript scripts/sim_2grp.R -bb $b\n";
}

foreach $r (@rnse){
	print "Rscript scripts/sim_2grp.R -bb 1 -rnse $r\n"
}
