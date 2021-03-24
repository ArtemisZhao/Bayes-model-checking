@files = <sim_data/*>;
foreach $f (@files){
	print "Rscript scripts/analyze_2grp.R $f\n";
}
