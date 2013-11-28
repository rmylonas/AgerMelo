#! /usr/bin/perl

use Data::Dumper;

die "add_factor.pl [rownames.csv] [sample_descr.csv]\n" unless scalar @ARGV == 2;

%factors = (3 =>"high", 2=>"medium", 1=>"low");

open DESCR, $ARGV[1] or die "cannot open [".$ARGV[1]."]: $!\n";
while(<DESCR>){
	chomp;
	@a = split(",", $_);
	$desc{$a[1]}->{sprintf("%02s",$a[2])} = $factors{$a[3]}; 
	
}
close DESCR;

#print Dumper %desc;

open NAMES, $ARGV[0] or die "cannot open [".$ARGV[0]."]: $!\n";
while(<NAMES>){
	chomp;
	s/"//g;
	@a = split("_", $_);
	$factor = $desc{$a[2]}->{$a[4]};
	print $_.($factor?("_$factor"):'')."\n";
}
close NAMES;
