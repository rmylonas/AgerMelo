#! /usr/bin/perl -w

use strict;
use Scalar::Util qw(looks_like_number);

my ($exp_rdata, $db_csv, $neg) = @ARGV;

# tolerance
my $ppm = 20;

# exp_csv
my $exp_csv = "/tmp/mz.csv";

# create a temporary csv containing the mz
my $cmd = "R -e 'load(\"" .$exp_rdata. "\"); write.table(data.frame(mz,rt), \"" . $exp_csv . "\", row.names=FALSE, col.names=FALSE);' > /dev/null";
system($cmd);


# load the reference mz
my @ref_mz;
my $i=1;
open(IN, $db_csv) or die "cannot open [$db_csv]: $!\n";
while(<IN>){
	chomp;
	my @a = split(/\t+/, $_);
	my $mz_id = ($neg) ? (2) : (1);

	push @ref_mz, [$a[$mz_id], $i, $a[0]] if(looks_like_number($a[$mz_id]));
	$i++;
}
close IN;

print STDERR (scalar @ref_mz, "\n");

my @out;

# loop through the experimental mz
open(IN, $exp_csv) or die "cannot open [$exp_csv]: $!\n";
my $k=1;
my %already;
while(<IN>){
	chomp;
	my ($exp, $rt) = split(/\t+/, $_);
	print STDERR "[$_]\n";
	foreach my $ref(@ref_mz){
		my $tol = $ppm * $exp / 1000000;
		# only look at valid values
		if($exp >= ($ref->[0] - $tol) and $exp <= ($ref->[0] + $tol)){
			$already{$ref->[1]} ++;
			next if $already{$ref->[1]} > 1;
			push @out, [$k, $ref->[1], $exp, $ref->[0], $ref->[2]];
			#last;
		}
	}
	$k++;
}
close IN;

print STDERR (scalar @out, "\n");

# print the results
print("exp_id,ref_id,exp_mz,ref_mz,ref_name,nr_hits\n");
foreach(@out){
	print(join(",", @$_), "\n");
}