#!/usr/bin/perl

foreach $fn (glob("out.*")) {
    $fn =~ /out\.(\w+)\.(\w+)\.(\w+)(\.(\w+))?/ or die;
    $bench = $1;
    $config = $2;
    if ($3 eq "static") {
	next if $5 eq "collect";
	$nth = $3;
	$run = $5;
    } elsif ($4) {
	$nth = $3;
	$run = $5;
    } else {
	$nth = "na";
	$run = $3;
    }
    print "$bench $config $nth $run\n";
    open(FILE, $fn) or die;
    while (<FILE>) {
	if (/(\d+\.\d+)user (\d+\.\d+)system/) {
	    $data{$bench}{$config}{$nth}{$run} = $1 + $2;
	    last;
	}
    }
    close(FILE);
}

foreach $bench (keys %data) {
    foreach $config (keys %{$data{$bench}}) {
	foreach $nth (keys %{$data{$bench}{$config}}) {
	    $time = 0;
	    foreach $run (keys %{$data{$bench}{$config}{$nth}}) {
		$time += $data{$bench}{$config}{$nth}{$run};
	    }
	    print "$bench $config $nth $time\n";
	}
    }
}
