#!/usr/bin/perl

foreach $fn (glob("out.*")) {
    if ($fn =~ /\.count/) {
	$fn =~ /out\.(\w+)\.(\w+)\.(\w+)(\.(\w+))?\.count/ or die;
	$count = 1;
	$m1 = $1; $m2 = $2; $m3 = $3; $m4 = $4; $m5 = $5;
    } else {
	$fn =~ /out\.(\w+)\.(\w+)\.(\w+)(\.(\w+))?/ or die;
	$count = 0;
	$m1 = $1; $m2 = $2; $m3 = $3; $m4 = $4; $m5 = $5;
    }
    $bench = $m1;
    $config = $m2;
    if ($m3 eq "static") {
	next if $m5 eq "collect";
	$nth = $m3;
	$run = $m5;
    } elsif ($m4) {
	$nth = $m5;
	$run = $m3;
    } else {
	$nth = "na";
	$run = $m3;
    }
    #print "$bench $config $nth $run $count ($fn)\n";
    open(FILE, $fn) or die;
    while (<FILE>) {
	if (!$count) {
	    if (/(\d+\.\d+)user (\d+\.\d+)system/) {
		$time{$bench}{$config}{$nth}{$run} = $1 + $2;
		last;
	    }
	} elsif ($count) {
	    if (/executed load insns:\s*(\d+)/) {
		$mem{$bench}{$config}{$nth}{loads} = $1;
	    } elsif (/executed store insns:\s*(\d+)/) {
		$mem{$bench}{$config}{$nth}{stores} = $1;
	    }
	}
    }
    close(FILE);
}

foreach $bench (keys %time) {
    foreach $config (keys %{$time{$bench}}) {
	foreach $nth (keys %{$time{$bench}{$config}}) {
	    $time = 0;
	    foreach $run (keys %{$time{$bench}{$config}{$nth}}) {
		$time += $time{$bench}{$config}{$nth}{$run};
	    }
	    $loads = $mem{$bench}{$config}{$nth}{loads};
	    $stores = $mem{$bench}{$config}{$nth}{stores};
	    print "$bench $config $nth $time $loads $stores\n";
	}
    }
}
