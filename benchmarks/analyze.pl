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
	    if (/^executed load insns:\s*(\d+)/) {
		$mem{$bench}{$config}{$nth}{loads} = $1;
	    } elsif (/^executed store insns:\s*(\d+)/) {
		$mem{$bench}{$config}{$nth}{stores} = $1;
	    } elsif (/^executed crf0 insns:\s*(\d+)/) {
		$mem{$bench}{$config}{$nth}{crf0} = $1;
	    } elsif (/^executed crfx insns:\s*(\d+)/) {
		$mem{$bench}{$config}{$nth}{crfx} = $1;
	    } elsif (/^translated blocks:\s*(\d+)/) {
		$mem{$bench}{$config}{$nth}{transblocks} = $1;
	    } elsif (/^translated insns:\s*(\d+)/) {
		$mem{$bench}{$config}{$nth}{transinsns} = $1;
	    } elsif (/^generated insns:\s*(\d+)/) {
		$mem{$bench}{$config}{$nth}{geninsns} = $1;
	    } elsif (/^load reg insns:\s*(\d+)/) {
		$mem{$bench}{$config}{$nth}{genloads} = $1;
	    } elsif (/^store reg insns:\s*(\d+)/) {
		$mem{$bench}{$config}{$nth}{genstores} = $1;
	    } elsif (/^generated crf0 bits:\s*(\d+)/) {
		$mem{$bench}{$config}{$nth}{gencrf0} = $1;
	    } elsif (/^generated crfx bits:\s*(\d+)/) {
		$mem{$bench}{$config}{$nth}{gencrfx} = $1;
	    } elsif (/^patched store reg insns:\s*(\d+)/) {
		$mem{$bench}{$config}{$nth}{patchedstores} = $1;
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
	    print "$bench $config $nth $time";
	    foreach $key (("loads", "stores", "crf0", "crfx", "transblocks", "transinsns", 
			   "geninsns", "genloads", "genstores", "gencrf0", "gencrfx", "patchedstores")) {
		$value = $mem{$bench}{$config}{$nth}{$key};
		print " $value";
	    }
	    print "\n";
	}
    }
}
