package fft;

use Math::Complex;
use constant M_PI  => 4 * atan2 1, 1;

# Internal variables
my $phasevec_exist = 0;
my @phasevec;
$#phasevec = 30;

# Public function
sub fft {
    my ($log2point, $xy_in_ref) = @_;
    my @xy_in = @{$xy_in_ref};

    my $i;
    if (!$phasevec_exist) {
	for ($i=0; $i<30; $i++) { # 2**32 --> negative !!! 32 bit signed
	    $point = 2<<$i;
	    $phasevec[$i] = cos(-2 * M_PI / $point) + sin(-2 * M_PI / $point)*i; # complex.h ??
	}
	$phasevec_exist = 1;
    }

    my @xy_out;
    $#xy_out = 1<<$log2point;

    for ($i=0; $i < (1<<$log2point); $i++) {
	my $brev = $i;
	$brev = (($brev & 0xaaaaaaaa) >> 1) | (($brev & 0x55555555) << 1);
	$brev = (($brev & 0xcccccccc) >> 2) | (($brev & 0x33333333) << 2);
	$brev = (($brev & 0xf0f0f0f0) >> 4) | (($brev & 0x0f0f0f0f) << 4);
	$brev = (($brev & 0xff00ff00) >> 8) | (($brev & 0x00ff00ff) << 8);
	$brev = ($brev >> 16) | ($brev << 16); # -- PHP: max 30 bit (signed int)

	$brev >>= 32-$log2point;
	#$brev >>= 16-$log2point;   // no >>16 <<16
	$xy_out[$brev] = $xy_in[$i];
    }

    # here begins the Danielson-Lanczos section
    my $n = 1<<$log2point;
    my $l2pt=0;
    my $mmax=1;
    while ($n > $mmax) {
	my $istep = $mmax<<1;

	my $wphase_XY = $phasevec[$l2pt++];

	my $w_XY = 1.0 + 0.0*i;
	for ($m=0; $m < $mmax; $m++) {
	    for ($i=$m; $i < $n; $i += $istep) {
		my $tempXY = $w_XY * $xy_out[$i+$mmax];
		$xy_out[$i+$mmax] = $xy_out[$i] - $tempXY;
		$xy_out[$i     ] += $tempXY;
	    }
	    $w_XY = $w_XY * $wphase_XY; # rotate
	}
	$mmax=$istep;
    }

    return @xy_out;
}
