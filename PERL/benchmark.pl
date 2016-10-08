#!/usr/bin/perl -w

#use strict;
#use warnings;

use lib './';

use Math::Complex;
use Time::HiRes;
use fft;

use constant LOG2FFTSIZE => 12;
use constant FFT_REPEAT => 10;

my $SIZE = 1<<LOG2FFTSIZE;
my @xy;
$#xy = $SIZE;


sub main {
    my $SIZE = 1<<LOG2FFTSIZE;
    my @fft_out;
    my $i;

    for ($i=0; $i<$SIZE/2; $i++) { $xy[$i]=  1. + 0*i; }
    for (    ; $i<$SIZE  ; $i++) { $xy[$i]= -1. + 0*i; }

    my $timestart = Time::HiRes::gettimeofday();
    for ($i=0; $i<FFT_REPEAT; $i++) { @fft_out = fft::fft(LOG2FFTSIZE, \@xy); }
    my $eltime = 1000.*(Time::HiRes::gettimeofday() - $timestart);
    printf("%6d piece of %d pt FFT;  %9.5f ms/piece\n", FFT_REPEAT, 1<<LOG2FFTSIZE, $eltime/FFT_REPEAT);

    for ($i=0; $i<6; $i++) {
	printf("%3d %16.9f %16.9f\n", $i, Re($fft_out[$i]), Im($fft_out[$i]));
    }

}

main();
