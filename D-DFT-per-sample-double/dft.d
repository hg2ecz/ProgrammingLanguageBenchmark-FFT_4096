import std.math;
import std.complex;

import std.stdio;

const uint LOG2FFTSIZE = 12;
const uint SIZE = (1<<LOG2FFTSIZE);

static uint dft_point = 0;
static double dft_revpt;

static cdouble dft_phasediffXY;
static cdouble[SIZE] dft_vfoXY;


void Dft_init(uint point) {
    dft_point = point;
    dft_revpt = 1./point;
    dft_phasediffXY = cos(-2*PI*dft_revpt) + 1i*sin(-2*PI*dft_revpt);

    for (uint i=0; i<point; i++) {
	dft_vfoXY[i]=1. + 0i;
    }
}

static cdouble dft_corr = 0 + 0i;
void Dft_sample(ref cdouble[SIZE] xy_out, cdouble xy_sample) {
    xy_sample -= dft_corr*dft_revpt;		// remove oldest value from DFT
    //writeln(xy_out);
    dft_corr = 0 + 0i;
    cdouble phaseXY = 1. + 0i;

//  ... slower
/*
    for (uint i=0; i<dft_point; i++) {
	xy_out[i] += xy_sample*dft_vfoXY[i];	// new DFT value

	dft_vfoXY[i] *= phaseXY;		// oscillator new state
	phaseXY *= dft_phasediffXY;		// oscillator new speed
	dft_corr += xy_out[i]*conj(dft_vfoXY[i]); // generate new prev. correction
    }
*/
// ... faster
    for (uint i=0; i<dft_point; i++) {
	cdouble tmp_vfoXY = dft_vfoXY[i]; // register --> faster
	cdouble tmp_xy_out = xy_out[i];   // register --> faster

	tmp_xy_out += xy_sample*tmp_vfoXY;	// new DFT value

	tmp_vfoXY *= phaseXY;			// oscillator new state
	phaseXY *= dft_phasediffXY;		// oscillator new speed
	dft_corr += tmp_xy_out*conj(tmp_vfoXY);	// generate new prev. correction
	//dft_corr += tmp_xy_out*(tmp_vfoXY.re - 1i*tmp_vfoXY.im); // faster than cmplx.Conj()
	dft_vfoXY[i]=tmp_vfoXY;
	xy_out[i] = tmp_xy_out;
    }
}
