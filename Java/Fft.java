public class Fft {

    // Internal variables
    static int phasevec_exist = 0;
    static double[][] phasevec = new double[32][2];

    // Public function
    public static void fft(int log2point, double [][] xy_out, double [][] xy_in) {
	if (phasevec_exist == 0) {
	    for (int i=0; i<32; i++) {
		int point = 2<<i;
		phasevec[i][0] = Math.cos(-2*Math.PI/point);
		phasevec[i][1] = Math.sin(-2*Math.PI/point);
	    }
	    phasevec_exist = 1;
	}
	for (int i=0; i < (1<<log2point); i++) {
	    long brev = i;
	    brev = ((brev & 0xaaaaaaaa) >> 1) | ((brev & 0x55555555) << 1);
	    brev = ((brev & 0xcccccccc) >> 2) | ((brev & 0x33333333) << 2);
	    brev = ((brev & 0xf0f0f0f0) >> 4) | ((brev & 0x0f0f0f0f) << 4);
	    brev = ((brev & 0xff00ff00) >> 8) | ((brev & 0x00ff00ff) << 8);
	    brev = (brev >> 16) | (brev << 16);

	    brev >>= 32-log2point;
	    xy_out[(int)brev] = xy_in[i].clone();
	}

	// here begins the Danielson-Lanczos section
	int n = 1<<log2point;
	int l2pt=0;
	int mmax=1;

	while (n>mmax) {
	    int istep = mmax<<1;

//	    double theta = -2*M_PI/istep;
//	    double complex wphase_XY = cos(theta) + sin(theta)*I;
	    double[] wphase_XY = phasevec[l2pt++];
	    double[] w_XY = {1.0, 0.0};

	    for (int m=0; m < mmax; m++) {
		for (int i=m; i < n; i += istep) {
		    double[] tempXY = { w_XY[0] *xy_out[i+mmax][0] - w_XY[1] *xy_out[i+mmax][1],
					w_XY[0] *xy_out[i+mmax][1] + w_XY[1] *xy_out[i+mmax][0] };

		    xy_out[i+mmax][0]  = xy_out[i][0] - tempXY[0];
		    xy_out[i+mmax][1]  = xy_out[i][1] - tempXY[1];

		    xy_out[i     ][0] += tempXY[0];
		    xy_out[i     ][1] += tempXY[1];
		}
		double w_X_tmp;
		w_X_tmp = w_XY[0] * wphase_XY[0] - w_XY[1] * wphase_XY[1]; // rotate
		w_XY[1] = w_XY[0] * wphase_XY[1] + w_XY[1] * wphase_XY[0]; // rotate
		w_XY[0] = w_X_tmp;
	    }
	    mmax=istep;
	}
    }
}
