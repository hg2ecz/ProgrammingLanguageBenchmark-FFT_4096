export default class Fft {
	private static phasevec: number[][] = [];

	public constructor() {
		for (let i = 0; i < 32; i++) {
			const point = 2 << i;
			Fft.phasevec[i] = [Math.cos(-2 * Math.PI / point), Math.sin(-2 * Math.PI / point)];
		}
	}

	public calculate(log2point: number, xy_in: number[][]): number[][] {
		const xy_out: number[][] = [];

		for (let i: number = 0; i < (1 << log2point); i++) {
			let brev: number = i;
			brev = ((brev & 0xaaaaaaaa) >> 1) | ((brev & 0x55555555) << 1);
			brev = ((brev & 0xcccccccc) >> 2) | ((brev & 0x33333333) << 2);
			brev = ((brev & 0xf0f0f0f0) >> 4) | ((brev & 0x0f0f0f0f) << 4);
			brev = ((brev & 0xff00ff00) >> 8) | ((brev & 0x00ff00ff) << 8);
			//brev = (brev >> 16) | (brev << 16);

			//brev >>= 32-log2point;
			brev >>= 16 - log2point;
			xy_out[brev] = [xy_in[i][0], xy_in[i][1]];
		}

		// here begins the Danielson-Lanczos section
		const n: number = 1 << log2point;
		let l2pt: number = 0;
		let mmax: number = 1;

		while (n > mmax) {
			const istep: number = mmax << 1;

			//	double theta = -2*M_PI/istep;
			//	double complex wphase_XY = cos(theta) + sin(theta)*I;
			const wphase_XY: number[] = Fft.phasevec[l2pt++];
			const w_XY: number[] = [1.0, 0.0];

			for (let m: number = 0; m < mmax; m++) {
				for (let i = m; i < n; i += istep) {
					const tempXY: number[] = [w_XY[0] * xy_out[i + mmax][0] - w_XY[1] * xy_out[i + mmax][1],
					w_XY[0] * xy_out[i + mmax][1] + w_XY[1] * xy_out[i + mmax][0]];

					xy_out[i + mmax][0] = xy_out[i][0] - tempXY[0];
					xy_out[i + mmax][1] = xy_out[i][1] - tempXY[1];

					xy_out[i][0] += tempXY[0];
					xy_out[i][1] += tempXY[1];
				}

				const w_XY_t: number = w_XY[0] * wphase_XY[0] - w_XY[1] * wphase_XY[1]; // rotate ... complex.h ?
				w_XY[1] = w_XY[0] * wphase_XY[1] + w_XY[1] * wphase_XY[0];
				w_XY[0] = w_XY_t;
			}

			mmax = istep;
		}

		return xy_out;
	}
}
