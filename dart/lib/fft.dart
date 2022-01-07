import 'package:complex/complex.dart';

class Fft {
  // Internal variables
  final sOne = Complex.one;
  final phasevec = [
    Complex(-1, -1.22464679914735E-16),
    Complex(6.12323399573677E-17, -1),
    Complex(0.707106781186548, -0.707106781186548),
    Complex(0.923879532511287, -0.38268343236509),
    Complex(0.98078528040323, -0.195090322016128),
    Complex(0.995184726672197, -0.0980171403295606),
    Complex(0.998795456205172, -0.049067674327418),
    Complex(0.999698818696204, -0.0245412285229123),
    Complex(0.999924701839145, -0.0122715382857199),
    Complex(0.999981175282601, -0.00613588464915448),
    Complex(0.999995293809576, -0.00306795676296598),
    Complex(0.999998823451702, -0.00153398018628477),
    Complex(0.999999705862882, -0.000766990318742704),
    Complex(0.999999926465718, -0.000383495187571396),
    Complex(0.999999981616429, -0.000191747597310703),
    Complex(0.999999995404107, -9.58737990959773E-05),
    Complex(0.999999998851027, -4.79368996030669E-05),
    Complex(0.999999999712757, -2.39684498084182E-05),
    Complex(0.999999999928189, -1.19842249050697E-05),
    Complex(0.999999999982047, -5.99211245264243E-06),
    Complex(0.999999999995512, -2.99605622633466E-06),
    Complex(0.999999999998878, -1.49802811316901E-06),
    Complex(0.999999999999719, -7.49014056584716E-07),
    Complex(0.99999999999993, -3.74507028292384E-07),
    Complex(0.999999999999982, -1.87253514146195E-07),
    Complex(0.999999999999996, -9.36267570730981E-08),
    Complex(0.999999999999999, -4.68133785365491E-08),
    Complex(1, -2.34066892682746E-08),
    Complex(1, -1.17033446341373E-08),
    Complex(1, -5.85167231706864E-09),
    Complex(1, 2.92583615853432E-09),
    Complex(1, 0)
  ];

  // Public function
  void calculate(int log2FftSize, List<Complex> xyIn, List<Complex> xyOut) {
    var n = 1 << log2FftSize;

    for (int i = 0; i < n; i++) {
      int brev = i;

      brev = ((brev & 0xaaaaaaaa) >> 1) | ((brev & 0x55555555) << 1);
      brev = ((brev & 0xcccccccc) >> 2) | ((brev & 0x33333333) << 2);
      brev = ((brev & 0xf0f0f0f0) >> 4) | ((brev & 0x0f0f0f0f) << 4);
      brev = ((brev & 0xff00ff00) >> 8) | ((brev & 0x00ff00ff) << 8);
      brev = (brev >> 16) | (brev << 16);

      brev >>= 32 - log2FftSize;
      xyOut[brev] = xyIn[i];
    }

    int l2pt = 0;
    int mmax = 1;

    while (n > mmax) {
      int istep = mmax << 1;
      var wphaseXY = phasevec[l2pt++];
      var wXY = sOne;

      for (int m = 0; m < mmax; m++) {
        for (int i = m; i < n; i += istep) {
          var tempXY = wXY * xyOut[i + mmax];

          xyOut[i + mmax] = xyOut[i] - tempXY;
          xyOut[i] += tempXY;
        }

        wXY *= wphaseXY;
      }

      mmax = istep;
    }
  }
}
