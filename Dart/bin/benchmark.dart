import 'package:benchmark/fft.dart';
import 'package:complex/complex.dart';

const int log2FftSize = 12;
const int fftRepeat = 10000;
const size = 1 << log2FftSize;

void main(List<String> arguments) {
  int i;

  var xy = List<Complex>.filled(size, Complex(1.0, 0.0), growable: false);
  var xyOut =
      List<Complex>.filled(xy.length, Complex(0.0, 0.0), growable: false);

  for (var i = size >> 1; i < size; i++) {
    xy[i] = Complex(-1.0, 0.0);
  }

  final fft = Fft();

  // JIT warm up ... possible give more speed
  for (i = 0; i < fftRepeat; i++) {
    fft.calculate(log2FftSize, xy, xyOut);
  }

  // FFT
  var stopwatch = Stopwatch();
  stopwatch.start();

  for (i = 0; i < fftRepeat; i++) {
    fft.calculate(log2FftSize, xy, xyOut);
  }

  stopwatch.stop();

  print('Total ($fftRepeat): ${stopwatch.elapsedMilliseconds}');

  final tpp = stopwatch.elapsedMilliseconds / fftRepeat;

  print('$fftRepeat piece(s) of ${1 << log2FftSize} pt FFT; $tpp ms/piece\n');

  for (i = 0; i < 6; i++) {
    print('$i\t${xyOut[i]}');
  }
}
