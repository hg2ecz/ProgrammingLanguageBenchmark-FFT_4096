// usage: ./fft_phasevec_calc > fft_const.c
#include <math.h>
#include <stdio.h>
int main() {
    const int len = 31;
    puts("// Generated with fft_phasevec_calc.c");
    printf("static FLOAT_VFO_TYPE phasevec[%d][2] = {\n", len);
    for (int i=0; i<len; i++) {
	int point = 2<<i;
	printf("    {%.16f, %.16f},\n", cos(-2*M_PI/point), sin(-2*M_PI/point));
    }
    puts("};");
}
