/*
   here are 3 arithmetic function
   implemented with integer arithmetic

   Use as included C file, because faster
   You can simulate speed CPU without FPU (many microcontrollers)

   (C) 2016  Zsolt Krüpl <hg2ecz@ham.hu>

Functions:
    float add(float ain, float bin);
    float sub(float ain, float bin);
    float mul(float ain, float bin);
*/


// 1 sign + 8 exp + 23 mant
static inline float add(float ain, float bin) {
    unsigned int *ap = (unsigned int *)&ain;
    unsigned int *bp = (unsigned int *)&bin;

    unsigned int a_sign = *ap & (1U<<31);
    unsigned int b_sign = *bp & (1U<<31);

    int a_exp = *ap & (0xff<<23);
    int b_exp = *bp & (0xff<<23);

    unsigned int a_frac = (1<<23) | (*ap & ((1<<23)-1));
    unsigned int b_frac = (1<<23) | (*bp & ((1<<23)-1));

    if (a_exp == 0 && a_frac == (1<<23) && b_exp == 0 && b_frac == (1<<23)) return ain; // zero

    if (a_sign == b_sign) {
	if (b_exp > a_exp) {
	    a_frac = (a_frac >> ((b_exp - a_exp)>>23)) + b_frac;
	    a_exp = b_exp;
	} else if (a_exp > b_exp) {
	    a_frac += b_frac >> ((a_exp - b_exp)>>23);
	} else a_frac += b_frac;

	if (a_frac & (1<<24)) { // exponent normalisation (+)
	    a_exp += 1<<23;
	    a_frac>>=1;
	}
    } else { // (+ and -) or (- and +)
	if (b_exp > a_exp) {
	    a_sign = b_sign;
	    a_frac = b_frac - (a_frac >> ((b_exp - a_exp)>>23));
	    a_exp = b_exp;
	} else if (a_exp > b_exp) {
	    a_frac -= b_frac >> ((a_exp - b_exp)>>23);
	} else {
	    if (a_frac >= b_frac) {
		a_frac -= b_frac;
	    } else {
		a_sign = b_sign;
		a_frac = b_frac - a_frac;
	    }
	}
	if (!(a_frac & ((1>>23)-1))) { // zero --> return zero
	    float *resf = (float *)&a_sign;
	    return *resf;
	}
	// fast exponent normalisation (-)
	if (!(a_frac & (0xffff<<8))){ a_frac <<=16; a_exp -=16<<23; } //16 bit group
	if (!(a_frac & (0xff<<16))) { a_frac <<= 8; a_exp -= 8<<23; } // 8 bit group
	if (!(a_frac & (0x0f<<20))) { a_frac <<= 4; a_exp -= 4<<23; } // 4 bit group
	if (!(a_frac & (0x03<<22))) { a_frac <<= 2; a_exp -= 2<<23; } // 2 bit group
	if (!(a_frac & (0x01<<23))) { a_frac <<= 1; a_exp -= 1<<23; } // 1 bit
    }
    unsigned int res = a_sign | a_exp | (a_frac & ((1<<23)-1));
    float *resf = (float*)&res;
    return *resf;
}

// 1 sign + 8 exp + 23 mant
static inline float sub(float ain, float bin) {
    unsigned int *bp = (unsigned int *)&bin;
    *bp ^= 1U<<31; // sign
    return add(ain, bin);
}

// 1 sign + 8 exp + 23 mant
static inline float mul(float ain, float bin) {
    unsigned int *ap = (unsigned int *)&ain;
    unsigned int *bp = (unsigned int *)&bin;

    unsigned int sign = (*ap & (1U<<31)) ^ (*bp & (1U<<31));        // neg * neg = pos, ...
    int exp = (*ap & (0xff<<23)) + (*bp & (0xff<<23)) - (0x7f<<23); // exp + exp - 127
    if (exp < 0) {
	float *resf = (float *)&sign;
	return *resf;
    }

    unsigned int a_frac = (1<<23) | (*ap & ((1<<23)-1));
    unsigned int b_frac = (1<<23) | (*bp & ((1<<23)-1));

    a_frac = ( (long long)a_frac * b_frac) >> 23;
    if (a_frac & (1<<24)) {
	exp += 1<<23;  // time opt
	a_frac>>=1;
    }

    //if (exp > (0xff<<23)) exp = 0xff<<23; ---> not need for signal processing
    unsigned int res = sign | exp | (a_frac & ((1<<23)-1));
    float *resf = (float*)&res;
    return *resf;
}
