/*
   here are 3 arithmetic function
   implemented with integer arithmetic

   Use as included C file, because faster
   You can simulate speed CPU without FPU (many microcontrollers)

   (C) 2016  Zsolt Krüpl <hg2ecz@ham.hu>
*/

static inline float add(float ain, float bin);
static inline float sub(float ain, float bin);
static inline float mul(float ain, float bin);

// 1 sign + 8 exp + 23 mant
static inline float add(float ain, float bin) {
    unsigned int *ap = (unsigned int *)&ain;
    unsigned int *bp = (unsigned int *)&bin;

    unsigned int a_sign = *ap & (1U<<31);
    unsigned int b_sign = *bp & (1U<<31);

    int a_exp = ((*ap & ((1U<<31)-1)) >> 23)-127;
    int b_exp = ((*bp & ((1U<<31)-1)) >> 23)-127;

    unsigned int a_frac = (1<<23) | (*ap & ((1<<23)-1));
    unsigned int b_frac = (1<<23) | (*bp & ((1<<23)-1));

    unsigned int sign;
    int exp;
    unsigned int frac;

    if (a_exp == -127 && a_frac == (1<<23) && b_exp == -127 && b_frac == (1<<23)) return ain; // zero

    if (a_sign == b_sign) {
	sign = a_sign;
	exp = a_exp;
	if (b_exp > a_exp) {
	    exp = b_exp;
	    a_frac >>= b_exp - a_exp;
	} else if (a_exp > b_exp) {
	    b_frac >>= a_exp - b_exp;
	}
	frac = a_frac + b_frac;
	if (frac & (1<<24)) {
	    exp++;
	    frac>>=1;
	}
    } else { // (+ and -) or (- and +)
	if (b_exp > a_exp) {
	    sign = b_sign;
	    exp = b_exp;
	    a_frac >>= b_exp - a_exp;
	    frac = b_frac - a_frac;
	} else if (a_exp > b_exp) {
	    sign = a_sign;
	    exp = a_exp;
	    b_frac >>= a_exp - b_exp;
	    frac = a_frac - b_frac;
	} else {
	    exp = a_exp;
	    if (a_frac >= b_frac) {
		sign = a_sign;
		frac = a_frac - b_frac;
	    } else {
		sign = b_sign;
		frac = b_frac - a_frac;
	    }
	}
	for (int i=0; i<23; i++) {
	    if (frac & (1<<23)) break;
	    frac <<=1;
	    exp--;
	}
    }
    unsigned int er = sign | ((exp+127) << 23) | (frac & ((1<<23)-1));
    float *erf = (float*)&er;
    return *erf;
}

// 1 sign + 8 exp + 23 mant
static inline float sub(float ain, float bin) {
    unsigned int *ap = (unsigned int *)&ain;
    unsigned int *bp = (unsigned int *)&bin;

    unsigned int a_sign = *ap & (1U<<31);
    unsigned int b_sign = *bp & (1U<<31);

    int a_exp = ((*ap & ((1U<<31)-1)) >> 23)-127;
    int b_exp = ((*bp & ((1U<<31)-1)) >> 23)-127;

    unsigned int a_frac = (1<<23) | (*ap & ((1<<23)-1));
    unsigned int b_frac = (1<<23) | (*bp & ((1<<23)-1));

    unsigned int sign;
    int exp;
    unsigned int frac;

    if (a_exp == -127 && a_frac == (1<<23) && b_exp == -127 && b_frac == (1<<23)) return ain; // zero

    if (a_sign != b_sign) { // sub (+ and -) or (- and +) --> simple +
	sign = a_sign;
	exp = a_exp;
	if (b_exp > a_exp) {
	    exp = b_exp;
	    a_frac >>= b_exp - a_exp;
	} else if (a_exp > b_exp) {
	    b_frac >>= a_exp - b_exp;
	}
	frac = a_frac + b_frac;
	if (frac & (1<<24)) {
	    exp++;
	    frac>>=1;
	}
    } else { // sub --> a-b
	if (b_exp > a_exp) {
	    sign = b_sign ^ (1U<<31);
	    exp = b_exp;
	    a_frac >>= b_exp - a_exp;
	    frac = b_frac - a_frac;
	} else if (a_exp > b_exp) {
	    sign = a_sign;
	    exp = a_exp;
	    b_frac >>= a_exp - b_exp;
	    frac = a_frac - b_frac;
	} else {
	    exp = a_exp;
	    if (a_frac >= b_frac) {
		sign = a_sign;
		frac = a_frac - b_frac;
	    } else {
		sign = b_sign ^ (1U<<31);
		frac = b_frac - a_frac;
	    }
	}
	for (int i=0; i<23; i++) {
	    if (frac & (1<<23)) break;
	    frac <<=1;
	    exp--;
	}
    }
    unsigned int er = sign | ((exp+127) << 23) | (frac & ((1<<23)-1));
    float *erf = (float*)&er;
    return *erf;
}

// 1 sign + 8 exp + 23 mant
static inline float mul(float ain, float bin) {
    unsigned int *ap = (unsigned int *)&ain;
    unsigned int *bp = (unsigned int *)&bin;

    unsigned int a_sign = *ap & (1U<<31);
    unsigned int b_sign = *bp & (1U<<31);

    int a_exp = ((*ap & ((1U<<31)-1)) >> 23)-127;
    int b_exp = ((*bp & ((1U<<31)-1)) >> 23)-127;

    unsigned int a_frac = (1<<23) | (*ap & ((1<<23)-1));
    unsigned int b_frac = (1<<23) | (*bp & ((1<<23)-1));

    unsigned int frac = ( (long long)a_frac * b_frac) >> 23;
    if (frac & (1<<24)) {
	a_exp++;
	frac>>=1;
    }

    int exp = a_exp+b_exp+127;
    if (exp > 255) exp = 255;
    unsigned int er = exp < 0 ? (a_sign^b_sign) : (a_sign^b_sign) | (exp << 23) | (frac & ((1<<23)-1));
    float *erf = (float*)&er;
    return *erf;
}
