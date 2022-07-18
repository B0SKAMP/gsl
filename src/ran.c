#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

#include <R.h>
#include <Rdefines.h>

gsl_rng* get_rng_from_sexp(SEXP rng);


SEXP ran_gaussian(SEXP r, SEXP ssigma, SEXP slength) {
	SEXP result;
	gsl_rng* gen;
	int i;
	double *sigma;
	
	gen = get_rng_from_sexp(r);
	sigma = REAL(ssigma);
	
	if(length(ssigma) == 1) {
		/* get draws */
		//length = asInteger(slength);
		PROTECT(result = NEW_NUMERIC(asInteger(slength)));
		for(i = 0; i<asInteger(slength); i++) {
			NUMERIC_POINTER(result)[i] = gsl_ran_gaussian(gen, sigma[0]);
		}
		UNPROTECT(1);
	} else {
		/* get draws */
		PROTECT(result = NEW_NUMERIC(length(ssigma)));
 		for(i = 0; i<length(ssigma); i++) {
			NUMERIC_POINTER(result)[i] = gsl_ran_gaussian(gen, sigma[i]);
		}
		UNPROTECT(1);
	}

	return result;
}


SEXP ran_gamma(SEXP r, SEXP sa, SEXP sb, SEXP slength) {
	SEXP result;
	gsl_rng* gen;
	int length, i;
	double *a, *b;
	
	gen = get_rng_from_sexp(r);
	a = REAL(sa);
	b = REAL(sb);

	if(length(sa) != 1 && length(sb) != 1) {
		length = length(sa);
		if(length(sb) != length) error("a and b must be the same length");
		/* get draws */
		PROTECT(result = NEW_NUMERIC(length));
		for(i = 0; i<length; i++) {
			NUMERIC_POINTER(result)[i] = gsl_ran_gamma(gen, a[i], b[i]);
		}
		UNPROTECT(1);
        } else if(length(sa) != 1 ) {
		length = length(sa);
		/* get draws */
		PROTECT(result = NEW_NUMERIC(length));
		for(i = 0; i<length; i++) {
			NUMERIC_POINTER(result)[i] = gsl_ran_gamma(gen, a[i], b[0]);
		}
		UNPROTECT(1);
	} else {
		length = asInteger(slength);
                /* get draws */
		PROTECT(result = NEW_NUMERIC(length));
		for(i = 0; i<length; i++) {
			NUMERIC_POINTER(result)[i] = gsl_ran_gamma(gen, a[0], b[0]);
		}
		UNPROTECT(1);
	}
	return result;
}
