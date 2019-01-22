#include <gsl/gsl_sf_coupling.h>
#include <gsl/gsl_errno.h>

/* function names generated by removing  "gsl_sf" from the beginning
   of the name and _e from the end.  Thus gsl_sf_debye_1_e  goes to
   debye_1. */


void coupling_3j(const int *two_ja, const int *two_jb, const int *two_jc, const int *two_ma, const int *two_mb, const int *two_mc, const int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_coupling_3j_e(two_ja[i], two_jb[i], two_jc[i], two_ma[i], two_mb[i], two_mc[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void coupling_6j(const int *two_ja, const int *two_jb, const int *two_jc, const int *two_jd, const int *two_je, const int *two_jf, const int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_coupling_6j_e(two_ja[i], two_jb[i], two_jc[i], two_jd[i], two_je[i], two_jf[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}  

void coupling_9j(const int *two_ja, const int *two_jb, const int *two_jc, const int *two_jd, const int *two_je, const int *two_jf, const int *two_jg, const int *two_jh, const int *two_ji, const int *len, double *val, double *err, int *status)
{
	int i;
	gsl_sf_result result;
	gsl_set_error_handler_off();
  
	for(i = 0; i< *len ; i++){
		status[i] = gsl_sf_coupling_9j_e(two_ja[i], two_jb[i], two_jc[i], two_jd[i], two_je[i], two_jf[i], two_jg[i], two_jh[i], two_ji[i], &result) ;
		val[i] = result.val;
		err[i] = result.err;
	}
}