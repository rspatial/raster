
#ifndef RASTERUTIL_GUARD
#define RASTERUTIL_GUARD


#include <math.h>
#include "Rmath.h"

#ifndef M_PI
#define M_PI (3.14159265358979323846)
#endif


double mod(double x, double n) {
	return(x - n * floor(x/n));
}

double normalizeLonDeg(double lon) {
	return( mod( (lon + 180), 360 ) - 180 );
}

double normalizeLonRad(double lon) {
	return( mod( (lon + M_PI), M_2PI) - M_PI);
}


/* Convert degrees to radians */
double toRad(double deg) {
	return( deg * 0.0174532925199433 );
}

double toDeg(double rad) {
	return( rad * 57.2957795130823 );
}




#endif

