#include	<math.h>


/*
By Paul Bourke
http://paulbourke.net/geometry/lineline2d/
Determine the intersection point of two line segments
*/

int intersectSegments( double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4, 
						double *xa, double *ya, double *xb, double *yb) {
   double mua,mub;
   double denom,numera,numerb;
   double EPS = 0.000000001;

    *xa = 0;
    *ya = 0;
    *xb = 0;
    *yb = 0;
 
   denom  = (y4-y3) * (x2-x1) - (x4-x3) * (y2-y1);
   numera = (x4-x3) * (y1-y3) - (y4-y3) * (x1-x3);
   numerb = (x2-x1) * (y1-y3) - (y2-y1) * (x1-x3);

   /* Are the line coincident? */
   if (fabs(numera) < EPS && fabs(numerb) < EPS && fabs(denom) < EPS) {
	 // changes by RH
	 // return two points if lines coincide.
		if (x1 > x3) {
			*xa = x1;
			*ya = y1;
		} else {
			*xa = x3;
			*ya = y3;
		}
		if (x2 > x4) {
			*xb = x2;
			*yb = y2;
		} else {
			*xb = x4;
			*yb = y4;
		}
		return(2);
   }

   /* Are the line parallel */
   if (fabs(denom) < EPS) {
		return(0);
   }

   /* Is the intersection along the the segments */
   mua = numera / denom;
   mub = numerb / denom;
   if (mua < 0 || mua > 1 || mub < 0 || mub > 1) {
		return(0);
   }
   
   *xa = x1 + mua * (x2 - x1);
   *ya = y1 + mua * (y2 - y1);
   return(1);
}

