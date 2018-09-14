#include <Rcpp.h>
#include "spat.h"

using namespace Rcpp;

RCPP_EXPOSED_CLASS(SpExtent)

//RCPP_EXPOSED_CLASS(RasterSource)
//RCPP_EXPOSED_CLASS(SpRaster)

RCPP_EXPOSED_CLASS(SpPolyPart)
RCPP_EXPOSED_CLASS(SpPoly)
RCPP_EXPOSED_CLASS(SpPolygons)


	
RCPP_MODULE(spmod){

    using namespace Rcpp;


    class_<SpPolyPart>("SpPolyPart")
		.constructor()
		.field_readonly("x", &SpPolyPart::x )
		.field_readonly("y", &SpPolyPart::y )
		.field_readonly("extent", &SpPolyPart::extent )
		.method("set", &SpPolyPart::set, "set")
		.method("setHole", &SpPolyPart::setHole, "setHole")
		.method("getHoleX", &SpPolyPart::getHoleX, "getHoleX")
		.method("getHoleY", &SpPolyPart::getHoleY, "getHoleY")
		.method("nHoles", &SpPolyPart::nHoles, "nHoles")
		.method("hasHoles", &SpPolyPart::hasHoles, "hasHoles")
		
	;	
    class_<SpPoly>("SpPoly")
		.constructor()
		.field_readonly("extent", &SpPoly::extent )
		.method("getPart", &SpPoly::getPart, "getPart")
		.method("addPart", &SpPoly::addPart, "addPart")
		.method("size", &SpPoly::size, "size")
		
	;	
    class_<SpPolygons>("SpPolygons")
//		.field("polygons", &SpPolygons::polys )
		.field_readonly("extent", &SpPolygons::extent )
		.field("attr", &SpPolygons::attr )
		.field("crs", &SpPolygons::crs )
		.constructor()
		.method("getPoly", &SpPolygons::getPoly, "getPoly")
		.method("addPoly", &SpPolygons::addPoly, "addPoly")
		.method("size", &SpPolygons::size, "size")

		.method("getAtt", &SpPolygons::getAtt, "getAtt")
		.method("setAtt", &SpPolygons::setAtt, "setAtt")
		
		.method("rasterize", &SpPolygons::rasterize, "rasterize")	
		.method("subset", &SpPolygons::subset, "subset")	
	;	

	
    class_<SpExtent>("SpExtent")
		.constructor()
		.constructor<double, double, double, double>()
		.property("vector", &SpExtent::asVector)		
		.property("valid", &SpExtent::valid)		
	;	

}
