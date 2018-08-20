#include <Rcpp.h>
#include "spat.h"

using namespace Rcpp;

RCPP_EXPOSED_CLASS(SpatExtent)

//RCPP_EXPOSED_CLASS(RasterSource)
//RCPP_EXPOSED_CLASS(SpatRaster)

RCPP_EXPOSED_CLASS(SpatPolyPart)
RCPP_EXPOSED_CLASS(SpatPoly)
RCPP_EXPOSED_CLASS(SpatPolygons)


	
RCPP_MODULE(spat){

    using namespace Rcpp;


    class_<SpatPolyPart>("SpatPolyPart")
		.constructor()
		.field_readonly("x", &SpatPolyPart::x )
		.field_readonly("y", &SpatPolyPart::y )
		.field_readonly("extent", &SpatPolyPart::extent )
		.method("set", &SpatPolyPart::set, "set")
		.method("setHole", &SpatPolyPart::setHole, "setHole")
		.method("getHoleX", &SpatPolyPart::getHoleX, "getHoleX")
		.method("getHoleY", &SpatPolyPart::getHoleY, "getHoleY")
		.method("nHoles", &SpatPolyPart::nHoles, "nHoles")
		.method("hasHoles", &SpatPolyPart::hasHoles, "hasHoles")
		
	;	
    class_<SpatPoly>("SpatPoly")
		.constructor()
		.field_readonly("extent", &SpatPoly::extent )
		.method("getPart", &SpatPoly::getPart, "getPart")
		.method("addPart", &SpatPoly::addPart, "addPart")
		.method("size", &SpatPoly::size, "size")
		
	;	
    class_<SpatPolygons>("SpatPolygons")
//		.field("polygons", &SpatPolygons::polys )
		.field_readonly("extent", &SpatPolygons::extent )
		.field("attr", &SpatPolygons::attr )
		.field("crs", &SpatPolygons::crs )
		.constructor()
		.method("getPoly", &SpatPolygons::getPoly, "getPoly")
		.method("addPoly", &SpatPolygons::addPoly, "addPoly")
		.method("size", &SpatPolygons::size, "size")

		.method("getAtt", &SpatPolygons::getAtt, "getAtt")
		.method("setAtt", &SpatPolygons::setAtt, "setAtt")
		
		.method("rasterize", &SpatPolygons::rasterize, "rasterize")	
		.method("subset", &SpatPolygons::subset, "subset")	
	;	

	
    class_<SpatExtent>("SpatExtent")
		.constructor()
		.constructor<double, double, double, double>()
		.property("vector", &SpatExtent::asVector)		
		.property("valid", &SpatExtent::valid)		
	;	

}
