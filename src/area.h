double area_polygon_lonlat(std::vector<double> lon, std::vector<double> lat, double a, double f);
double area_polygon_plane(std::vector<double> x, std::vector<double> y);

std::vector<double> area_polygon_lonlat(std::vector<double> lon, std::vector<double> lat, std::vector<int> pols, std::vector<int> parts, std::vector<int> holes, double a, double f);
std::vector<double> area_polygon_plane(std::vector<double> x, std::vector<double> y, std::vector<int> pols, std::vector<int> parts, std::vector<int> holes);

