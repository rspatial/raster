/* Robert Hijmans, October 2014 */

#include <vector>
#include <limits>
#include <cmath>


std::vector<int > get_dims( std::vector<int > dim) {

  dim.resize(9);
  for (int i=0; i < 3; i++) {
    dim[i+6] = std::ceil(dim[i] / double(dim[i+3])); 
  }
  return(dim);
 
/*
  // raster dimensions
  int nr = dim[0], nc = dim[1], nl =dim[2];
  // aggregation factors in the three dimensions
  int dy = dim[3], dx = dim[4], dz = dim[5];
  // new dimensions: rows, cols, lays
  dim[6] = std::ceil(nr / double(dy)); 
  dim[7] = std::ceil(nc / double(dx)); 
  dim[8] = std::ceil(nl / double(dz));
*/  

}


std::vector<std::vector< double > > get_aggregates(std::vector<std::vector< double > > data, std::vector<int> dim) {
  
  // raster nrow, ncol, nlay
  int nr = dim[0], nc = dim[1], nl =dim[2];
  // nl == data.size();
  // data[0].size() == nr * nc == ncell;
  
  // aggregation factor in three dimensions
  int dy = dim[3], dx = dim[4], dz = dim[5];

  // blocks per row (=ncol), col (=nrow) 
  int bpC = dim[6], bpR = dim[7];
  // blocks per layer
  int bpL = bpR * bpC;

  // new number of layers
  int newNL = dim[8];
  
  // new number of rows, adjusted for additional (expansion) rows
  int adjnr = bpC * dy;

  // number of aggregates
  int nblocks = (bpR * bpC * newNL);
  // cells per aggregate
  int blockcells = dx * dy * dz;
  
  // output: each row is a block 
  std::vector< std::vector<double> > a(nblocks, std::vector<double>(blockcells, std::numeric_limits<double>::quiet_NaN()));
  

  for (int b = 0; b < nblocks; b++) {
    int lstart = dz * (b / bpL);
    int rstart = (dy * (b / bpR)) % adjnr;
    int cstart = dx * (b % bpR);

    int lmax   = std::min(nl, (lstart + dz));
    int rmax   = std::min(nr, (rstart + dy));
    int cmax   = std::min(nc, (cstart + dx));

   // Rcout << b << ", " << lstart << ", "  << rstart << ", "  << cstart <<  "\n";

    int f = 0;
    for (int j = lstart; j < lmax; j++) {
      for (int r = rstart; r < rmax; r++) {
        int cell = r * nc;
        for (int c = cstart; c < cmax; c++) {
          //Rcout << "cell : " << cell + c << "\n";
          a[b][f] = data[cell + c][j];
          f++;
        }
      }
    }
  }

  return(a);
}



std::vector<std::vector< double > > aggregate(std::vector<std::vector< double > > data, std::vector<int> dim, bool narm, int fun) {

  // fun  = 'sum', 'mean', 'min', 'max'
  //           0,     1,     2,    3
  
  int mean = 0;
  if (fun==1) {
    fun = 0;
    mean = 1;
  }
  
  // blocks per row (=ncol), col (=nrow) 
  int ncol = dim[6], nrow = dim[7];
  // new number of layers
  int nl = dim[8];
  
  // output: each row is a new cell 
  double NA = std::numeric_limits<double>::quiet_NaN();
  std::vector< std::vector<double> > v(nrow*ncol, std::vector<double>(nl, NA));
                                         
// get the aggregates	
 std::vector<std::vector< double > > a = get_aggregates(data, dim);

 int nblocks = a.size();
 int naggs = a[0].size();
// Rcout << nblocks << ", " << naggs << "\n";
 
  for (int i = 0; i < nblocks; i++) {
    int row = (i / ncol) % nrow; 
    int col = i % ncol;
    int cell = row * ncol + col;
    int lyr = std::floor(i / (nrow * ncol));

//    Rcout << row << ", " << col << ", " << lyr << "\n";
    
    double x = 0;
    if (fun==2) { // min
      x = std::numeric_limits<double>::infinity();
    } else if (fun==3) { // max
      x = - std::numeric_limits<double>::infinity() ; 
    } 
    
    double cnt = 0;
    
    for (int j = 0; j < naggs; j++) {
      //Rcout << x << ", " << a[i][j] <<  "\n";
      if (std::isnan(a[i][j])) {
        if (!narm) {
          x = NA;
          goto breakout;
        }
      } else {
        if (fun==2) { // min
          x = std::min(x, a[i][j]);
        } else if (fun==3) { // max
          x = std::max(x, a[i][j]);
        } else { // sum or mean	
          x += a[i][j];
        }
        cnt++;
      }
    }
    if (cnt > 0) {
    if (mean) {
        x = x / cnt;
      } 
    } else {
      x = NA;
    }
    breakout:
    v[cell][lyr] = x;
  }
  return(v);
}

