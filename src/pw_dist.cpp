#include <Rcpp.h>
using namespace Rcpp;

double pw_dist (NumericVector x, NumericVector y, double ab, double min_x, double min_y, int nc){
  double d = 0;
  for (int i = 0 ; i < nc ; i++){
    //Rcout << "The value is " << min_x << std::endl;
        if(!((x[i] == min_x) && (y[i] == min_y))){
      d += pow(x[i] - y[i] + ab, 2);
    }
  }
  return sqrt(d);
}
  
  
  int n_drop (NumericVector v_i, NumericVector v_j, double min_i, double min_j, int nc){
    int res = 0;
    for (int i = 0 ; i < nc ; i++){
      if((v_i[i] == min_i) && (v_j[i] ==  min_j)){
        res++;
      }
    }
    return res;
  }

double get_ab (double ntot, double ndrop, double min_x, double min_y){
  double res = (min_x - min_y) * (ndrop / (ntot - ndrop));
  return(res);
}
  

// [[Rcpp::export(rng = false)]]
NumericMatrix pw_dist_cpp (const NumericMatrix x){
  const int nr = x.nrow();
  const int nc = x.ncol();
  NumericMatrix out(nr,nr);
  NumericVector r_min(nr);
  for (int i = 0 ; i < nr ; i++){
    r_min[i] = min(x.row(i));
  }
  
  for (int i = 0 ; i < nr ; i++){
    NumericVector v_i = x.row(i);
    //double min_i = min(v_i);
    for (int j = i  ; j < nr ; j ++) {
      if (i != j){
        NumericVector v_j = x.row(j);
        //double min_j = min(v_j);
        int ndrop = n_drop(v_i, v_j,    r_min[i], r_min[j], nc);
        double ab = get_ab(nc, ndrop,   r_min[i], r_min[j]);
        double d  = pw_dist(v_i, v_j, ab,  r_min[i], r_min[j], nc);
        out(j,i) = d;
      }
    }
  }
  return (out) ;
}


