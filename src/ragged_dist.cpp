#include <Rcpp.h>
using namespace Rcpp;

double dist(NumericVector x, NumericVector y){
  double d = sqrt( sum( pow(x - y, 2) ) );
  return d;
}

LogicalVector sz_inx(NumericVector v_i, NumericVector v_j, double min_i, double min_j){
  LogicalVector l_ind = ((v_i == min_i) & (v_j ==  min_j));
  return l_ind;
}

NumericVector recenter(int ntot, int ndrop, double min_x, NumericVector v_x, NumericVector v_Nx){
  NumericVector v_rc = v_Nx + ((min_x * ndrop) /  (ntot - ndrop));
  
  return(v_rc);
}

// [[Rcpp::export(rng = false)]]
NumericMatrix r_dist_cpp (const NumericMatrix x){
  int nr = x.nrow();
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
        
        LogicalVector l_ind = sz_inx(v_i, v_j, r_min[i], r_min[j]);
        //NumericVector v_i_sub = recenter(v_i, l_ind);
        //NumericVector v_j_sub = recenter(v_j, l_ind);
        int ntot  = l_ind.size(); 
        int ndrop = sum(as<IntegerVector>(l_ind));
        
        //Rcpp::as<NumericVector>(v_x[l_ind]).size();
        double d = dist(recenter(ntot, ndrop, r_min[i], v_i, v_i[!(l_ind)]), 
                        recenter(ntot, ndrop, r_min[j], v_j, v_j[!(l_ind)]));
        out(j,i) = d;
      }
    }
  }
  return (out) ;
}


