#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector minDistTimeState(NumericMatrix query, NumericMatrix target, NumericVector time, double scale) {
  int n = query.nrow(), d = query.ncol();
  NumericVector out(n);
  double v;

  // iterate through query
  for (int i = 0; i < n; ++i) {
    // start search for minimization at target index s = query index i
    int sMin = i;
    double minDist = 0;
    for (int k = 0; k < d; ++k) {
      v = query(i, k) - target(sMin, k);
      minDist += v*v;
    } // times are the same, so no additional term
    // search target index with increasing distance to query index i
    // Part 1: s > i
    for (int s = i+1; s < n; ++s) {
      v = scale*(time[i] - time[s]);
      double dst = v*v;
      // time distance will not get smaller.
      // If it is already larger than the smallest distance so far, stop the search.
      if (dst > minDist) break;
      for (int k = 0; k < d; ++k) {
        v = query(i, k) - target(s, k);
        dst += v*v;
      }
      if (dst < minDist) {
        sMin = s;
        minDist = dst;
      }
    }
    // Part 2: s < i
    for (int s = i-1; s >= 0; --s) {
      v = scale*(time[i] - time[s]);
      double dst = v*v;
      // time distance will not get smaller.
      // If it is already larger than the smallest distance so far, stop the search.
      if (dst > minDist) break;
      for (int k = 0; k < d; ++k) {
        v = query(i, k) - target(s, k);
        dst += v*v;
      }
      if (dst < minDist) {
        sMin = s;
        minDist = dst;
      }
    }
    out[i] = sqrt(minDist);
  }
  return out;
}


// [[Rcpp::export]]
NumericVector minDist(NumericMatrix query, NumericMatrix target) {
  int n = query.nrow(), d = query.ncol();
  NumericVector out(n);
  double v, dst;

  // iterate through query
  for (int i = 0; i < n; ++i) {
    // start search for minimization at target index s = query index i
    double minDist = INFINITY;
    for (int s = 0; s < n; ++s) {
      dst = 0;
      for (int k = 0; k < d; ++k) {
        v = query(i, k) - target(s, k);
        dst += v*v;
      }
      if (dst < minDist) {
        minDist = dst;
      }
    }
    out[i] = sqrt(minDist/d);
  }
  return out;
}
