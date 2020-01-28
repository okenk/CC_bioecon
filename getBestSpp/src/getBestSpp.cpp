#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]

IntegerVector getBestSpp(NumericMatrix exp_profit) {

  int nships = exp_profit.nrow();
  int npops = exp_profit.ncol();

  IntegerVector best_spp(nships);
  double max;
  int max_ind;

  for(int ship = 0; ship < nships; ship++) {
    max = 0;
    for(int spp = 0; spp < npops; spp++) {
      if(exp_profit(ship,spp) > max) {
        max = exp_profit(ship,spp);
        max_ind = spp + 1;
      }
      if (max > 0) {
        best_spp(ship) = max_ind;
      } else {
        best_spp(ship) = NA_INTEGER;
      }
    }
  }
  return best_spp;
}
