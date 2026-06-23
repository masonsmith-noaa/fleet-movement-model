#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector bootstrap_psc_cpp(int n_iterations, 
                                NumericVector displaced_gf, 
                                NumericVector psc_pool, 
                                NumericVector psc_inside_wk) {
  
  int n_obs = displaced_gf.size();
  NumericVector results(n_iterations);
  RNGScope scope; 
  
  // Calculate the total observed inside PSC once to save time
  double total_observed_inside = sum(psc_inside_wk);
  
  for(int i = 0; i < n_iterations; ++i) {
    double total_new_psc = 0;
    
    for(int j = 0; j < n_obs; ++j) {
      // Use R::runif and floor to get a random index
      int random_idx = floor(R::runif(0, psc_pool.size()));
      total_new_psc += displaced_gf[j] * psc_pool[random_idx];
    }
    
    // Net Change = Simulated Outside PSC - Total Observed Inside PSC
    results[i] = total_new_psc - total_observed_inside;
  }
  
  return results;
}