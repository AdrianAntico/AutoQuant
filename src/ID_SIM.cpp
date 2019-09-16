#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector QRGibbsSim(NumericVector CountScore, NumericVector SizeScore, NumericVector CountList, NumericVector SizeList, int nSims) {
  
  // Initial variables
  int i,j,k,l,CountLoops,SizeLoops;
  double rn1,rn2,count_min_val,count,count_max_val,sumQ,sumq,size_min_val,size_max_val;
  CountLoops = CountList.size();
  SizeLoops = SizeList.size();
  count_min_val = CountList[0];
  count_max_val = CountList[CountLoops-1];
  size_min_val = SizeList[0];
  size_max_val = SizeList[SizeLoops-1];
  NumericVector store(nSims);
  
  // Count Quantiles Loop
  for (i=0; i < nSims; i++) {
    rn1 = rand() / (RAND_MAX + 1.);
    k = ceil(rn1 * (CountLoops-1));
    if(rn1 <= count_min_val) {
      count = floor(rand() / (RAND_MAX + 1.) + ceil(CountScore[0]));
    } else if (rn1 >= count_max_val) {
      count = floor(rand() / (RAND_MAX + 1.) + ceil(CountScore[CountLoops]));
    } else {
      count = floor(rand() / (RAND_MAX + 1.) + ceil(CountScore[k]*pow((CountScore[k+1]+1)/(CountScore[k]+1),(rn1 - CountList[k])/(CountList[k+1]-CountList[k]))));
    }
    
    // Continue if count is positive
    if (count > 0) {
      sumQ = 0;
      
      // Size Quantiles Loop
      for (j=0; j < count; j++) {
        sumq = 0;
        rn2 = rand() / (RAND_MAX + 1.);
        l = ceil(rn2 * (SizeLoops-1));
        if(rn2 <= size_min_val) {
          sumq = floor(rand() / (RAND_MAX + 1.) + ceil(SizeScore[0]));
        } else if (rn2 >= size_max_val) {
          sumq = floor(rand() / (RAND_MAX + 1.) + ceil(SizeScore[SizeLoops-1]));
        } else {
          sumq = floor(rand() / (RAND_MAX + 1.) + ceil(SizeScore[l]*pow((SizeScore[l+1]+1)/(SizeScore[l]+1),(rn2 - SizeList[l])/(SizeList[l+1] - SizeList[l]))));
        }
        if(sumq <= 1) {
          sumQ += 1;
        } else {
          sumQ += sumq;          
        }
      }
      store[i] = sumQ;
    } else {
      store[i] = 0;
    }
  }
  return store;
}