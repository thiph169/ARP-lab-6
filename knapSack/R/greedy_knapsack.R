# 
#' Greedy Heuristic approach for the knapsack problem
#'
#' @param x data frame with two columns \code{w} (weight) and \code{v} (value) of items to place in the knapsack
#' 
#' @param W the maximum weight (numeric) the knapsack can hold
#' 
#' @param fast whether to use the rcpp optimization
#'
#' @return theoretical maximum \code{$value} (knapsack value) composed of \code{$elements} (which items)
#' 
#' @export
#' 


greedy_knapsack<- function(x, W, fast = FALSE){
  
  stopifnot(all(x >= 0))
  stopifnot(class(x)=="data.frame") 
  stopifnot(W >= 0)
  
  
  weight<- x[,1]
  value<- x[,2]
  r<- value/weight
  r_sort<- order(r,decreasing=TRUE)
  
  items<- numeric(0)
  totWeight<- 0
  totValue<- 0
  
  for (i in r_sort){
    if(weight[i] + totWeight > W){
      break
    }
    if((weight[i] + totWeight) <= W){
      items<- c(items, i)
      totWeight<- weight[i] + totWeight
      totValue<- value[i] + totValue
    }
  }
  ls<- list(value = round(totValue), elements = items)
  return(ls)
}



