#' Brute force approach for the knapsack problem with parallel.
#'
#' @param x data frame with two columns \code{w} (weight) and \code{v} (value) of items to place in the knapsack #' 
#' @param W the maximum weight (numeric) the knapsack can hold
#' 
#' @param fast whether to use the rcpp optimization
#'
#' @return theoretical maximum \code{$value} (knapsack value) composed of \code{$elements} (which items) #' 
#' @export
#'
set.seed(42)
n <- 2000
knapsack_objects <-
    data.frame(
      w=sample(1:4000, size = n, replace = TRUE),
      v=stats::runif(n = n, 0, 10000)
    )
  return(knapsack_objects)

brute_force_knapsack_parallel <- function(x,W, parallel = TRUE){
  suppressMessages(require(combinat, quietly = TRUE))
  suppressMessages(require(parallel, quietly = TRUE))
  
  if(!is.data.frame(x)){
    stop("x must be a data.frame")
  }
  
  if(ncol(x) != 2){
    stop("x should only have 2 columns, a value columns and a weight column")
  }
  
  if(!all(names(x) %in% c("v","w"))){
    stop("Names of columns in x should only be 'v' for value and 'w' for weight")
  }
  
  if(length(W) >1 | !is.numeric(W)){
    stop("W must be numeric with length 1 ")
  }
  
  if(any(x[,c("w","v")] < 0)){
    stop("Data.frame contains non-positive values")
  }
  
  
  if(parallel){
    if(Sys.info()["sysname"] == "Windows"){
      stop("The parallel function can not be used in a Windows system")
    } else {
      
      cores = parallel::detectCores()
      
      comb <- unlist(parallel::mclapply(1:nrow(x), 
                                        function(i){
                                          combinat::combn(rownames(x), 
                                                          m = i, 
                                                          simplify = FALSE, 
                                                          fun = as.numeric)},
                                        mc.cores = cores),
                     recursive = FALSE)
      
      values <- parallel::mclapply(comb, 
                                   function(comb){ifelse(sum(x[comb,"w"]) <=W,
                                                         sum(x[comb,"v"]),
                                                         0)},
                                   mc.cores = cores)
      
    }
  } else {
    
    comb <- unlist(lapply(1:nrow(x), 
                          function(i){
                            combinat::combn(rownames(x), 
                                            m = i, 
                                            simplify = FALSE, 
                                            fun = as.numeric)}),
                   recursive = FALSE)
    
    values <- lapply(comb, function(comb){ifelse(sum(x[comb,"w"]) <=W,
                                                 sum(x[comb,"v"]),
                                                 0)})
  }
  
  
  
  return(list(
    "value" =  round(values[[which.max(values)]],digits = 0),
    "elements" = comb[[which.max(values)]]
  ))
}
brute_force_knapsack_parallel(x = knapsack_objects[1:12,], W = 3500)