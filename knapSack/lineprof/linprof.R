# devtools::install_github("hadley/lineprof") 


#------------------------------
set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
#------------------------------
#source(paste(getwd(),"celcius","R","brute_force_knapsack.R", sep = "/"))


library(lineprof)

source(paste(getwd(),"knapSack","R","brute_force_knapsack.R", sep = "/"))
brute_lineprof <- lineprof(brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500))
brute_lineprof


source(paste(getwd(),"knapSack","R","dynamic_knapsack.R", sep = "/"))
dynamic_lineprof <- lineprof(knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500))
dynamic_lineprof


source(paste(getwd(),"knapSack","R","greedy_knapsack.R", sep = "/"))
greedy_lineprof <- lineprof(greedy_knapsack(x = knapsack_objects[1:800,], W = 3500))
greedy_lineprof


