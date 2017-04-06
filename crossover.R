crossover <- function(a, b){
  #takes a single chromosome and crossovers and returns both chromosomes
  len <-length(a);
  
  point_1 <-floor(runif(1,min=0,max = len/2));
  point_2 <-(floor(runif(1,min=len/2+1,max = len-1)));
  c = a;
  d = b;
  c[(point_1+1):point_2] = b[(point_1+1):point_2];
  d[(point_1+1):point_2] = a[(point_1+1):point_2];
  return(list(c,d))
}

mutation <- function(a){
  # mutates a chromosome and complements a random feature if prob > 0.5
  rand = runif(1, 0, 1) # calculates a random probability
  rindex = sample(1:length(a), 1) # finds a random feature
  if(rand > 0.0){
    a[rindex] = as.numeric(!a[rindex])
  }
  return(a)
}
# 
# fitness <- function(S, a){
#   indices = which[a == 1]
#   data = S[, indices]
#   i = index.DB(data, )
# }
# a =  sample(c(0,1), replace=TRUE, size=6)
# b = sample(c(0,1), replace=TRUE, size=6)
# 
# print(crossover(a, b))
# print(mutation(a))
