`majority` <- function(x){
  x <- as.factor(x)
  n <- nlevels(x)
  votes <- rep(0,n)
  for (i in 1:length(x)) votes[as.integer(x[i])] <- votes[as.integer(x[i])]+1
  levels(x)[order(votes,decreasing=TRUE,sample(1:n,n))[1]]
  }

