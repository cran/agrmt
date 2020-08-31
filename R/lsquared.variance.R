lsquared.variance = function(V) {
  # Calculate approximate variance of Blair and Lacy's (2000) lsquared (L2^2)
  # V should be a frequency vector
  # From Blair and Lacy 2000: 274
  k = length(V)
  i = 1:length(V)
  N = sum(V)
  pi = V/N
  
  a = (k - 1:k)/2
  for(i in 1:(k-1)) {
    a[i] = a[i] + sum((i - ((i+1):k))*pi[(i+1):k])
  }
  
  lsquared.var = 64*(sum(pi * a^2) - (sum(pi * a))^2)/(N*((k-1)^2))
  return(lsquared.var)
}
