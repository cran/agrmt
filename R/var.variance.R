var.variance = function(V) {
  # Calculate the approximate variance of the categorical variance estimator
  # V should be a frequency vector
  # Based off of Blair and Lacy 2000: 274
  k = length(V)
  N = sum (V)
  p = V / N
  mu = mean(expand(V))
  
  const = -sum(2*p*((1:k)-mu))
  a = (N / (N - 1)) * ((1:k) * const + ((1:k) - mu)^2)
  
  var.var = (sum(p * a^2) - (sum(p * a))^2)/N
  return(var.var)
}
