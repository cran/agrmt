l.variance = function(V) {
  # Calculate approximate variance of Blair and Lacy's (2000) l (L2)
  # V should be a frequency vector
  # From Blair and Lacy 2000: 274
  lsquared = concentration(V,2)^2
  l.var = lsquared.variance(V)/(4 * lsquared)
  return(l.var)
}
