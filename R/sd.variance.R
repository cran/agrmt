sd.variance = function(V) {
  # Calculate the approximate variance of the categorical standard deviation
  # V should be a frequency vector
  var = var(expand(V))
  sd.var = var.variance(V)/(4 * var)
  return(sd.var)
}
