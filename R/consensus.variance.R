consensus.variance = function(V) {
  # Calculate the approximate variance of the consensus (Cns) estimator
  # V should be a frequency vector
  # Based off of Blair and Lacy 2000: 274
  k = length(V)
  N = sum(V)
  p = V / N
  mu = mean(expand(V))
  dX = max(expand(V)) - min(expand(V))
  
  jge = ((1:k) < mu) * 2 - 1
  const = - sum(p * jge / ((1 - abs(1:k - mu)/dX)*log(2)*dX))
  a = (1:k)*const + log2(1 - abs(1:k - mu)/dX)
  consensus.var = (sum(p * a^2) - sum(p * a)^2)/N
  
  return(consensus.var)
}
