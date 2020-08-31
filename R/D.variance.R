D.variance = function(V) {
  # Calculate the approximate variance of Leik's (1966) D (L1)
  # V should be a frequency vector
  # Based off of Blair and Lacy 2000: 274
  
  N = sum(V)
  k = length(V)
  p = V/N
  F = cumsum(p)
  
  a = rep(x = 0, times = k) #(k - 1:k)*2/(k-1)
  for(i in 1:k) {
    F.ghalf = (F >= 1/2)*1
    F.lhalf = (F < 1/2)*1
    npos = sum(F.ghalf[i:k])
    nneg = sum(F.lhalf[i:k])
    a[i] = (npos - nneg)* 2 / (k-1)
  }
  
  D.var = (sum(p * a^2) - sum(p * a)^2)/N
  return(D.var)
}
