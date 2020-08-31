Reardon = function(V) {
  # Calculate Reardon's (2009) entropy
  K = length(V)
  Fi = cumsum(V/sum(V))
  summand = Fi*log2(1/Fi) + (1 - Fi)*log2(1/(1 - Fi))
  r = (1/(K-1)) * sum(summand[1:(K-1)]) #sum only the first K-1 terms
  return(r)
}
