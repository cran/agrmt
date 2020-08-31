censor = function(V, left = 0, right = 100) {
  V[V < left] = 0.0000000000001
  V[V > right] = right
  return(V)
}
