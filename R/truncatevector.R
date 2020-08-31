truncatevector = function(V, left = 0, right = 100) {
  W = V[V >= left & V <= right]
  return(W)
}
