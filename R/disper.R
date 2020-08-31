disper = function(A, B, metric = 2) {
  # measure of dispersion
  # input validation:
  if(length(A) != length(B) | length(A) == 0) stop("Error: A and B must have the same nonzero length")
  if(metric <= 0) stop("Error: metric must be greater than 0")
  # special case
  if(metric == Inf) { return(max(abs(A - B))) }
  # normal case
  return((sum(abs(A - B)^metric))^(1/metric))
}
