dispersion = function(V, metric = 2) {
  # measure of dispersion from measure of concentration
  return(1 - concentration(V, metric))
}
