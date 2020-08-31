concentration <- function(V, metric = 2) {
  # measure of concentration
  V_F = cumsum(V/sum(V))              # empirical CMF
  k = length(V_F)
  V_Fk = V_F[1:(k-1)]                 # F up to k-1
  maxdisp_Fk = rep(x = 1/2, k - 1)    # the CMF for the point of maximal dispersion
  maxconc_Fk = c(1,rep(x = 0, k - 2)) # the CMF for the point of maximal concentration
  
  D = disper(V_Fk, maxdisp_Fk, metric)
  D_max = disper(maxdisp_Fk, maxconc_Fk, metric)
  return(D/D_max)
}
