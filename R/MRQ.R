MRQ <- function(Z) {
  # polarization index (Montalvo and Reynal-Querol 2005); cited in Ziller 2014
  z <- length(Z)
  4 * sum(Z[1:z]^2*(1-Z))
  }
