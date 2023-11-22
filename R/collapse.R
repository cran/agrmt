collapse =
  function(D, pos=FALSE, na.rm=TRUE) {
    # Calculates a frequency vector F from a population vector D
    # Arguments:  D  = population vector
    #            pos = positions of the categories
    #                  (necessary if categories with 0 observations exist)
    #                  if not provided, we assume observations in all categories
    #          na.rm = NA are removed (default)
    if(na.rm == TRUE) {
      # remove NA from vector, as.numeric() to get back the vector
      D = as.numeric(na.omit(D))
      }
    if(is.vector(D) == FALSE) {
      # check if we have a vector
      warning("Warning: Expected a vector, or vector is empty.")
      return(NA)}
    if(is.numeric(D[1]) == FALSE) {
      # check if we have numbers in the vector
      warning("Warning: Expected a numeric value in the vector.")
      return(NA)}
    if(is.na(D[1]) == TRUE) {
      # fail with warning if NA are found or empty
      warning("Warning: Expected a vector, vector is empty, or vector starts with NA.")
      return(NA)}
    if(na.rm==TRUE){T = as.data.frame(table(D))}                  # table(D) to count frequencies
    else{T = as.data.frame(table(D, useNA="ifany"))}  # table(D) to count frequencies
    F = T[,2]                    # [,2] chooses the values
    l = length(pos)              # number of categories specified
    if (!(pos[1] == FALSE & pos[l] == FALSE)) {   # number of positions is provided:
      # if first value of pos is not zero, and last value is not zero
      F2 = F           # source of values
      k = length(pos)  # number of positions
      F = rep(0,k)     # empty with length(k)
      V = unique(D)    # chooses the categories (positions) of the values in F2
      V = V[!is.na(V)] # remove NA from unique values
      V = sort(V)      # keep order that is messed up by the preceding line
      m = length(V)    # number of non-zero values to be filled into F
      for(i in 1:m) {
        for(j in 1:k) {
          if (pos[j] == V[i]) F[j] = F2[i] # fill in frequencies at the correct positions
        }
      }
    }
    return(F)
  }
