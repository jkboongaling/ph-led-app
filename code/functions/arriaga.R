# Arriaga decomposition function
# The input consists of two vectors of age-specific death rates.
# Sex must be specified as Male ("m") or Female ("f").
# Breakdown argument determines the output.
# FALSE: total contributions by age
# TRUE : direct and indirect + interaction effects

arriaga <- function(nmx1, nmx2, sex1, sex2 = sex1, breakdown = FALSE) {
  
  # Create two life tables
  LT1 <- life_table(nmx1, sex1)
  LT2 <- life_table(nmx2, sex2)
  
  # Specify life table functions needed to perform Arriaga decomposition
  lx1 <- LT1$lx
  lx2 <- LT2$lx
  Lx1 <- LT1$Lx
  Lx2 <- LT2$Lx
  Tx1 <- LT1$Tx
  Tx2 <- LT2$Tx
  
  if (breakdown == FALSE) {
    
    delta <- rep(0, 101)
    
    for (i in 1:100) {
      delta[i] <-
        (lx1[i] / lx1[1]) * 
        (Lx2[i] / lx2[i] - Lx1[i] / lx1[i]) +
        (Tx2[i + 1] / lx1[1]) * 
        (lx1[i] / lx2[i] - lx1[i + 1] / lx2[i +1])
    }
    
    delta[101] <-
      (lx1[101] / lx1[1]) * 
      (Tx2[101] / lx2[101] - Tx1[101] / lx1[101])
    
  }
  
  if (breakdown == TRUE) {
    
    direct <- rep(0, 101)
    indirect <- rep(0, 101)
    
    for (i in 1:100) {
      direct[i] <-
        (lx1[i] / lx1[1]) * 
        (Lx2[i] / lx2[i] - Lx1[i] / lx1[i])

      indirect[i] <-
        (Tx2[i + 1] / lx1[1]) * 
        (lx1[i] / lx2[i] - lx1[i + 1] / lx2[i + 1])
    }
    
    direct[101] <-
      (lx1[101] / lx1[1]) * 
      (Tx2[101] / lx2[101] - Tx1[101] / lx1[101])
    
    delta <- data.frame(age = 0:100, direct = direct, indirect = indirect)
    
  }

  return(delta)
  
}

