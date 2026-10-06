# Life table function
# The input is a vector of age-specific death rates (age 0 to age 100+).
# Sex must be specified as Male ("m") or Female ("f").

life_table <- function(mx, sex) {
  
  N <- length(mx)
  
  # Assume deaths occur, on average, halfway through the age interval
  ax <- rep(0.5, N)
  
  # Infant ax
  if (sex == "m") {
    ax[1] <- ifelse(mx[1] < 0.107, 0.045 + 2.684 * mx[1], 0.330)
  }
  else if (sex == "f") {
    ax[1] <- ifelse(mx[1] < 0.107, 0.053 + 2.800 * mx[1], 0.350)
  }
  
  # Death probability
  # Chiang's conversion from mx to qx
  qx <- mx / (1 + (1 - ax) * mx)
  
  # Last value of qx = 1; everyone dies eventually
  qx[N] <- 1
  
  # Survival probability
  px <- 1 - qx
  
  # Number of survivors
  lx <- c(100000, 100000 * cumprod(px[-N]))
  lx <- pmax(lx, 0)
  
  # Number of deaths
  dx <- lx * qx
  
  # Person-years lived within each age interval
  Lx <- lx[-1] + ax[-N] * dx[-N]
  
  # Person-years lived in the open-ended interval
  Lx[N] <- ifelse(mx[N] > 0, lx[N] / mx[N], 0)
  
  # Total number of person-years lived
  Tx <- rev(cumsum(rev(Lx)))
  
  # Life expectancy at each age
  ex <- Tx / lx
  
  # Output
  age <- 0:(N-1)
  LT <- data.frame(age, mx, lx, dx, Lx, Tx, ex)
  return(LT)
  
}

