# Decomposition by cause
# Depends on configuration objects defined in config.R

cause_decomp <- function(age_decomp, mx1, mx2, cause1, cause2) {
  
  denominator <- ifelse(mx2 - mx1 == 0, 1, mx2 - mx1)
  
  cause_fac1 <- cause1 * mx1 / denominator * age_decomp
  cause_fac2 <- cause2 * mx2 / denominator * age_decomp
  
  cause_mat1 <- cause_fac2 - cause_fac1
  cause_mat1 <- data.frame(age = 0:100, age_grp, age_decomp, cause_mat1, 
                           check.names = FALSE)
  
  cause_mat2 <- cause_mat1 |>
    dplyr::summarise(dplyr::across(-c(age), \(x) sum(x, na.rm = TRUE)), 
                     .by = age_grp)
  
  list(cause_mat1 = cause_mat1, cause_mat2 = cause_mat2)
  
}

