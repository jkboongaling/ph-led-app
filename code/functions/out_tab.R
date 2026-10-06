# Create summary table for decomposition
# Depends on configuration objects defined in config.R

out_tab <- function(out) {
  
  cause_totals <- colSums(out$cmat[4:15])
  
  summary_table <- matrix(round(
    c(
      out$e0_2,
      out$e0_1,
      out$e0_diff,
      out$arriaga_d,
      out$arriaga_i,
      out$cmat_grp$age_decomp,
      cause_totals,
      sum(cause_totals)
    ),
    4
  ), ncol = 1)
  
  row.names(summary_table) <- c(
    paste0("Life expectancy at birth - ", out$lbl2),
    paste0("Life expectancy at birth - ", out$lbl1),
    "Life expectancy difference",
    "Direct component",
    "Indirect and interaction component",
    age_lbl,
    causes,
    "Estimated total difference from decomposition"
  )
  
  colnames(summary_table) <- paste0("PH ", out$lbl)
  knitr::kable(summary_table, caption = "e0 Decomposition")

}

