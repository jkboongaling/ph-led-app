# Create summary figure for decomposition
# Depends on configuration objects defined in config.R

out_fig <- function(out, y_min = -0.6, y_max = 0.7) {
  
  plot_data <- out$cmat_grp |> 
    tidyr::pivot_longer(cols = c(3:14),
                        names_to = "cause",
                        values_to = "contribution") |> 
    dplyr::mutate(cause = factor(cause, levels = causes))
  
  note <- paste0(
    out$lbl2, " e0 = ", format(round(out$e0_2, 1), nsmall = 1), "\n",
    out$lbl1, " e0 = ", format(round(out$e0_1, 1), nsmall = 1), "\n",
    "\u0394 = ", format(round(out$e0_diff, 1), nsmall = 1)
  )
  
  p <-
    plot_data |> 
    ggplot2::ggplot(ggplot2::aes(x = age_grp, y = contribution, fill = cause)) +
    ggplot2::geom_col() +
    ggplot2::stat_summary(ggplot2::aes(group = 1), fun = sum, geom = "line") +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::scale_fill_manual(values = cause_colors, breaks = causes) +
    ggplot2::scale_y_continuous(breaks = seq(y_min, y_max, 0.1), 
                                limits = c(y_min, y_max),
                                labels = scales::label_number(accuracy = 0.1)) +
    ggplot2::annotate("text", x = 17, y = Inf, hjust = 0, vjust = 1.5, 
                      label = note, size = 3, fontface = "italic") +
    ggplot2::labs(x = "Age group", y = "Contribution (in years)", fill = "Cause",
                  title = out$lbl) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  p
  return(p)
  
}

