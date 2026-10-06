# Decompose time differentials
# Depends on configuration objects defined in config.R

decompose_time <- function(year1, year2, sex, mx_data, cod_data) {

  # Mortality rates
  mx1 <- mx_data |> 
    dplyr::filter(year == year1, sex == .env$sex) |> 
    dplyr::arrange(age) |> 
    dplyr::pull(mx)
  
  mx2 <- mx_data |> 
    dplyr::filter(year == year2, sex == .env$sex) |> 
    dplyr::arrange(age) |> 
    dplyr::pull(mx)
  
  # Cause-of-death proportions
  cause1 <- cod_data  |> 
    dplyr::filter(year == year1, sex == .env$sex) |> 
    dplyr::select(age, cause, proportion) |>
    tidyr::complete(age = 0:100, cause = causes, fill = list(proportion = 0)) |> 
    tidyr::pivot_wider(names_from = cause,  
                       values_from = proportion, 
                       values_fill = list(proportion = 0),
                       names_repair = "minimal") |> 
    dplyr::arrange(age) |> 
    dplyr::select(dplyr::all_of(causes))
  
  cause2 <- cod_data |> 
    dplyr::filter(year == year2, sex == .env$sex) |> 
    dplyr::select(age, cause, proportion) |> 
    tidyr::complete(age = 0:100, cause = causes, fill = list(proportion = 0)) |> 
    tidyr::pivot_wider(names_from = cause,  
                       values_from = proportion, 
                       values_fill = list(proportion = 0),
                       names_repair = "minimal") |> 
    dplyr::arrange(age) |> 
    dplyr::select(dplyr::all_of(causes))
  
  na_mask1 <- is.na(cause1)
  na_mask2 <- is.na(cause2)
  
  cause1[na_mask1] <- 0
  cause2[na_mask2] <- 0
  
  if (year1 >= 2006) cause1[100:101,] <- cause1[99, ]
  if (year2 >= 2006) cause2[100:101,] <- cause2[99, ]
  
  # Life tables
  lt1 <- life_table(mx1, sex)
  lt2 <- life_table(mx2, sex)
  
  # Arriaga decomposition
  age_decomp1 <- arriaga(mx1, mx2, sex1 = sex, breakdown = TRUE)
  
  # Total age-specific contributions
  age_decomp2 <- age_decomp1$direct + age_decomp1$indirect
  
  # Cause-specific decomposition
  cause_comp <- cause_decomp(age_decomp2, mx1, mx2, cause1, cause2)
  
  # Return output
  list(
    yr1 = year1,
    yr2 = year2,
    sex1 = sex,
    e0_1 = lt1$ex[1],
    e0_2 = lt2$ex[1],
    e0_diff = lt2$ex[1] - lt1$ex[1],
    arriaga_d = sum(age_decomp1$direct),
    arriaga_i = sum(age_decomp1$indirect),
    cmat = cause_comp$cause_mat1,
    cmat_grp = cause_comp$cause_mat2,
    lbl = ifelse(sex=="m", "Males", "Females"),
    lbl1 = year1,
    lbl2 = year2
  )
  
}

