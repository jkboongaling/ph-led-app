# Define standard age groups, cause of death categories, colors, and themes.

# Define age groups
age_brk <- c(0, 1, seq(5, 95, by = 5), Inf)
age_lbl <- c("0", "1-4", paste(seq(5, 90, by = 5), seq(9, 94, by = 5), sep = "-"), "95+")
age_grp <- cut(0:100, breaks = age_brk, right = FALSE, labels = age_lbl)

# Define cause of death list
causes <- 
  
  c(
  "Infectious diseases",
  "Neoplasms",
  "Diabetes",
  "Ischemic heart disease",
  "Stroke",
  "Other cardiovascular diseases",
  "Respiratory diseases",
  "Suicide",
  "Transport accidents",
  "Other external causes",
  "COVID-19",
  "Others"
  )

# Define cause of death color
cause_colors <- 
  
  c(
  "Infectious diseases"              = "#33a02c", 
  "Neoplasms"                        = "#6a3d9a",  
  "Diabetes"                         = "#ffbf00",  
  "Ischemic heart disease"           = "#b2182b",  
  "Stroke"                           = "#ef3b2c",  
  "Other cardiovascular diseases"    = "#fb6a4a",  
  "Respiratory diseases"             = "#1f78b4", 
  "Suicide"                          = "#7b3f00",   
  "Transport accidents"              = "#b86b3e",   
  "Other external causes"            = "#d8b365",   
  "COVID-19"                         = "#008080",   
  "Others"                           = "#4d4d4d"
  )

