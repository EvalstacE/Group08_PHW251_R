# Load packages
pacman::p_load(
  dplyr, tidyr, ggplot2, lubridate, ggthemes, cowplot, readr, rlang, purrr,
  classInt, tidycensus, sf, here, stringr, svglite, rmapshaper, readxl,
  scales, ggrepel, viridis, RColorBrewer, maps, ggfx, glue, skimr,
  DataExplorer, knitr, kableExtra, janitor, reactable, MMWRweek
)

# Where helper .R files live (avoid relying on here() for this)
func_dir <- "_functions"

r_files <- list.files(func_dir, pattern = "\\.R$", full.names = TRUE)

# Source into the knit global env so chunks can see the functions
purrr::walk(
  r_files,
  ~ base::source(.x, local = knitr::knit_global(), encoding = "UTF-8")
)

message("✅ _common.R sourced (packages loaded, helpers available).")