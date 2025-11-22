# Load and install packages
pacman::p_load(
  dplyr, tidyr, readr, here, janitor, lubridate,  
  rlang, stringr, purrr, ggplot2, ggthemes, knitr,
  kableExtra, MMWRweek, scales, sf, tigris, classInt
)

options(tigris_use_cache = TRUE)

# Bring in helper functions
func_dir <- here::here("functions")
r_files <- list.files(func_dir, pattern = "\\.R$", full.names = TRUE)
purrr::walk(r_files, source)


# Bring in datasets
data_dirs <- c(
  here::here("data", "cleaned_data"),
  here::here("data", "inf_rate_dfs")
)

csv_files <- purrr::map(data_dirs, ~ list.files(
  path = .x,
  pattern = "\\.csv$",
  full.names = TRUE
)) %>% unlist()

purrr::walk(csv_files, function(file_path) {
  obj_name <- tools::file_path_sans_ext(basename(file_path))
  df <- readr::read_csv(file_path, show_col_types = FALSE)
  assign(obj_name, df, envir = knitr::knit_global())
})
