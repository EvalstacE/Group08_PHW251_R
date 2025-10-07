library(dplyr)
library(tidyr)
library(purrr)
library(janitor)
library(gt)


example_vals <- function(x, n = 5) {
  u <- unique(x)
  u <- u[!is.na(u)]
  paste(head(as.character(u), n), collapse = ", ")
}


one_dataset_spec <- function(df, dataset_label) {
  stopifnot(is.data.frame(df))
  
  # keep original names; also compute a standardized name to propose
  std_names <- janitor::make_clean_names(names(df), case = "snake")
  
  meta <- tibble(
    variable     = names(df),
    column_name  = names(df),
    std_name     = std_names,
    type         = map_chr(df, ~ class(.x)[1]),
    missing_n    = map_int(df, ~ sum(is.na(.x))),
    unique_n     = map_int(df, ~ dplyr::n_distinct(.x)),
    mean         = map_dbl(df, ~ if (is.numeric(.x) && any(!is.na(.x))) mean(.x, na.rm = TRUE) else NA_real_),
    median       = map_dbl(df, ~ if (is.numeric(.x) && any(!is.na(.x))) stats::median(.x, na.rm = TRUE) else NA_real_),
    range        = map_chr(df, ~ if (is.numeric(.x) && any(!is.na(.x))) {
      r <- range(.x, na.rm = TRUE); paste0(r[1], "-", r[2])
    } else NA_character_),
    examples     = map_chr(df, example_vals)
  )
  
  meta %>%
    pivot_longer(
      cols = -variable,
      names_to = "field",
      values_to = dataset_label
    )
}





build_comparison_spec <- function(dfs_named_list) {
  # dfs_named_list should be like: list("CA dataset" = ca_df, "LA County dataset" = la_df, "Population dataset" = pop_df)
  
  specs <- imap(dfs_named_list, ~ one_dataset_spec(.x, .y))
  spec <- reduce(specs, full_join, by = c("variable", "field"))
  
  # nice row order for the stub
  field_order <- c("column_name","std_name","type","missing_n","unique_n","mean","median","range","examples")
  spec %>%
    mutate(field = factor(field, levels = field_order)) %>%
    arrange(variable, field) %>%
    mutate(field = as.character(field)) %>%
    # optional empty column for hand-written notes
    mutate(cleaning_notes = NA_character_)
}



# read your raw files (adjust paths as needed)
ca_df  <- read.csv("_data/scenario_1/sim_novelid_CA.csv")
la_df  <- read.csv("_data/scenario_1/sim_novelid_LACounty.csv")
pop_df <- read.csv("_data/scenario_1/ca_pop_2023.csv")

spec_tbl <- build_comparison_spec(list(
  "CA dataset"          = ca_df,
  "LA County dataset"   = la_df,
  "Population dataset"  = pop_df
))



