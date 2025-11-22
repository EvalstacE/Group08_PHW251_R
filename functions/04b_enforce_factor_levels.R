enforce_factor_levels <- function(x) {
  x_chr <- as.character(x)
  
  # get left side of the interval, e.g. "0", "729", "1K", "4K+"
  left <- sub(" .*", "", x_chr)
  
  # strip commas, plus signs, etc.
  left_clean <- gsub("[+,]", "", left)
  
  # convert K/M notation to numeric
  lower_num <- case_when(
    grepl("M", left_clean, ignore.case = TRUE) ~ as.numeric(gsub("[^0-9.]", "", left_clean)) * 1e6,
    grepl("K", left_clean, ignore.case = TRUE) ~ as.numeric(gsub("[^0-9.]", "", left_clean)) * 1e3,
    TRUE ~ as.numeric(gsub("[^0-9.]", "", left_clean))
  )
  
  # build level order by sorting on numeric lower bound
  lvl_df <- tibble(label = x_chr, lower_num = lower_num) %>%
    distinct() %>%
    arrange(lower_num)
  
  lvl_order <- lvl_df$label
  
  factor(x_chr, levels = lvl_order, ordered = TRUE)
}