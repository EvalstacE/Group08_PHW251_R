enforce_factor_levels <- function(x) {
  x_chr <- as.character(x)
  
  left <- sub(" .*", "", x_chr)
  left_clean <- gsub("[+,]", "", left)
  
  lower_num <- case_when(
    grepl("M", left_clean, ignore.case = TRUE) ~ as.numeric(gsub("[^0-9.]", "", left_clean)) * 1e6,
    grepl("K", left_clean, ignore.case = TRUE) ~ as.numeric(gsub("[^0-9.]", "", left_clean)) * 1e3,
    TRUE ~ as.numeric(gsub("[^0-9.]", "", left_clean))
  )
  
  lvl_df <- tibble(label = x_chr, lower_num = lower_num) %>%
    distinct() %>%
    arrange(lower_num)
  
  lvl_order <- lvl_df$label
  
  factor(x_chr, levels = lvl_order, ordered = TRUE)
}