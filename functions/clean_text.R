clean <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%            
    str_squish()    
}
  

fmt_pct <- function(x) {
  ifelse(
    x %% 1 == 0,                   
    sprintf("%.0f%%", x),       
    sprintf("%.1f%%", x)       
  )
}