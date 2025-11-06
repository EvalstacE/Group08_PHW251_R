clean <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%            
    str_squish()    
}
  