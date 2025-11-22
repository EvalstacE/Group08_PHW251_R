create_case_cnt_lbl <- function(x) {
  dplyr::case_when(
    x < 1 ~ paste0("0"),
    between(x, 1, 99) ~ paste0("1-99"),
    x < 1000 ~ paste0(floor(x / 100) * 100, "+"),
    x < 10000 ~ paste0(floor(x / 1000), "K+"),
    x < 100000 ~ paste0(floor(x / 10000) * 10, "K+"),
    x < 1000000 ~ paste0(floor(x / 100000) * 100, "K+"),
    TRUE ~ ">1M"
  )
}


create_case_rate_lbl <- function(x) {
  dplyr::case_when(
    x < 1 ~ "0",
    x >= 1   & x <= 50   ~ "1–50",
    x >= 51  & x <= 100  ~ "51–100",
    x >= 101 & x <= 300  ~ "101–300",
    x >= 301 & x <= 500  ~ "301–500",
    x >= 501 & x <= 1000  ~ "501–1000",    
    x < 10000 ~ paste0(floor(x / 1000), "K+"),
    x < 100000 ~ paste0(floor(x / 10000) * 10, "K+"),
    x < 1000000 ~ paste0(floor(x / 100000) * 100, "K+"),
    TRUE ~ ">1M"
  )
}