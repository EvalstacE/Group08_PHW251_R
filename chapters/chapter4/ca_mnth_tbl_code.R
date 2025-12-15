
source("_common04.R")

###########################################
###########################################
###########################################
##                                       ##
##    Set up dataframes for reactable    ##
##                                       ##
###########################################
###########################################

mnth_order <- factor(c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), ordered = TRUE)

ca_weekly_cases <- combined_df %>%
  group_by(mmwr_year, mmwr_week, start_date, end_date) %>%
  summarise(
    total_ca_pop        = first(total_ca_pop),
    cumulative_infected = sum(cumulative_infected),
    cumulative_severe   = sum(cumulative_severe), 
    new_infections      = sum(new_infections),
    new_severe          = sum(new_severe),
    cum_rate_100k       = round((10^5*cumulative_infected/total_ca_pop), 1),
    inf_rate_100k       = round((10^5*new_infections/total_ca_pop), 1),
    sev_rate_100k       = round((10^5*new_severe/total_ca_pop), 1),
    .groups             = "drop"
  ) %>%
  mutate(
    start_date = lubridate::as_date(start_date),
    end_date   = lubridate::as_date(end_date),
    mnth_lbl  = lubridate::month(start_date, label = TRUE),
    health_officer_region = "Statewide", 
    county = "Statewide", 
    total_cnty_pop = NA_real_
  ) %>%
  relocate(mnth_lbl, .after = end_date)


cnty_weekly_cases <- combined_df %>%
  group_by(health_officer_region, county, mmwr_year, mmwr_week, start_date, end_date) %>%
  summarise(
    total_ca_pop        = first(total_ca_pop),
    total_cnty_pop      = first(total_cnty_pop),
    cumulative_infected = first(cumulative_infected),
    cumulative_severe   = first(cumulative_severe), 
    new_infections      = sum(new_infections),
    new_severe          = sum(new_severe),
    cum_rate_100k       = round((10^5*cumulative_infected/total_cnty_pop), 1),
    inf_rate_100k       = round((10^5*new_infections/total_cnty_pop), 1),
    sev_rate_100k       = round((10^5*new_severe/total_cnty_pop), 1),
    .groups             = "drop"
  ) %>%
  mutate(
    start_date = lubridate::as_date(start_date),
    end_date   = lubridate::as_date(end_date),
    mnth_lbl  = lubridate::month(start_date, label = TRUE)
  ) %>%
  relocate(mnth_lbl, .after = end_date)


ca_monthly_cases <- ca_weekly_cases %>%
  group_by(mnth_lbl) %>%
  summarise(
    total_ca_pop        = first(total_ca_pop),
    cumulative_infected = sum(cumulative_infected),
    cumulative_severe   = sum(cumulative_severe), 
    new_infections      = sum(new_infections),
    new_severe          = sum(new_severe),
    cum_rate_100k       = round((10^5*cumulative_infected/total_ca_pop), 1),
    inf_rate_100k       = round((10^5*new_infections/total_ca_pop), 1),
    sev_rate_100k       = round((10^5*new_severe/total_ca_pop), 1),
    .groups             = "drop"
  ) %>%
  mutate(
    mnth_lbl = factor(mnth_lbl, levels = mnth_order, ordered = TRUE)
    ) %>%
  arrange(mnth_lbl)


ca_mnth_wide <- ca_monthly_cases %>%
  select(mnth_lbl,  new_infections, inf_rate_100k, new_severe, sev_rate_100k) %>%
  mutate(
    new_infections = round(new_infections), 
    new_severe = round(new_severe)
  ) %>%
    
  tidyr::pivot_longer(
    cols      = -mnth_lbl,
    names_to  = "metric",
    values_to = "value"
  ) %>%
  
  tidyr::pivot_wider(
    names_from  = mnth_lbl,
    values_from = value,
    values_fill = 0
  ) 


all_wk_df <- rbind(cnty_weekly_cases, ca_weekly_cases)

##-- Save dfs 
#-write.csv(cnty_weekly_cases, here("data/inf_rate_dfs/cnty_weekly_cases.csv"), row.names = FALSE)
#-write.csv(ca_weekly_cases, here("data/inf_rate_dfs/ca_weekly_cases.csv"), row.names = FALSE)
#-write.csv(ca_monthly_cases, here("data/inf_rate_dfs/ca_monthly_cases.csv"), row.names = FALSE)
#-write.csv(ca_mnth_wide, here("data/inf_rate_dfs/ca_mnth_wide.csv"), row.names = FALSE)
#-write.csv(all_wk_df, here("data/inf_rate_dfs/all_wk_df.csv"), row.names = FALSE)

###########################################
###########################################
###########################################
###########################################
##                                       ##
##      Objects needed for reactable     ##
##                                       ##
###########################################
###########################################

data <- ca_mnth_wide

metric_map <- c(
  new_infections = "New Infections",
  inf_rate_100k  = "Infection Rate (per 100k)",
  new_severe     = "Severe Infections",
  sev_rate_100k  = "Severe Infection Rate (per 100k)"
)

data <- data %>%
  mutate(
    metric = recode(metric, !!!metric_map),
    metric = factor(metric, levels = unname(metric_map))
  )

months <- c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
mnth_cols <- months


##-- color scale
pnk_purp_scale <- c(
  "#f1f0ea",
  "#52176b",
  "#1e0c47"
)

pal <- grDevices::colorRamp(pnk_purp_scale)

row_totals <- data %>%
  dplyr::select(dplyr::all_of(months)) %>%
  as.matrix() %>%
  rowSums(na.rm = TRUE)


###########################################
###########################################
###########################################
###########################################
##                                       ##
##  Function to scale background color   ##
##                                       ##
###########################################
###########################################


##-- month columns with scaled background color
month_cols <- setNames(lapply(months, function(m) {
  reactable::colDef(
    name     = m,
    #maxWidth = 75,
    align    = "center",

    cell  = function(value) {
      if (is.na(value) || value == 0) "\u2013" else format(value, big.mark = ",")
    },
    style = function(value, index) {
      total <- row_totals[index]
      
      if (is.na(value) || is.na(total) || value == 0 || total == 0 || total < 10) {
        return(list(color = "#dadada"))
      }
      
      share <- value / total
      share <- max(0, min(1, share))  # clamp 0–1
      
      rgbv <- pal(share)
      bg   <- grDevices::rgb(rgbv[1], rgbv[2], rgbv[3], maxColorValue = 255)
      
      # --calcs to adjust text coor
      r <- rgbv[1] / 255
      g <- rgbv[2] / 255
      b <- rgbv[3] / 255
      lum <- 0.2126 * r + 0.7152 * g + 0.0722 * b
      text_col <- if (lum < 0.5) "#ffffff" else "#111111"
      
      list(
        background = bg, 
        color = text_col,
        fontWeight = 600,
        fontSize = "14px"
      )
      
    }
  )
  
}), months)




###########################################
###########################################
###########################################
###########################################
##                                       ##
##         Build the Reactable           ##
##                                       ##
###########################################
###########################################


ca_mnth_tbl <- 
  
reactable::reactable(
  data,
  pagination = FALSE,
  defaultSorted    = "metric",
  defaultSortOrder = "asc",
  class = "reactable",
  width = "auto",

  defaultColDef = reactable::colDef(
    vAlign = "center",
    headerVAlign = "bottom",
    sortable    = FALSE ,
    style = list(fontSize = "16px")
  ),
    
  
  columnGroups = list(
    
    reactable::colGroup(
      name = "",
      columns = mnth_cols,
    )
  ),
    
  
  columns = c(
      list(
        metric = reactable::colDef(
          name = "Metric",
          align = "left",
          minWidth = 120
        )
        
      ),
      
      month_cols
    )
 
  
##################  
## end of table ##
################## 
)


