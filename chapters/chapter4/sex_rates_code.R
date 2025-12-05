

by_sex1 <- combined_df %>%
  #--only last week (52)
  filter(mmwr_week == 52) %>%
  
  #--one row per region–county–sex 
  group_by(health_officer_region, county, sex) %>%
  summarise(
    total_sex_pop       = first(total_sex_pop),
    cumulative_infected = sum(cumulative_infected, na.rm = TRUE),
    cumulative_severe   = sum(cumulative_severe, na.rm = TRUE),
    .groups = "drop"
  ) 

by_sex2 <- by_sex1 %>%  
  #--sum county pops and infectionswithin each region–sex
  group_by(health_officer_region, sex) %>%
  summarise(
    total_sex_pop       = sum(total_sex_pop, na.rm = TRUE),
    cumulative_infected = sum(cumulative_infected, na.rm = TRUE),
    cumulative_severe   = sum(cumulative_severe, na.rm = TRUE),
    inf_rate_100k       = round((10^5*cumulative_infected/total_sex_pop)),
    sev_rate_100k       = round((10^5*cumulative_severe/total_sex_pop)),
    .groups = "drop"
  )

by_sex1 <- combined_df %>%
  group_by(health_officer_region, county, sex) %>%
  filter(mmwr_week == 52) %>%
  ungroup() %>%
  group_by(health_officer_region, sex) %>%
    summarise(
      total_sex_pop       = first(total_sex_pop),
      cumulative_infected = sum(cumulative_infected, na.rm = TRUE),
      cumulative_severe   = sum(cumulative_severe, na.rm = TRUE),
      .groups = "drop"
    )
 







by_sex1 <- combined_df %>%
  group_by(health_officer_region, county, sex) %>%
  summarise(
    total_sex_pop       = first(total_sex_pop),
    cumulative_infected = max(cumulative_infected, na.rm = TRUE),
    cumulative_severe   = max(cumulative_severe, na.rm = TRUE),
    inf_rate_100k       = round((10^5*cumulative_infected/total_sex_pop)),
    sev_rate_100k       = round((10^5*cumulative_severe/total_sex_pop)),
    .groups = "drop"
  )  
  
  
  
  group_by(health_officer_region, sex) %>%
  summarise(
    total_pop           = sum(total_sex_pop),
    cumulative_infected = sum(cumulative_infected, na.rm = TRUE),
    cumulative_severe   = sum(cumulative_severe, na.rm = TRUE),
    inf_rate_100k       = round((10^5*cumulative_infected/total_pop)),
    sev_rate_100k       = round((10^5*cumulative_severe/total_pop)),
    .groups = "drop"
  ) %>%
  arrange(health_officer_region, sex)