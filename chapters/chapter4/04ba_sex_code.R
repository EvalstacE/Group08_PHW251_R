
by_sex_cnty <- combined_df %>% 
#--only last week (52)
filter(mmwr_week == 52) %>%
#--one row per region–county–sex 
group_by(health_officer_region, county, sex) %>%
summarise(
  total_sex_pop       = first(total_sex_pop),
  total_cnty_pop      = first(total_cnty_pop),
  cumulative_infected = sum(cumulative_infected, na.rm = TRUE),
  cumulative_severe   = sum(cumulative_severe, na.rm = TRUE),
  .groups = "drop"
) %>%
  mutate(
    sex_prop            = 100*total_sex_pop/total_cnty_pop,
    inf_rate_100k       = round((10^5*cumulative_infected/total_sex_pop)),
    sev_rate_100k       = round((10^5*cumulative_severe/total_sex_pop)),
  ) 
#################################
######### statewide rates
by_sex_age_state <- combined_df %>% 
  #--only last week (52)
  filter(mmwr_week == 52) %>%
  #--one row per region–county–sex 
  group_by(mmwr_week, sex, age_cat) %>%
  summarise(
    sex_age_pop         = sum(pop),
    total_ca_pop        = first(total_ca_pop),
    cumulative_infected = sum(cumulative_infected, na.rm = TRUE),
    cumulative_severe   = sum(cumulative_severe, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(sex) %>%
  mutate(total_sex_pop = sum(sex_age_pop)) %>% 
  ungroup() %>%
  mutate(
    age_sex_prop        = sex_age_pop/total_sex_pop,
    inf_rate_100k       = round((10^5*cumulative_infected/sex_age_pop)),
    sev_rate_100k       = round((10^5*cumulative_severe/sex_age_pop))
  )


statewide_crude_by_sex <- by_sex_age_state %>%
  group_by(sex) %>%
  summarise(
    total_severe = sum(cumulative_severe),
    total_sex_pop = first(total_sex_pop),
    crude_sev_rate_100k = 1e5 * total_severe / total_sex_pop
  )


std_age_weights_sex <- by_sex_age_state %>%
  group_by(sex, age_cat) %>%
  summarise(
    w = sex_age_pop / total_sex_pop,
    .groups = "drop"
  )


by_sex_age_cnty <- combined_df %>% 
  #--only last week (52)
  filter(mmwr_week == 52) %>%
  #--one row per region–county–sex 
  group_by(health_officer_region, county, age_cat, sex) %>%
  summarise(
    sex_age_pop         = sum(pop),
    total_cnty_pop      = first(total_cnty_pop),
    cumulative_infected = sum(cumulative_infected, na.rm = TRUE),
    cumulative_severe   = sum(cumulative_severe, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    inf_rate       = cumulative_infected/sex_age_pop,
    sev_rate       = cumulative_severe/sex_age_pop
  ) 

cnty_sex_age_adj <- by_sex_age_cnty %>%
  left_join(std_age_weights_sex, by = c("sex", "age_cat")) %>%
  group_by(health_officer_region, county, sex) %>%
  summarise(
    adj_rate_100k = 1e5 * sum(inf_rate * w, na.rm = TRUE),
    adj_sev_100k = 1e5 * sum(sev_rate * w, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(county) %>%

  ungroup() %>%
  mutate(group_var = "sex", geo_level = "county") %>%
  rename(group_var_cat = sex)


#mutate(
  #female_higher_sev = adj_sev_100k[sex == "FEMALE"] >
    #adj_sev_100k[sex == "MALE"]
#) %>%
#summary(cnty_sex_age_adj$female_higher_sev)
