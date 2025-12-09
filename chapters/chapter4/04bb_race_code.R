source("_common04.R")

cnty_rnks <- cnty_ranked_df %>%
  select(county, total_cnty_pop, priority_tier)

race_prop_inf <- all_rates_dem_adj %>%
  filter(geo_level == "county", 
         group_var == "race_short") %>%
  select(
    county, group_var_cat, group_pop, inf_rate_100k, 
    sev_rate_100k, adj_sev_100k, cumulative_severe,
    adj_sev_joint_rate
  ) %>%
  left_join(cnty_rnks, by = "county") %>%
  mutate(
    race_prop = 100*group_pop / total_cnty_pop,
    priority_tier = factor(priority_tier, 
                           levels = c("Third Priority", "Second Priority", "Top Priority"))
  ) 



hisp_df <- race_prop_inf %>% filter(group_var_cat == "Hispanic")

race_p1 <- 
  ggplot() + 
  facet_wrap(~group_var_cat, scales = "free", nrow = 1) +   
  geom_smooth(
    data = hisp_df,
    aes(
      x = race_prop,
      y = adj_sev_joint_rate,
      group = 1
    ),
    method = "lm",
    se = TRUE,                
    fill = "#f6c143",          
    alpha = 0.2,               
    color = drkst_clr,         
    linewidth = 0.9
  )+   
  
  geom_point(
    data = hisp_df, 
    shape = 21,
    color = drkst_clr,
    fill = "#52176b",
    alpha = 0.65,
    size = 3,
    aes(
      x = race_prop, 
      y = adj_sev_joint_rate,
    )
  ) +
  
  labs(
    x = "Proportion of County Population (%)",
    y = "Joint Age-Race Adjusted \nSevere Infection Rate"
  ) + 
  
  theme_classic() + 
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = grid_clr, linewidth = .5),
    axis.text = element_text(color = grid_clr, size = 7),
    strip.background = element_rect(fill = "#f1f0ea", color = grid_clr, linewidth = .5),
    strip.text = element_text(size = 16)
  )




others <- race_prop_inf %>% filter(group_var_cat != "Hispanic")

race_p2 <- 
  ggplot() + 
  facet_wrap(~group_var_cat, scales = "free", nrow = 2) + 
  
  geom_smooth(
    data = others,
    aes(
      x = race_prop,
      y = adj_sev_joint_rate,
      group = 1
    ),
    method = "lm",
    se = TRUE,                
    fill = "#f6c143",          
    alpha = 0.2,               
    color = drkst_clr,         
    linewidth = 0.9
  )+   
  
  geom_point(
    data = others, 
    shape = 21,
    color = drkst_clr,
    fill = "#cdcabd",
    alpha = 0.6,
    aes(
      x = race_prop, 
      y = adj_sev_joint_rate,
    )
  ) +
  
  labs(
    x = "Proportion of County Population (%)",
    y = "Joint Age-Race Adjusted \nSevere Infection Rate"
  ) + 
  
  theme_classic() + 
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = grid_clr, linewidth = .5),
    axis.text = element_text(color = grid_clr, size = 7),
    strip.background = element_rect(fill = "#f1f0ea", color = grid_clr, linewidth = .5)
  )






race_cat_lng <- all_rates_by_demographic %>%
  filter(geo_level == "statewide", 
         group_var == "race_short") %>%
  
  select(group_var_cat, cumulative_severe, cumulative_infected) %>%
  mutate(
    total_sev = sum(cumulative_severe),
    total_inf = sum(cumulative_infected)
  ) %>%
  group_by(group_var_cat) %>%
  mutate(
    sev_prop = 100*cumulative_severe / total_sev,
    inf_prop = 100*cumulative_infected / total_inf
  ) %>%
  ungroup() %>%
  arrange(desc(sev_prop)) %>%
  mutate(
    group_var_cat = fct_reorder(group_var_cat, sev_prop, .desc = TRUE)
  ) %>%
  select(1, 6:7) %>%
  pivot_longer(
    cols      = c(sev_prop, inf_prop),
    names_to  = "inf_type",
    values_to = "prop"
  ) %>%
  mutate(
    inf_type = recode(
      inf_type,
      "sev_prop" = "Severe Infections",
      "inf_prop" = "All Infections"
    )
  ) 


race_bar_p <- race_cat_lng %>%
  
  plotly::plot_ly(
    y    = ~inf_type,
    x    = ~prop,
    type = "bar",
    stroke = TRUE,
    color = ~group_var_cat,
    colors = cat_pal,
    hovertext  = ~paste0(
      group_var_cat, "<br>",
      inf_type, "<br>",
      "Percent: <b>", round(prop, 1), "%</b>"
    ),
    hoverinfo = "text",
    
    marker = list(
      size = 14,
      line = list(
        color = "#ffffff",  
        width = 2         
      )
    )
    
  ) %>%
  
  plotly::layout(
    barmode = "stack",  
    yaxis = list(
      title = "",
      tickfont = list(size = 16)
    ),
    xaxis = list(
      title = "",
      showgrid   = FALSE,  
      showticklabels = FALSE, 
      ticks      = ""   
    ),
    legend = list(
      title = list(text = "Race"),
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.1, 
      font = list(size = 16),
      itemsizing = "constant" 
    ),
    margin = list(b = 50, t = 50)
    
  )





race_df <- all_rates_by_demographic %>%
  filter(geo_level == "statewide", 
         group_var == "race_short") %>%
  
  select(group_var_cat, cumulative_severe, cumulative_infected) %>%
  mutate(
    total_sev = sum(cumulative_severe),
    total_inf = sum(cumulative_infected)
  ) %>%
  group_by(group_var_cat) %>%
  mutate(
    sev_prop = fmt_pct(100*cumulative_severe / total_sev),
    inf_prop = fmt_pct(100*cumulative_infected / total_inf)
  ) %>%
  ungroup() %>%
  arrange(desc(sev_prop)) %>%
  mutate(
    group_var_cat = fct_reorder(group_var_cat, sev_prop, .desc = TRUE)
  ) %>%
  select(1, 6:7) %>%
  rename(
    `Race` = group_var_cat ,
    `Infections`        = inf_prop,
    `Severe Infections` = sev_prop,
  )


race_tbl_1 <- kable(race_df, align = "c") %>%
  row_spec(0, bold = TRUE)  %>%
  kable_styling(bootstrap_options = c("hover"))





race_dist_df <- all_rates_by_demographic %>%
  filter(group_var == "race_short", geo_level == "county") %>%
  select(county, group_var, group_var_cat, cumulative_infected, cumulative_severe) %>%
  group_by(county) %>%
  mutate(
    total_sev = sum(cumulative_severe),
    total_inf = sum(cumulative_infected)
  ) %>% ungroup() %>%
  group_by(county, group_var_cat) %>%
  mutate(
    sev_prop = 100*cumulative_severe / total_sev,
    inf_prop = 100*cumulative_infected / total_inf
  ) %>% ungroup()

race_dist_sum <- race_dist_df %>%
  group_by(group_var_cat) %>%
  summarise(
    med_sev_raw = median(sev_prop),
    max_sev_raw = max(sev_prop),
    min_sev_raw = min(sev_prop),
    sd_sev_raw  = sd(sev_prop)
  ) %>%
  mutate(
    med_sev = fmt_pct(med_sev_raw),
    rng_sev = paste0(fmt_pct(min_sev_raw), " - ", fmt_pct(max_sev_raw)),
    sd_sev  = fmt_pct(sd_sev_raw)
  ) %>%
  select(group_var_cat, med_sev, rng_sev, sd_sev) %>%
  rename(
    `Race` = group_var_cat ,
    Median = med_sev,
    Range = rng_sev,
    SD = sd_sev
  )

race_tbl_2 <- kable(race_dist_sum, align = "c") %>%
  row_spec(0, bold = TRUE)  %>%
  kable_styling(bootstrap_options = c("hover"))



















