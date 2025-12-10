source("_common04.R")


age_props_all <- all_rates_dem_adj %>%
  filter(geo_level == "statewide", group_var == "age_cat") %>%
  select(county, group_var_cat, group_pop, total_ca_pop) %>%
  mutate(
    age_prop = 100*group_pop/total_ca_pop
  ) %>%
  select(group_var_cat, group_pop, age_prop)


age_cat_lng <- all_rates_by_demographic %>%
  filter(geo_level == "statewide", 
         group_var == "age_cat") %>%
  mutate(
    group_var_cat = factor(group_var_cat, levels = rev(c("0-17", "18-49", "50-64", "65+")))
  )%>%
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
  )%>%
  left_join(age_props_all, by = "group_var_cat")%>%
  mutate(lbl_bold = paste0("<b>", group_var_cat, "</b>"))



### encode factor order and color palette
age_levels <- age_cat_lng %>%
  distinct(group_var_cat) %>%
  pull(group_var_cat)

age_cat_lng <- age_cat_lng %>%
  mutate(group_var_cat = factor(group_var_cat, levels = age_levels))

age_pal_named <- setNames(cat_pal_sm[seq_along(age_levels)], age_levels)
###############





###### pie --

age_pie_df <- age_cat_lng %>%
  distinct(group_var_cat, group_pop, lbl_bold) %>%
  mutate(
    group_var_cat = factor(group_var_cat, levels = age_levels),
    prop = group_pop / sum(group_pop),
    display_lbl = if_else(prop >= 0.05, lbl_bold, "")
  )

age_prop_pie <- age_pie_df %>%
  plotly::plot_ly(
    labels    = ~group_var_cat,
    values    = ~group_pop,
    text      = ~display_lbl,
    hoverinfo = "label+percent"
  ) %>%
  add_pie(
    hole                  = 0.6,
    sort                  = FALSE,  # keep factor order
    textinfo              = "text+percent",
    textposition          = "outside",
    insidetextorientation = "radial",
    marker = list(
      colors = unname(age_pal_named[levels(age_pie_df$group_var_cat)])
    )
  ) %>%
  layout(
    title      = "",
    showlegend = FALSE,
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    
    # transparent backgrounds
    plot_bgcolor  = "rgba(0,0,0,0)",
    paper_bgcolor = "rgba(0,0,0,0)"
  )%>% 
  plotly::config(displayModeBar = FALSE)



###################
###################




age_bar_p <- age_cat_lng %>%
  mutate(group_var_cat = factor(group_var_cat, levels = age_levels)) %>%
  
  plotly::plot_ly(
    y    = ~inf_type,
    x    = ~prop,
    type = "bar",
    stroke = TRUE,
    color = ~group_var_cat,
    colors = cat_pal_sm,
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
      title = list(text = "Age Group"),
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.1, 
      font = list(size = 16),
      itemsizing = "constant" 
    ),
    margin = list(b = 50, t = 50)
    
    
  )



age_cat_df <- all_rates_by_demographic %>%
  filter(geo_level == "statewide", 
         group_var == "age_cat") %>%
  mutate(
    group_var_cat = factor(group_var_cat, levels = rev(c("0-17", "18-49", "50-64", "65+")))
  )%>%
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
  select(1, 6:7) %>%
  rename(
    `Age Group` = group_var_cat ,
    `Infections`        = inf_prop,
    `Severe Infections` = sev_prop,
  )


age_tbl_1 <- kable(age_cat_df, align = "c") %>%
  row_spec(0, bold = TRUE)  %>%
  kable_styling(bootstrap_options = c("hover"))






age_dist_df <- all_rates_by_demographic %>%
  filter(group_var == "age_cat", geo_level == "county") %>%
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

age_dist_sum <- age_dist_df %>%
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
    `Age Group` = group_var_cat ,
    Median = med_sev,
    Range = rng_sev,
    SD = sd_sev
  )

age_tbl_2 <- kable(age_dist_sum, align = "c") %>%
  row_spec(0, bold = TRUE)  %>%
  kable_styling(bootstrap_options = c("hover"))





















