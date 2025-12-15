source("_common04.R")


sex_pal1 <- c("#3cad82","#e56937")

sex_props_all <- all_rates_dem_adj_wsex %>%
  filter(geo_level == "statewide", group_var == "sex") %>%
  select(county, group_var_cat, group_pop, total_ca_pop) %>%
  mutate(
    sex_prop = 100*group_pop/total_ca_pop
  ) %>%
  select(group_var_cat, group_pop, sex_prop)


sex_cat_lng <- all_rates_dem_adj_wsex %>%
  filter(geo_level == "statewide", 
         group_var == "sex") %>%
  mutate(
    group_var_cat = factor(group_var_cat, levels = rev(c("FEMALE", "MALE")))
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
  left_join(sex_props_all, by = "group_var_cat")%>%
  mutate(lbl_bold = paste0("<b>", group_var_cat, "</b>"))


### encode factor order and color palette
sex_levels <- sex_cat_lng %>%
  distinct(group_var_cat) %>%
  pull(group_var_cat)

sex_cat_lng <- sex_cat_lng %>%
  mutate(group_var_cat = factor(group_var_cat, levels = sex_levels))

sex_pal_named <- setNames(sex_pal1[seq_along(sex_levels)], sex_levels)
###############




###### pie --

sex_pie_df <- sex_cat_lng %>%
  distinct(group_var_cat, group_pop, lbl_bold) %>%
  mutate(
    group_var_cat = factor(group_var_cat, levels = sex_levels),
    prop = group_pop / sum(group_pop),
    display_lbl = if_else(prop >= 0.05, lbl_bold, "")
  )

sex_prop_pie <- sex_pie_df %>%
  plotly::plot_ly(
    labels    = ~group_var_cat,
    values    = ~group_pop,
    text      = ~display_lbl,
    hoverinfo = "label+percent"
  ) %>%
  add_pie(
    sort                  = FALSE,  # keep factor order
    textinfo              = "text+percent",
    textposition          = "outside",
    insidetextorientation = "radial",
    marker = list(
      colors = unname(sex_pal_named[levels(sex_pie_df$group_var_cat)])
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
##--bar


sex_bar_p <- sex_cat_lng %>%
  mutate(group_var_cat = factor(group_var_cat, levels = sex_levels)) %>%
  
  plotly::plot_ly(
    y    = ~inf_type,
    x    = ~prop,
    type = "bar",
    stroke = TRUE,
    color = ~group_var_cat,
    colors = sex_pal1,
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
      title = list(text = "Sex"),
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.1, 
      font = list(size = 16),
      itemsizing = "constant" 
    ),
    margin = list(b = 50, t = 50)
    
    
  )


###################
###################
##--tables

sex_cat_df <- all_rates_dem_adj_wsex %>%
  filter(geo_level == "statewide", 
         group_var == "sex") %>%
  mutate(
    group_var_cat = factor(group_var_cat, levels = rev(c("FEMALE", "MALE")))
  )%>%
  select(group_var_cat, cumulative_severe, cumulative_infected, inf_rate_100k, sev_rate_100k) %>%
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
  select(1, 4:5, 8:9) %>%
  mutate(inf_rate_100k = scales::comma(inf_rate_100k)) %>%
  rename(
    `Sex` = group_var_cat ,
    `% Infections`        = inf_prop,
    `% Severe Infections` = sev_prop,
    `Infection Rate`    = inf_rate_100k,
    `Severe Rate`       = sev_rate_100k
  ) 


sex_tbl_1 <- kable(sex_cat_df, align = "c") %>%
  row_spec(0, bold = TRUE)  %>%
  kable_styling(bootstrap_options = c("hover"))




sex_dist_df <- all_rates_by_demographic %>%
  filter(group_var == "sex", geo_level == "county") %>%
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

sex_dist_sum <- sex_dist_df %>%
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
    `Sex` = group_var_cat ,
    Median = med_sev,
    Range = rng_sev,
    SD = sd_sev
  )

sex_tbl_2 <- kable(sex_dist_sum, align = "c") %>%
  row_spec(0, bold = TRUE)  %>%
  kable_styling(bootstrap_options = c("hover"))




###################
###################
##--plotlys


sex_pal <- c(
  "FEMALE" = "#3cad82",
  "MALE"   = "#e56937"
)

sex_cnty <- all_rates_dem_adj_wsex %>%
  filter(group_var == "sex", geo_level == "county") %>%
  group_by(county) %>%
  mutate(max_rate = max(adj_sev_100k, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    county        = forcats::fct_reorder(county, max_rate),
    group_var_cat = factor(group_var_cat, levels = c("FEMALE", "MALE")),
    sex_color     = sex_pal[group_var_cat] 
  )

cnty_levels <- levels(sex_cnty$county)

p <- sex_cnty %>%
  plot_ly() %>%
  
  add_trace(
    data  = sex_cnty,
    x     = ~adj_sev_100k,
    y     = ~county,
    type  = "scatter",
    mode  = "lines",
    split = ~county,
    line  = list(color = "rgba(150,150,150,0.6)", width = 1),
    hoverinfo  = "none",
    showlegend = FALSE
  ) %>%
  
  add_trace(
    data  = sex_cnty %>% filter(group_var_cat == "FEMALE"),
    x     = ~adj_sev_100k,
    y     = ~county,
    type  = "scatter",
    mode  = "markers",
    marker = list(
      size  = 9,
      color = sex_pal["FEMALE"]
    ),
    name = "Female",
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "Sex: Female<br>",
      "Adj severe rate: <b>%{x:.1f}</b> per 100k",
      "<extra></extra>"
    )
  ) %>%
  
  add_trace(
    data  = sex_cnty %>% filter(group_var_cat == "MALE"),
    x     = ~adj_sev_100k,
    y     = ~county,
    type  = "scatter",
    mode  = "markers",
    marker = list(
      size  = 9,
      color = sex_pal["MALE"]
    ),
    name = "Male",
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "Sex: Male<br>",
      "Adj severe rate: <b>%{x:.1f}</b> per 100k",
      "<extra></extra>"
    )
  ) %>%
  
  layout(
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = 1.1
    ),
    xaxis = list(
      title = "Age-adjusted severe <br>infection rate (per 100,000)",
      zeroline = FALSE
    ),
    yaxis = list(
      title         = "",
      showgrid      = FALSE,
      showticklabels = TRUE,
      zeroline      = FALSE,
      categoryorder = "array",
      categoryarray = cnty_levels
    )
  )

sex_cnty_p <-  p %>% layout(height = 900)






sex_summary_hc <- sex_cnty %>%
  group_by(group_var_cat) %>%
  summarise(
    mean_adj = mean(adj_sev_100k),
    se_hc = {
      m <- lm(adj_sev_100k ~ 1)
      sqrt(vcovHC(m, type = "HC3"))[1,1]
    },
    lower_95 = mean_adj - 1.96 * se_hc,
    upper_95 = mean_adj + 1.96 * se_hc,
    .groups = "drop"
  )



sex_stat_p <- 
ggplot(sex_summary_hc, aes(x = group_var_cat, y = mean_adj)) +
  
  geom_errorbar(
    aes(ymin = lower_95, ymax = upper_95, color = group_var_cat),
    width = 0.15,
    linewidth = 0.8
  ) +
  
  geom_point(size = 4, aes(color = group_var_cat)) +
  scale_color_manual(values = c("#3cad82", "#e56937"), guide = "none") + 

  labs(
    x = "",
    y = "Mean County Age-Adjusted \nSevere Infection Rate (per 100,000)",
    title = "",
    subtitle = "Mean ± 95% robust (HC3) confidence intervals \nRates per 100k"
  ) +
  ylim(200,500)+
  theme_minimal(base_size = 12) + 
  theme(
    axis.title.y = element_text(margin = margin(r=10), size = 10),
    axis.text.x = element_text(margin = margin(r=10), size = 14),
    plot.subtitle = element_text(margin = margin(b=10), size = 8)
  )





sex_summary_hc_plot <- sex_summary_hc %>%
  mutate(
    x_pos = ifelse(group_var_cat == "FEMALE", 2, 3),
    hover_txt = paste0(
      "Sex: <b>", group_var_cat, "</b><br>",
      "Mean adj severe rate: <b>", round(mean_adj, 1), "</b><br>",
      "95% CI: <b>", round(lower_95, 1), "</b> to <b>", round(upper_95, 1), "</b>"
    )
  )

sex_stat_plotly <- sex_summary_hc_plot %>%
  plot_ly(
    x    = ~x_pos,
    y    = ~mean_adj,
    type = "scatter",
    mode = "markers",
    error_y = list(
      type       = "data",
      symmetric  = FALSE,
      array      = ~upper_95 - mean_adj,
      arrayminus = ~mean_adj - lower_95,
      thickness  = 1.5,
      width      = 4
    ),
    color  = ~group_var_cat,
    colors = c("FEMALE" = "#3cad82", "MALE" = "#e56937"),
    marker = list(size = 10),
    text   = ~hover_txt,
    hoverinfo = "text",
    showlegend = FALSE
  ) %>%
  layout(
    title = list(
      text = ""
    ),
    xaxis = list(
      title = "",
      tickvals = c(2, 3),
      ticktext = c("FEMALE", "MALE"),
      range = c(1.5, 3.5),   # <-- squeezes inward
      tickfont = list(size = 14)
    ),
    yaxis = list(
      title = "Mean County Age-Adjusted<br>Severe Infection Rate (per 100,000)",
      range = c(200, 500),
      tickvals = c(250, 350, 450),
      titlefont = list(size = 12)
    ),
    margin = list(l = 60, r = 20, t = 60, b = 40)
  )






