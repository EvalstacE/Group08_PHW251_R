
source("_common04.R")
source("_common04_maps.R")




##--cumulative cases map
cum_cases_map <- make_cnty_basemap(sf_all = cnty_sf) %>%
  
  addPolygons(
    data        = top_95cum,
    fillColor   = "#f6c143",
    color       = drkst_clr,
    weight      = 1,
    fillOpacity = 0.6,
    opacity = 1
  ) %>%
  
    addCircleMarkers(
      data        = cum_cases_pnts,
      radius      = ~ifelse(
        cumulative_infected > 0,
        3 + 20 * cumulative_infected / max_cases,  
        0
      ),
      stroke      = TRUE,
      weight      = 1,
      color       = drkst_clr,
      fillColor   = "#52176b",
      fillOpacity = 1,
      label       = ~paste0(
        "<strong>", county, "</strong><br>",
        "Cumulative Cases: <strong>", scales::comma(cumulative_infected), "</strong>"
      ) %>% lapply(htmltools::HTML)
    ) %>%

  addLegend(
    position = "bottomleft",
    colors   = "#f6c143",
    labels   = "≥ 95th Percentile Cumulative Infections",
    opacity  = 0.6,
    title    = "County Priority Highlight"
  )


#############
#############


cnty_ranked_df <- cnty_ranked_df %>%
  mutate(
    county_label = case_when(
      priority_tier == "Top Priority" ~ 
        glue("<span style='font-weight:bold; font-size:12px; color:#52176b'>{county}</span>"),
      
      priority_tier == "Second Priority" ~ 
        glue("<span style='font-weight:bold; font-size:12px; color:#854d88'>{county}</span>"),
      
      TRUE  ~ 
        glue("<span style='font-size:12px; color:#555555'>{county}</span>")),
    
    priority_tier = factor(
      priority_tier,
      levels = c("Top Priority", "Second Priority", "Third Priority")),
    pop_prop = 100 * pop_prop,
    
    county = forcats::fct_reorder(county, adj_sev_100k)
  ) 


cnty_rank_sf <- cnty_sf %>%
  select(county) %>%
  left_join(cnty_ranked_df, by = "county") %>%
  
  mutate(
    priority_tier = factor(
      priority_tier,
      levels = c("Top Priority", "Second Priority", "Third Priority")
    ), 
    
    hover_lbl = glue(
      "<span style='font-size:16px;><strong>{county}</strong><br>
       Severe AAR: <strong>{scales::comma(round(adj_sev_100k, 1))}</strong></span>"
    ) %>% as.character()
  )


cnty_top_sf <- cnty_rank_sf %>% filter(priority_tier == "Top Priority")


cnty_dorl <- cnty_rank_sf %>%
  st_transform(3310) %>%
  mutate(dorl_wt = 0.8) %>% 
  cartogram_dorling(
    weight  = "dorl_wt",
    k       = 0.2,  
    itermax = 200
  )

cnty_dorl_centroids <- cnty_dorl %>%
  st_centroid()

cnty_dorl_centroids <- cnty_dorl_centroids %>%
  st_transform(4326) %>%
  mutate(
    lng = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2],
    radius = scales::rescale(adj_sev_100k, to = c(2, 23))
  )

# palette
pal_priority <- colorFactor(
  palette = c(
    "Top Priority"            =  "#1e0c47",
    "Second Priority"         =  "#854d88",
    "Third/Fourth Priority"   =  "#f1f0ea"
  ),
  domain  = cnty_dorl_centroids$priority_tier
)




cnty_rank_map <- make_cnty_basemap(sf_all = cnty_sf) %>%
  
      addPolygons(
        data        = cnty_top_sf ,
        fillColor   = "#f6c143",
        color       = drkst_clr,
        weight      = 1,
        fillOpacity = 0.6,
        opacity = 1
      ) %>%

      addCircleMarkers(
        data        = cnty_dorl_centroids,
        radius      = ~radius,         
        stroke      = TRUE,
        weight      = 1.2,
        color       = drk_clr,
        fillColor   = ~pal_priority(priority_tier),
        fillOpacity = 1,
        label       = ~paste0(
          "<strong>", county, "</strong><br>",
          "Severe AAR: <strong>", scales::comma(round(adj_sev_100k,1)), "</strong>"
        ) %>% lapply(htmltools::HTML)
      )  %>%
  
  addLegend(
    position = "bottomleft",
    colors   = "#f6c143",
    labels   = "≥ 90th Percentile Infection Rates",
    opacity  = 0.6,
    title    = "County Priority Highlight"
  )

  
  
