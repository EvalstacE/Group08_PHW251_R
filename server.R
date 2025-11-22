server <- function(input, output) {
  

##--reactive data based on slider input
  filteredData <- reactive({
    req(cnty_week_pnts, input$mmwr_slider)
    cnty_week_pnts %>%
      filter(
        mmwr_week == input$mmwr_slider,
        cumulative_infected > 0
      )
  }) 
  
##--convert mmwr weeks to labels
output$mmwr_wk_label <- renderUI({
    req(input$mmwr_slider)
    
    week_info <- cnty_week_pnts %>%
      filter(mmwr_week == input$mmwr_slider) %>%
      mutate(end_date = lubridate::as_date(end_date)) %>%
      distinct(mmwr_week, end_date) %>%
      arrange(end_date)
    
    if (nrow(week_info) == 0) return(NULL)
    
    formatted_date <- format(week_info$end_date[1], "%B %d, %Y")
    
    HTML(
      glue::glue(
        "<div style='text-align:center; font-size:2rem;'>
         Week Ending: <strong>{formatted_date}</strong>
       </div>"
      )
    )
})

##################################################
##################################################
#    Weekly Infection Rates by County / Week     #
##################################################
##################################################
##--basemap
output$cnty_case_rate_map <- renderLeaflet({
  make_cnty_basemap(
    sf_all = ca_cnty_sf,
    sf_top = top_cnty_rates
  )
})


##--updated map interacting with slider

  observe({
    data <- req(filteredData()) %>%
      filter(cumulative_infected > 0) %>%
      mutate(
        alpha_val = case_when(
          inf_rate_breaks %in% levels(inf_rate_breaks)[1:2] ~ 0.35,
          inf_rate_breaks %in% levels(inf_rate_breaks)[(nlevels(inf_rate_breaks)-1):nlevels(inf_rate_breaks)] ~ 1,
          TRUE ~ 0.55
        )
      )
    
    leafletProxy("cnty_case_rate_map") %>%
      clearGroup("case_markers") %>%
      addCircleMarkers(
        data = data,
        lng = ~lng,
        lat = ~lat,
        group = "case_markers",
        radius = ~sqrt(inf_rate_100k) *0.3,
        fillColor = ~custom_pal_cases(inf_rate_breaks),
        fillOpacity = ~alpha_val,
        stroke = TRUE,
        weight = 1, 
        color = ~custom_pal_cases(inf_rate_breaks),
        
        label = ~lapply({
          
          border_col <- custom_pal_cases(inf_rate_breaks)
          
          glue::glue(
            "<div style='
         background-color:white;
         border: 3px solid {border_col};
         padding:6px 10px;
         border-radius:6px;
         font-size:12px;
         color:black;
       '>
           <b>{county} County:</b><br/>
           Total infections: <b>{comma(cumulative_infected)}</b><br/>
           Infection rate: <b>{comma(round(inf_rate_100k, 1))}</b> per 100k<br/>
           Severe infection rate: <b>{comma(round(sev_rate_100k, 1))}</b> per 100k
     </div>"
          )
        }, htmltools::HTML), 
        
        labelOptions = labelOptions(
          direction = "auto",
          opacity = 1,
          textsize = "12px",
          offset = c(0, -1),
          style = list(
            "background-color" = "white",         
            "border" = "1px solid transparent",   
            "padding" = "0px",                   
            "border-radius" = "6px"
          )
        )
    )
    
})
  


output$cnty_case_rate_legend <- renderUI({
  data <- req(cnty_week_pnts)
  map_legend_ui(
    break_factor = data$inf_rate_breaks,
    pal_fun      = custom_pal_cases,
    title        = "Infection Rate per 100k Population"
  )
})







#########################################################
#########################################################
#    Weekly SEVERE Infection Rates by County / Week     #
#########################################################
#########################################################
##--basemap
output$cnty_sev_rate_map <- renderLeaflet({
  make_cnty_basemap(
    sf_all = ca_cnty_sf,
    sf_top = top_cnty_rates
  )
})


##--updated map interacting with slider

observe({
  
  data <- req(filteredData()) %>%
    filter(cumulative_infected > 0) %>%
    mutate(
      alpha_val = case_when(
        sev_rate_breaks %in% levels(sev_rate_breaks)[1:2] ~ 0.35,
        sev_rate_breaks %in% levels(sev_rate_breaks)[(nlevels(sev_rate_breaks)-1):nlevels(sev_rate_breaks)] ~ 1,
        TRUE ~ 0.55
      )
    )
  
  leafletProxy("cnty_sev_rate_map") %>%
    clearGroup("case_markers") %>%
    addCircleMarkers(
      data = data,
      lng = ~lng,
      lat = ~lat,
      group = "case_markers",
      radius = ~(sqrt(sev_rate_100k * sev_scale) * 0.3),
      fillColor = ~custom_pal_sev_cases(sev_rate_breaks),
      fillOpacity = ~alpha_val,
      stroke = TRUE,
      weight = 1, 
      color = ~custom_pal_sev_cases(sev_rate_breaks),
      
      label = ~lapply({
      
        border_col <- custom_pal_sev_cases(sev_rate_breaks)
        
        glue::glue(
            "<div style='
         background-color:white;
         border: 3px solid {border_col};
         padding:6px 10px;
         border-radius:6px;
         font-size:12px;
         color:black;
       '>
           <b>{county} County:</b><br/>
           Total infections: <b>{comma(cumulative_infected)}</b><br/>
           Infection rate: <b>{comma(round(inf_rate_100k, 1))}</b> per 100k<br/>
           Severe infection rate: <b>{comma(round(sev_rate_100k, 1))}</b> per 100k
     </div>"
        )
      }, htmltools::HTML), 
      
      labelOptions = labelOptions(
        direction = "auto",
        opacity = 1,
        textsize = "12px",
        offset = c(0, -1),
        style = list(
          "background-color" = "white",         
          "border" = "1px solid transparent",   
          "padding" = "0px",                   
          "border-radius" = "6px"
        )
      )
  )
  
})



output$cnty_sev_rate_legend <- renderUI({
  data <- req(cnty_week_pnts)
  
  # pull labels from factor levels, colors from the same palette
  map_legend_ui(
    break_factor = data$sev_rate_breaks,
    pal_fun      = custom_pal_sev_cases,
    title        = "Severe Infection Rate per 100k Population"
  )
})

##############
# End Server #
############## 
}
