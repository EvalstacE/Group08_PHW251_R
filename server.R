server <- function(input, output) {
  

#-- reactives to store selections 
  
  selected_county <- reactiveVal(NULL)
  selected_region <- reactiveVal(NULL)  
  
  
##--reactive data based on geography selection

filt_dem_df <- reactive({
  req(dem_df, input$region_selector, input$dem_selector)
    
    # capture current map selections so this reactive depends on them
    sc <- selected_county()
    sr <- selected_region()
    
    df <- dem_df %>%
      dplyr::filter(
        geo_level == input$region_selector,
        group_var == input$dem_selector
      )
    
    # apply map-based filters if a selection exists
    if (input$region_selector == "county" && !is.null(sc)) {
      df <- df %>% dplyr::filter(county == sc)
    }
    
    if (input$region_selector == "region" && !is.null(sr)) {
      df <- df %>% dplyr::filter(health_officer_region == sr)
    }
    
    df
}) 
  

##--reactive table output
output$dem_table <- DT::renderDT({
  req(filt_dem_df())
  
  df <- filt_dem_df() %>%
    dplyr::select(-c(
      total_group_var_pop,
      total_ca_pop,
      geo_level,
      cumulative_severe,
      cumulative_infected
    ))
  
  if (input$region_selector == "region") {
    df <- df %>% dplyr::select(-county)
  } else {
    df <- df %>% dplyr::select(-health_officer_region)
  }
  
  df %>%
    DT::datatable()
})



##--maps

# map outputs
output$cnty_map <- renderLeaflet({
  make_cnty_basemap(cnty_sf, cnty_pnts)
})

output$hor_map <- renderLeaflet({
  make_hor_basemap(cnty_sf, hor_sf)
})






# observers

# layerId = ~county
observeEvent(input$cnty_map_shape_click, {
  click <- input$cnty_map_shape_click
  req(click$id)  
  
  selected_county(click$id)
  
  # optional: visually highlight selected county
  leafletProxy("cnty_map") %>%
    clearGroup("selected") %>%
    addPolygons(
      data        = cnty_sf %>% dplyr::filter(county == click$id),
      fillColor   = "#fbd113",
      fillOpacity = 0.2, 
      color       = "#002e6d",
      weight      = 2,
      group       = "selected"
    )
})


# layerId = ~health_officer_region
observeEvent(input$hor_map_shape_click, {
  click <- input$hor_map_shape_click
  req(click$id)  
  
  selected_region(click$id)
  
  leafletProxy("hor_map") %>%
    clearGroup("selected") %>%
    addPolygons(
      data        = hor_sf %>% dplyr::filter(health_officer_region == click$id),
      fillColor   = "#fbd113",
      fillOpacity = 0.2, 
      color       = "#002e6d",
      weight      = 2,
      group       = "selected"
    )
})


##--clears selections
observeEvent(input$reset_hor, {
  selected_region(NULL)
  
  leafletProxy("hor_map") %>%
    clearGroup("selected")
})


observeEvent(input$reset_cnty, {
  selected_county(NULL)
  
  leafletProxy("cnty_map") %>%
    clearGroup("selected")
})



observeEvent(input$region_selector, {
  if (input$region_selector == "region") {
    selected_county(NULL)
    leaflet::leafletProxy("cnty_map") %>% clearGroup("selected")
  } else if (input$region_selector == "county") {
    selected_region(NULL)
    leaflet::leafletProxy("hor_map") %>% clearGroup("selected")
  }
})




##############
# End Server #
############## 
}
