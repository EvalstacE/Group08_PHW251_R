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
output$mmwr_wk_label <- renderText({
    req(input$mmwr_slider)
    
    week_info <- cnty_week_pnts %>%
      filter(mmwr_week == input$mmwr_slider) %>%
      mutate(end_date = lubridate::as_date(end_date)) %>%
      distinct(mmwr_week, end_date) %>%
      arrange(end_date)
    
    if (nrow(week_info) == 0) return("")
    
    paste0(
      "Week Ending: ",
      format(week_info$end_date[1], "%B %d, %Y")
    )
})
  
##--basemap
  output$time_map <- renderLeaflet({
    sf_data <- ca_cnty_sf
    
    leaflet(sf_data) %>%
      addPolygons(
        layerId   = ~county,
        fillColor = "#f1f0ea",
        color     = "#b2b1ac",
        weight    = 1,
        fillOpacity = 1
      )
  })


##--updated map interacting with slider

  observe({
    data <- req(filteredData()) %>%
      filter(cumulative_infected > 0)
    
    leafletProxy("time_map") %>%
      # Only clear this specific group
      clearGroup("case_markers") %>%
      addCircleMarkers(
        data = data,
        # if you're using lng/lat cols:
        # lng = ~lng,
        # lat = ~lat,
        group = "case_markers",
        radius = ~sqrt(inf_rate_100k) *0.4,
        fillColor = "#0e2b44",
        fillOpacity = 0.9,
        stroke = FALSE,
        color = "white"
      )
  })
  




  
}
