server <- function(input, output) {
  

##--reactive data based on slider input
filteredData <- reactive({
  req(cnty_centroids)
  req(input$mmwr_slider)
  
  cnty_centroids %>%
    left_join(
      weekly_df %>%
        filter(
          mmwr_week == input$mmwr_slider,
          cumulative_infected > 0
        ),
      by = "county"
    ) %>%
    filter(!is.na(cumulative_infected)) 
})
  



##################
##  Week Slider ##
##################

output$week_slider <- renderUI({
  tagList(
    tags$style(type = "text/css", "
      #mmwr_slider .control-label { font-size: 1rem; font-weight: 600; }
      #mmwr_slider .irs-grid-text { font-size: 0.75rem; margin-bottom: 0rem !important; padding-bottom: 0rem !important;}
    "),
    div(
      id = "mmwr_slider",
      sliderInput(
        inputId = "mmwr_slider",
        label   = "Click and drag the slider to view by week:",
        min     = min(weekly_df$mmwr_week),
        max     = max(weekly_df$mmwr_week),
        value   = max(weekly_df$mmwr_week),
        step    = 1,
        width   = "100%",
        animate = animationOptions(interval = 1000, loop = TRUE)
      )
    )
  )
})


#########################
##  Week Slider Labels ##
#########################
output$mmwr_wk_label <- renderUI({
  req(input$mmwr_slider)
  
  week_info <- weekly_df %>%
    filter(mmwr_week == input$mmwr_slider) %>%
    mutate(end_date = lubridate::as_date(end_date)) %>%
    distinct(mmwr_week, end_date) %>%
    arrange(end_date)
  
  if (nrow(week_info) == 0) return(NULL)
  
  formatted_date <- format(week_info$end_date[1], "%B %d, %Y")
  
  HTML(
    glue::glue(
      "<div style='text-align:center; font-size:1.2rem;margin-bottom: 0.75rem !important;margin-top: 0rem !important; padding: 0rem !important;'>
         Week Ending: <strong>{formatted_date}</strong>
       </div>"
    )
  )
})


##################################################
##################################################
#           Weekly Infections by County          #
##################################################
##################################################
##--basemap
output$cnty_case_map <- renderLeaflet({
  make_cnty_basemap(
    sf_all = cnty_sf
  )
})


##--updated map interacting with slider

observe({
  data <- req(filteredData())
  
  leafletProxy("cnty_case_map") %>%
    clearGroup("case_markers") %>%
    addCircleMarkers(
      data        = data,
      radius      = ~ifelse(
        cumulative_infected > 0,
        3 + 20 * cumulative_infected / max_cases,  
        0
      ),
      stroke      = TRUE,
      weight      = 1,
      color       = drk_clr,
      fillColor   = "#52176b",
      fillOpacity = 0.9,
      group       = "case_markers",
      label       = ~paste0(
        "<strong>", county, "</strong><br>",
        "Cumulative Cases: <strong>", scales::comma(cumulative_infected), "</strong>"
      ) %>% lapply(htmltools::HTML)
    )
})
  


output$cum_cases_plot <- plotly::renderPlotly({
  req(p1_df, input$mmwr_slider)
  
  this_week <- input$mmwr_slider
  
  # Option: only show weeks up to the selected week
  df_plot <- p1_df %>%
    filter(mmwr_week <= this_week) %>%
    mutate(is_selected = mmwr_week == this_week)
  
  # Base line
  p <- plotly::plot_ly(
    data = df_plot,
    name = "Cumulative Infections",
    x    = ~mmwr_week,
    y    = ~cumulative_infected,
    type = "scatter",
    mode = "lines+markers",
    line = list(color = drkst_clr, width = 0.5),
    marker = list(color = "#52176b", size = 6), 
    hoverinfo = "text",
    text      = ~paste0(
      "<b>", s_dt, " - ", e_dt, "</b>",
      "<br>Cumulative infected: <b>", cum_lbl, "</b>",
      "<br>Percent change: <b>", round(p_chng, 1), "%</b>"
    ),
    showlegend = TRUE
  )
  
  p  %>%
    layout(
      title = list(text = ""),
      xaxis = list(title = "MMWR Week"),
      yaxis = list(title = "")
    )
  
})


##############
# End Server #
############## 
}
