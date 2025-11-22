
ui <- fluidPage(
  

layout_column_wrap( 
  style = "margin-top: 3rem !important;",
  width = 1,
  heights_equal = "row",
  sliderInput(
    inputId = "mmwr_slider",
    label   = "Slide by Week:",
    min     = min(cnty_week_pnts$mmwr_week),
    max     = max(cnty_week_pnts$mmwr_week),
    value   = min(cnty_week_pnts$mmwr_week),
    step    = 1,
    width   = "100%",
    animate = animationOptions(interval = 1000, loop = TRUE)
  ), 
  
  uiOutput("mmwr_wk_label"), 
  
  layout_column_wrap(
    style = "margin: 3rem !important;",
    width = 1/2, 
    
    card(
      uiOutput("cnty_case_rate_legend"),
      leafletOutput("cnty_case_rate_map", height = "750px")
    ), 
    
    card(
      uiOutput("cnty_sev_rate_legend"),
      leafletOutput("cnty_sev_rate_map", height = "750px")
    ) 
    
  )
  

  
)

)