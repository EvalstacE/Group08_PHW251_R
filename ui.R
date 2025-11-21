
ui <- fluidPage(
  
layout_column_wrap( 
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
  
  textOutput("mmwr_wk_label"), 
  
  leafletOutput("time_map", height = "550px")
  
)

)