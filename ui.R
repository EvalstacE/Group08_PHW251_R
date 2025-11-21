
ui <- fluidPage(
  
layout_column_wrap( 
  sliderInput(
    inputId = "mmwr_slider",
    label   = "Slide by Week:",
    min     = min(inf_cnty_time_data$mmwr_wk),
    max     = max(inf_cnty_time_data$mmwr_wk),
    value   = min(inf_cnty_time_data$mmwr_wk),
    step    = 1,
    width   = "100%",
    animate = animationOptions(interval = 1000, loop = TRUE)
  ), 
  
  textOutput("mmwr_wk_label"), 
  
  leafletOutput("time_map", height = "550px")
  
)

)