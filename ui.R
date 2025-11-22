
ui <- fluidPage(
  
theme = bs_theme(
    version = 5, 
    bootswatch = "cosmo"
),
  
card(full_screen = TRUE, 
     
layout_column_wrap( 
    style = "margin: 0rem !important; margin-top: 1rem !important;",
    width = 1,
    heights_equal = "row",
    
    uiOutput("week_slider"),
    
    uiOutput("mmwr_wk_label"), 
    
    uiOutput("two_map_ui")
  )

)

##############
#   End UI   #
##############
)