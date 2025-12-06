
ui <- fluidPage(
  
theme = bs_theme(
    version = 5, 
    bootswatch = "cosmo"
),



layout_column_wrap(width = 1/2,
                   
   layout_column_wrap(width = 1,
      heights_equal = "row",
      
      actionButton("reset_cnty", "Clear Selection", width = "200px"),
      leafletOutput("cnty_map")
   ), 
   
   layout_column_wrap(width = 1,
      heights_equal = "row",
      
      actionButton("reset_hor", "Clear Selection", width = "200px"),
      leafletOutput("hor_map")
   )
                   
),

  
     

layout_sidebar(
  sidebar = 
    
    tagList(
        radioButtons(
          inputId = "region_selector", 
          label   = "Select Region",
          choices = unique(dem_df$geo_level)
        ), 
      
        selectInput(
          inputId = "dem_selector", 
          label   = "Select Demographic",
          choices = unique(dem_df$group_var),
          multiple = FALSE
        )
    ), 
  
  DT::DTOutput("dem_table")
  
)
    
    

    

    
    




##############
#   End UI   #
##############
)