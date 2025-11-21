# === Bring in shapefiles === #

bring_in_sfs <- 
  
  function(folder = "data/shapefiles") {
    shapefiles <- c(
      ca_cnty_pnts = "ca_cnty_pnts",
      ca_cnty_sf = "ca_cnty_sf",
      hor_pnts = "hor_pnts",
      hor_sf = "hor_sf"
    )
    
    purrr::imap(shapefiles, function(file_name, obj_name) {
      read_sf(here(folder, file_name)) %>%
        st_transform(crs = 4326)
    })
  }