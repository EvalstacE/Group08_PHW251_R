
###- '_common04.R' runs first (brings in all functions)
#### -- then sources this file 


##-- Bring in shapefiles

cnty_week_pnts <- read.csv(file = here("data/weekly_case_points/cnty_week_pnts.csv"))
hor_week_pnts <- read.csv(file = here("data/weekly_case_points/hor_week_pnts.csv"))


geoms <- bring_in_sfs()
ca_state_sf <- geoms$ca_state_sf

ca_cnty_sf <- geoms$ca_cnty_sf 

ca_cnty_pnts <- read.csv(file = here("data/weekly_case_points/ca_cnty_pnts.csv")) 


hor_sf <- geoms$hor_sf 

hor_pnts <- read.csv(file = here("data/weekly_case_points/hor_pnts.csv"))







##-- color palettes for maps

lgt_clr <- "#fcebed"
drk_clr <- "#1e0c47"
drkst_clr <- "#0f172a"
grid_clr <- "#212738"


custom_pal <- c(
  "#fcebed",
  "#fdacb8",
  "#b93f76",
  "#52176b",
  "#1e0c47"
)


rnk_pal <- c(
  "#52176b",
  "#854d88",
  "#f1f0ea"
)



m <- list(
  l = 50,
  r = 50,
  b = 50,
  t = 50,
  pad = 20
)