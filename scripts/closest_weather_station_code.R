library(waterNETN)
library(tidyverse)
library(sf)
library(rgeoboundaries)
library(tmap)
# devtools::install_github("ropensci/rnoaa")
library(rnoaa)

importData()
# find nearest noaa weather station
# rnoaa was removed from CRAN last month. Install from github
#devtools::install_github("ropensci/rnoaa") # only use to find weather stations
stns <- ghcnd_stations()
lat_lon_df <- getSites(active = F) |> select(id = SiteCode, latitude = SiteLatitude, longitude = SiteLongitude)
nearby_stations <- meteo_nearby_stations(lat_lon_df = lat_lon_df, station_data = stns, radius = 50)

# closest_station <-
closest_WS1 <- map_dfr(seq_along(nearby_stations),
                 function(x){
                   site = names(nearby_stations)[x]
                   dat1 <- data.frame(site,
                                      nearby_stations[[x]][, 'id'],
                                      nearby_stations[[x]][, 'latitude'],
                                      nearby_stations[[x]][, 'longitude'],
                                      nearby_stations[[x]][, "distance"])
                 return(dat1)
                 })
# MORR USC00281592 por 2000 - 2013
# MORR US1NJMS0003 was close but small por
colnames(closest_WS1) <- c("SiteCode", "id", "ws_lat", "ws_long", "ws_dist_km")
closest_WS1$UnitCode <- ifelse(grepl("^AC", closest_WS1$SiteCode), "ACAD",
                               substr(closest_WS1$SiteCode, 1, 4))

# Revise manually to include nearest WS with most complete record from 2006-2024, including
# data from 2024. Used scacis.rcc-acis.org to find best ws per park
best_ws <- data.frame(UnitCode = c("ACAD", "MABI", "MIMA", "MORR", "ROVA",
                                   "SAGA", "SAIR", "SARA", "WEFA"),
                      id = c("USR0000MMCF", "USC00439984", "USW00014702", "USC00281335", "USW00064756",
                             "USC00435768", "USC00196783", "USC00301068", "USW00054734"))
closest_WS <- left_join(best_ws, closest_WS1, by = c("UnitCode", "id"))

# Find the county for each water monitoring location, and add to closest_WS

# data("closest_WS")
# head(closest_WS)

us_states <- geoboundaries("USA", "adm1")
us_county <- geoboundaries("USA", "adm2")
#st_crs(us_county)
ws_sf <- st_as_sf(closest_WS, coords = c("ws_long", "ws_lat"), crs = 4326)
st_crs(us_county) == st_crs(ws_sf) # T
sites <- getSites(active = F) |> st_as_sf(coords = c("SiteLongitude", "SiteLatitude"), crs = 4269)
sites84 <- st_transform(sites, crs = 4326)

# Look at data to check overlap
tm_shape(us_county, bbox = ws_sf) + tm_borders("grey") +
  tm_shape(us_states, bbox = ws_sf) + tm_borders("black") +
  tm_shape(sites) + tm_dots("blue", size = 0.4) +
  tm_shape(ws_sf) + tm_dots("red", size = 0.5)

# relate weather station to state and county
ws_cty <- st_join(ws_sf, us_county)
ws_cty_final <- left_join(closest_WS,
                          ws_cty |> select(SiteCode, WStnCounty = shapeName),
                          by = "SiteCode") |>
                as.data.frame() |> select(-geometry)
head(ws_cty_final)
table(ws_cty_final$UnitCode, ws_cty_final$WStnCounty)

ws_state <- st_join(ws_sf, us_states)
ws_comb <- left_join(ws_cty_final, ws_state |> select(SiteCode, WStnState = shapeName), by = "SiteCode") |>
  select(-geometry)
table(ws_comb$UnitCode, ws_comb$WStnState)

# relate site to state and county
site_state <- st_join(sites84, us_states) |> select(GroupCode:site_type, ParkState = shapeName) |>
  as.data.frame() |> select(-geometry)
site_cty <- st_join(sites84, us_county) |> select(GroupCode:site_type, ParkCounty = shapeName) |>
  as.data.frame() |> select(-geometry)

site_comb <- left_join(site_cty, site_state |> select(SiteCode, ParkState), by = "SiteCode")

# Combine site and ws counties
comb_all <- full_join(ws_comb,
                        site_comb |> select(SiteCode, ParkCounty, ParkState), by = "SiteCode")

# Add County and State FIPS code
library(tidycensus)
data("fips_codes")
head(fips_codes)
fips_codes <- fips_codes |> mutate(county1 = word(county, 1))

comb_wsfips <- left_join(comb_all, fips_codes, by = c("WStnState" = "state_name",
                                                      "WStnCounty" = "county1")) |>
  mutate(WStnFIPS = paste0(state_code, county_code)) |>
  select(-state, -county, -state_code, -county_code)

comb_parkfips <- left_join(comb_all, fips_codes, by = c("ParkState" = "state_name",
                                                        "ParkCounty" = "county1")) |>
  mutate(ParkFIPS = paste0(state_code, county_code)) |>
  select(SiteCode, ParkFIPS)

closest_WS <- full_join(comb_wsfips, comb_parkfips, by = "SiteCode")
head(closest_WS)
# Add to data folder associated with package
usethis::use_data(closest_WS, overwrite = T)



