library(tidyverse)
library(sf)
library(leaflet)


#https://cwwp2.dot.ca.gov/documentation/district-map-county-chart.htm

ca_highways <- st_read("geo-data/SHN_Lines.geojson")
ca_counties <- st_read("geo-data/California_County_Boundaries.geojson")

sac_area_highways <- ca_highways %>% 
  filter(County == "AMA" | 
           County == "CAL" | 
           County == "ED" | 
           County == "NEV" | 
           County == "PLA" |
           County == "SAC" |
           County == "SJ" |
           County == "SOL" |
           County == "STA" |
           County == "SUT" |
           County == "TUP" |
           County == "YOL" |
           County == "YUB")


sac_area_counties <- ca_counties %>% 
  filter(CountyName == "Amador" |
           CountyName == "Calaveras" |
           CountyName == "El Dorado" |
           CountyName == "Nevada" |
           CountyName == "Placer" |
           CountyName == "Sacramento" |
           CountyName == "San Joaquin" |
           CountyName == "Solano" |
           CountyName == "Stanislaus" |
           CountyName == "Sutter" |
           CountyName == "Tuolumne" |
           CountyName == "Yolo" |
           CountyName == "Yuba")

st_write(sac_area_counties, "output/sac_area_counties.geojson")

sac_area_top_highways <- sac_area_highways %>% 
  filter(Route == 80 |
           Route == 50 |
           Route == 5 )

st_write(sac_area_top_highways, "output/sac_area_top_highways.geojson")

sac_area_highways_map <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolylines(data = sac_area_top_highways, color = "navy", opacity = 1, weight = 2) %>% 
  addPolygons(data = sac_area_counties, fillColor = "lightblue", opacity = 0.8, weight = 0)

sac_area_highways_map



#SOCAL

socal_area_highways <- ca_highways %>% 
  filter(County == "LA" | 
           County == "ORA" | 
           County == "RIV" | 
           County == "SBD" | 
           County == "VEN")


socal_area_counties <- ca_counties %>% 
  filter(CountyName == "Los Angeles" |
           CountyName == "Orange" |
           CountyName == "Riverside" |
           CountyName == "San Bernardino" |
           CountyName == "Ventura")

st_write(socal_area_counties, "output/socal_area_counties.geojson")

socal_area_top_highways <- socal_area_highways %>% 
  filter(Route == 5 |
           Route == 405 |
           Route == 10 | 
           Route == 210)

st_write(socal_area_top_highways, "output/socal_area_top_highways.geojson")


socal_area_highways_map <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolylines(data = socal_area_top_highways, color = "navy", opacity = 1, weight = 2) %>% 
  addPolygons(data = socal_area_counties, fillColor = "lightblue", opacity = 0.8, weight = 0)

socal_area_highways_map



bay_area_highways <- ca_highways %>% 
  filter(County == "ALA" | 
           County == "CC" | 
           County == "SF" | 
           County == "SCL" | 
           County == "MRN" |
           County == "SM")


bay_area_counties <- ca_counties %>% 
  filter(CountyName == "Alameda" |
           CountyName == "Contra Costa" |
           CountyName == "San Francisco" |
           CountyName == "Santa Clara" |
           CountyName == "Marin" |
           CountyName == "San Mateo")

st_write(bay_area_counties, "output/bay_area_counties.geojson")

bay_area_top_highways <- bay_area_highways %>% 
  filter(Route == 101 |
           Route == 580 |
           Route == 680 | 
           Route == 280) 

st_write(bay_area_top_highways, "output/bay_area_top_highways.geojson")

bay_area_highways_map <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolylines(data = bay_area_top_highways, color = "navy", opacity = 1, weight = 2) %>% 
  addPolygons(data = bay_area_counties, fillColor = "lightblue", opacity = 0.8, weight = 0)

bay_area_highways_map
  