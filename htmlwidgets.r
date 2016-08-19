#########    htmlwidgets   ###############
#Leaflet
library(leaflet)
library(dynlm)
pal <- colorQuantile("YlOrRd", NULL, n = 8)
leaflet(orstationc) %>% 
  addTiles() %>%
  addCircleMarkers(color = ~pal(tann))
