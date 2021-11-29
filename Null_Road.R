#30DayMapChallenge | Day 29 | NULL Map
#Null Road (Catawba County, North Carolina)
#Data: Roads via osmdata
#Data: Null Road created in Google Maps - Export KML - Converted to GeoJson in QGIS

library(tidyverse)
library(osmdata)
library(geojsonio)
library(showtext)
library(ggforce)

##Add Font
font_add_google(name = "Open Sans", family = "open sans")

#turn on showtext
showtext_auto()

#read the geojson file
library(geojsonio)
Null_Road <- geojson_read("/Users/ryan.hart/30 Day Map Challenge 2021/Day 29/Null_Road.geojson",  what = "sp")

#fortify to dataframe format
library(broom)
Null_Road_fortified <- tidy(Null_Road)

getbb("Catawba County Carolina")

streets <- getbb("Catawba County Carolina")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()
streets

small_streets <- getbb("Catawba County Carolina")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "#4B9CD3",
          size = .5,
          alpha = .6) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#4B9CD3",
          size = .3,
          alpha = .4) +
  geom_line(data = Null_Road_fortified, 
            aes( x = long, y = lat, group = group), 
            color="black", 
            size = 1) +
  coord_sf(xlim = c(-81.5, -81), 
           ylim = c(35.55, 35.8)) +
  annotate(geom = "curve", x = -81.14, y = 35.6, xend = -81.22, yend = 35.57, 
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = -81.13, y = 35.6, label = "Null Road", hjust = "left", family = theme_get()$text[["family"]], 
           size = 4, fontface = "bold") +
  theme_void() +
  labs(title = "Catawba County, North Carolina\n",
     #subtitle = "Catawba County, North Carolina",
     caption = "\nDesign: @ryanahart \nData: Open Street Data & Google Maps") +
  theme(text = element_text(color="#4B9CD3", family = "open sans"),
        plot.title = element_text(face = "bold",
                                  hjust = 0.5,
                                  size = 18),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        plot.caption = element_text(hjust = 0.5,
                                    size = 9),
        plot.margin =  margin(25, 40, 25, 25),
        plot.background = element_rect(fill="#F5F5F5", color=NA))

#save
ggsave("Null_Road.png", units = "in", width=8, height=6)