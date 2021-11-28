# install.packages("rjson")
# install.packages("jsonlite") 
# install.packages("leaflet")
library("rgdal")
library("jsonlite")
library(leaflet)
library(foreach)
library(purrr)
vectorize_fromJSON <- Vectorize(fromJSON)
refectorWKT <- function(polygon){
  coordinates <- list()
  pl <- vectorize_fromJSON(polygon)
  # print(length(pl))
  c <- 0
  for (i in seq(1,length(pl)/2,by=1)) {
    # print(pl[i])
    coordinates<-append(coordinates,list(c(pl[i],coordinates,pl[i+1])))
    
    
  }
  # for (p in pl) {
  #   tmp <- list()
  #   tmp["lat"]<-p[1]
  #   tmp["lng"]<-p[2]
  #   print(tmp["lat"])
  #   a[[length(a)+1]]<-tmp
  # }
  tmp = list(list(coordinates))
  
  return(list(
    type = "Feature",
    geometry = list(
      type = "MultiPolygon",
      coordinates = list(list(coordinates)))
    ,
    properties = list(
      name = "Ballard",
      population = 48000,
      # You can inline styles if you want
      style = list(
        fillColor = "yellow",
        weight = 2,
        color = "#000000"
      )
    ),
    id = "ballard"
  ))
  
}
point_data = fromJSON(txt = "G:/DataAnalysis/DataSet/dnmap/pois.json" )
json_file <- "G:/DataAnalysis/DataSet/dn.json"
json_data = fromJSON(txt=json_file) %>% as.data.frame
area_data = fromJSON(txt = "G:/DataAnalysis/DataSet/dnmap/areas.json" )
# area_data <- transform(area_data,wkt=refectorWKT(polygon))
# area_data$polygon <- map(area_data$polygon, fromJSON)

m <- leaflet() %>%
  addTiles() 
m <- m %>% setView(lng=108.156697, lat=16.037833,zoom = 12)  # Add default OpenStreetMap map tiles
m <- m %>% addMarkers(lng=108.156697, lat=16.037833, popup="The birthplace of R")
# m %>% addGeoJSON(area_data$wkt)
for (row in 1:nrow(area_data)) {
  polygonStr <- area_data[row, "polygon"]
  name <- area_data[row, "name"]
  color_hex_background <- area_data[row, "color_hex_background"]
  vtpoly = vectorize_fromJSON(polygonStr)
  print(vtpoly[length(vtpoly)-10])
  a <- list()
  for (i in seq(1,length(vtpoly)/2,by=1)) {
    p <- c(vtpoly[length(vtpoly)/2+i],vtpoly[i])
    print("---")
    a <- append(a,list(p))
  }
  geoj <- list(
    type = "Feature",
    geometry = list(
      type = "MultiPolygon",
      coordinates = list(list(a))
    ),
    properties = list(
      name = "Ballard",
      population = 48000,
      # You can inline styles if you want
      style = list(
        fillColor = color_hex_background,
        fillOpacity=0.7,
        weight = 2,
        color = "#000000"
      )
    ),
    id = "ballard"
  )
  m <- m %>% addMarkers(lat = a[[1]][2],lng = a[[1]][1], popup=name)
  m <- m %>% setView(lat = a[[1]][2],lng = a[[1]][1], zoom = 13) %>% addGeoJSON(geoj)
  
  
}

m
# seattle_geojson <- list(
#   type = "Feature",
#   geometry = list(
#     type = "MultiPolygon",
#     coordinates = list(list(list(
#       c(111.49672,17.10387 ),
#       c(112.27675,16.97256 ),
#       c(112.74367,16.65707),
#       c(112.77663, 16.45171 ),
#       c(112.62282,16.04564),
#       c(112.51845,15.99284),
#       c(111.1946,15.7445 )
#     )))
#   ),
#   properties = list(
#     name = "Ballard",
#     population = 48000,
#     # You can inline styles if you want
#     style = list(
#       fillColor = "yellow",
#       weight = 2,
#       color = "#000000"
#     )
#   ),
#   id = "ballard"
# )
# m %>% setView(lat = 17.10387, lng=111.49672, zoom = 13) %>% addGeoJSON(seattle_geojson)

