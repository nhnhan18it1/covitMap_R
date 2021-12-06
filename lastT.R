# install.packages("rjson")
# install.packages("jsonlite") 
# install.packages("leaflet")
# install.packages("stringi")
library("rgdal")
library("jsonlite")
library(leaflet)
library(foreach)
library(purrr)
library(dplyr)
library(stringi)
library(stringr)
library(dplyr)

unidecode <- function(x) {
  Encoding(x) <- "Unicode"
  return(tolower(x))
}


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
point_data = fromJSON(txt = "G:/DataAnalysis/CovidMap//DataSet/dnmap/pois.json" )
json_file <- "G:/DataAnalysis/CovidMap/DataSet/response.json"
json_data = fromJSON(txt=json_file) %>% as.data.frame
json_data %>% group_by(quan_huyen) %>% summarise(so_ca = dplyr::count(quan_huyen))
numberV <- json_data %>% count(quan_huyen)
subset(numberV, (quan_huyen == "SÆ¡n TrÃ"))
numberV[numberV$quan_huyen = "Sơn Trà"]
test = read.csv2("G:/DataAnalysis/CovidMap/DataSet/test.csv",header = TRUE,sep = ",",encoding = "latin1")
numberV <- test%>% count(quan_huyen)
subset(test, (quan_huyen == "SÆ¡n TrÃ"))

distrit_data <- fromJSON(txt = "G:/DataAnalysis/CovidMap/DataSet/dnmap/districtAreas.json")

area_data = fromJSON(txt = "G:/DataAnalysis/CovidMap/DataSet/dnmap/areas.json" )
# area_data <- transform(area_data,wkt=refectorWKT(polygon))
# area_data$polygon <- map(area_data$polygon, fromJSON)


counter<-json_data%>%count(quan_huyen)
counter %>% mutate_if(is.character, str_trim)

# subset(counter,(n>12 && quan_huyen!=""))
tmp <- counter %>%
  filter(quan_huyen == area_data$filterValue[7],n>12)
tmp$n+1


m <- leaflet() %>%
  addTiles() 
m <- m %>% setView(lng=108.156697, lat=16.037833,zoom = 16)  # Add default OpenStreetMap map tiles
m <- m %>% addMarkers(lng=108.156697, lat=16.037833, popup="The birthplace of R")

# m %>% addGeoJSON(area_data$wkt)
for (row in 1:nrow(area_data)) {
  polygonStr <- area_data[row, "polygon"]
  name <- area_data[row, "name"]
  filterVal <- area_data[row, "filterValue"]
  tmp <- counter %>%
    filter(tolower(quan_huyen) == tolower(filterVal))
  print(filterVal)
  print(tmp)
  colorPA <- "#571B7E"
  if (max(tmp$n)<=100) {
    colorPA<-"#FFC125"
  }
  if (max(tmp$n)>100 && tmp$n<=300) {
    colorPA<-"#FFC125"
  }
  if (max(tmp$n)>300 && tmp$n<=500) {
    colorPA<-"#8A4117"
  }
  if (max(tmp$n)>500 && tmp$n<=800) {
    colorPA<-"#7D0552"
  }
  color_hex_background <- area_data[row, "color_hex_background"]
  vtpoly = vectorize_fromJSON(polygonStr)
  a <- list()
  for (i in seq(1,length(vtpoly)/2,by=1)) {
    p <- c(vtpoly[length(vtpoly)/2+i],vtpoly[i])
    a <- append(a,list(p))
  }
  geoj <- list(
    type = "Feature",
    geometry = list(
      type = "MultiPolygon",
      coordinates = list(list(a))
    ),
    properties = list(
      name = name,
      numberPatient = tmp$n,
      # You can inline styles if you want
      style = list(
        fillColor = colorPA,
        fillOpacity=0.7,
        weight = 2,
        color = "#000000"
      )
    ),
    id = name
  )
  m <- m %>% addMarkers(lat = a[[1]][2],lng = a[[1]][1], popup=name)
  m <- m %>% setView(lat = a[[1]][2],lng = a[[1]][1], zoom = 8) %>% addGeoJSON(geoj)
  
  
}

# for (row in 1:nrow(distrit_data)) {
#   polygonStr <- distrit_data[row, "polygon"]
#   mo_ta <- distrit_data[row, "mo_ta"]
#   ma_mau <- distrit_data[row, "ma_mau"]
#   xa_phuong <- distrit_data[row, "xa_phuong"]
#   vtpoly = vectorize_fromJSON(polygonStr)
#   a <- list()
#   for (i in seq(1,length(vtpoly)/2,by=1)) {
#     p <- c(vtpoly[length(vtpoly)/2+i],vtpoly[i])
#     a <- append(a,list(p))
#   }
#   geoj <- list(
#     type = "Feature",
#     geometry = list(
#       type = "MultiPolygon",
#       coordinates = list(list(a))
#     ),
#     properties = list(
#       name = name,
#       population = 48000,
#       # You can inline styles if you want
#       style = list(
#         popup = mo_ta,
#         fillColor = ma_mau,
#         fillOpacity=0.1,
#         weight = 2,
#         color = "#000000"
#       )
#     ),
#     id = name
#   )
#   # m <- m %>% addMarkers(lat = a[[1]][2],lng = a[[1]][1],popup=xa_phuong)
#   m <- m %>% setView(lat = a[[1]][2],lng = a[[1]][1], zoom = 6) %>% addGeoJSON(geoj)
#   
#   
# }
for (row in 1:nrow(point_data)) {
  lat <- point_data[row, "lat"]
  lng <- point_data[row, "lng"]
  description <- point_data[row, "description"]
  m <- m %>% addCircles(lat = lat, weight = 5,lng = lng,color = "red",radius = 3,popup = description)
  m <- m %>% setView(lat = lat, lng = lng, zoom = 6)
}
m<-m %>% addLegend("bottomright", 
                   colors =c("#FFC125", "#FFC125", "#8A4117", "#7D0552", "#571B7E"),
                   labels= c("0 - 100", "100 - 300","300-500","500-800", ">800"),
                   title= "Phân bổ bệnh nhân",
                   opacity = 0.8)
m


tkpa <- json_data %>% filter(quan_huyen=="Thanh Khê") 
tkpa %>% count(tinh_trang)
