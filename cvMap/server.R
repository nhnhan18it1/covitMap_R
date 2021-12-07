#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library(shiny)
library("rgdal")
library("jsonlite")
library(leaflet)
library(foreach)
library(purrr)
library(dplyr)
library(stringi)
library(stringr)
library(dplyr)


vectorize_fromJSON <- Vectorize(fromJSON)


point_data = fromJSON(txt = "G:/DataAnalysis/CovidMap//DataSet/dnmap/pois.json" )
json_file <- "G:/DataAnalysis/CovidMap/DataSet/response.json"
json_data = fromJSON(txt=json_file) %>% as.data.frame
# json_data %>% group_by(quan_huyen) %>% summarise(so_ca = dplyr::count(quan_huyen))
# numberV <- json_data %>% count(quan_huyen)
distrit_data <- fromJSON(txt = "G:/DataAnalysis/CovidMap/DataSet/dnmap/districtAreas.json")

area_data = fromJSON(txt = "G:/DataAnalysis/CovidMap/DataSet/dnmap/areas.json" )

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$line <- renderPlot({
        json_data$ngay_cong_bo <- as.Date(json_data$ngay_cong_bo, "%d/%m/%Y")
        
        plotdt <- json_data %>% mutate(month = format(ngay_cong_bo, "%m"), year = format(ngay_cong_bo, "%Y")) %>%
            group_by(month, year) %>% count(month)
        # print(plotdt)
        ggplot(plotdt, aes(x=month, y=n,group=1)) + geom_line() + geom_point()
    })
    
    output$MapPlot1 <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>% 
            setView(lng=108.156697, lat=16.037833,zoom = 16)%>%
            addMarkers(lng=108.156697, lat=16.037833, popup="The birthplace of R")%>% 
            addLegend("bottomright", 
                     colors =c("#77D4A5", "#FFC125", "#8A4117", "#7D0552", "#571B7E"),
                    labels= c("0 - 100", "100 - 300","300-500","500-800", ">800"),
                    title= "Phân bổ bệnh nhân",
                    opacity = 0.8)
    })
    
    observe({
        numberV <- json_data %>% count(quan_huyen)
        counter<-json_data%>%count(quan_huyen)
        m <- leafletProxy("MapPlot1")%>%
            addMapPane("ames_area", zIndex = 430) %>% # shown below ames_circles
            addMapPane("ames_circles", zIndex = 420)
        for (row in 1:nrow(distrit_data)) {
            polygonStr <- distrit_data[row, "polygon"]
            mo_ta <- distrit_data[row, "mo_ta"]
            ma_mau <- distrit_data[row, "ma_mau"]
            xa_phuong <- distrit_data[row, "xa_phuong"]
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
                    name = xa_phuong,
                    population = 48000,
                    # You can inline styles if you want
                    style = list(
                        popup = mo_ta,
                        fillColor = ma_mau,
                        fillOpacity=0.1,
                        weight = 2,
                        color = "#000000"
                    )
                ),
                id = xa_phuong
            )
            # m <- m %>% addMarkers(lat = a[[1]][2],lng = a[[1]][1],popup=xa_phuong)
            m <- m %>% setView(lat = a[[1]][2],lng = a[[1]][1], zoom = 6) %>% addGeoJSON(geoj)


        }
        for (row in 1:nrow(point_data)) {
            lat <- point_data[row, "lat"]
            lng <- point_data[row, "lng"]
            logo <- point_data[row, "logo"]
            description <- point_data[row, "description"]
            m %>% addMarkers(lat = lat,lng = lng,icon = icons(
                iconUrl=logo,
                iconWidth = 18, iconHeight = 20,
                iconAnchorX = 22, iconAnchorY = 22,
                shadowWidth = 20, shadowHeight = 20,
                shadowAnchorX = 4, shadowAnchorY = 62
            ),popup = description%>%lapply(htmltools::HTML), options = pathOptions(pane = "ames_circles"))
           
        }
        for (row in 1:nrow(area_data)) {
            polygonStr <- area_data[row, "polygon"]
            name <- area_data[row, "name"]
            filterVal <- area_data[row, "filterValue"]
            tkpa <- json_data %>% filter(quan_huyen==filterVal) 
            tkpa<-tkpa %>% count(tinh_trang)
            infor <- ""
            for(i in 1:nrow(tkpa)) {
                
                infor <- paste(infor,"<strong>",tkpa$tinh_trang[i],": ",tkpa$n[i],"<strong/><br/>")
            }
            print(filterVal)
            
            colorPA <- "#571B7E"
            if (sum(tkpa$n)<=100) {
                colorPA<-"#77D4A5"
            }
            if (sum(tkpa$n)>100 && sum(tkpa$n)<=300) {
                colorPA<-"#FFC125"
            }
            if (sum(tkpa$n)>300 && sum(tkpa$n)<=500) {
                colorPA<-"#8A4117"
            }
            if (sum(tkpa$n)>500 && sum(tkpa$n)<=800) {
                colorPA<-"#7D0552"
            }
            color_hex_background <- area_data[row, "color_hex_background"]
            vtpoly = vectorize_fromJSON(polygonStr)
            lat <- c()
            lng <- c()
            
            print(length(vtpoly))
            for (i in seq(1,length(vtpoly)/2,by=1)) {
                lat <- c(lat,vtpoly[i])
                lng <- c(lng,vtpoly[length(vtpoly)/2+i])
            }
            print(length(lat))
            if (length(lat)==7) {
                return()
            }
            label <- paste("<h4>",name,"</h4><br/><strong>số ca nhiễm:", sum(tkpa$n) ," </strong><br/>",infor)
            print(label)
            m %>% addMarkers(lat = lat[1],lng = lng[2], popup=name)
            m %>% setView(lat = lat[1],lng = lng[2], zoom = 8)
            m %>% addPolygons(
                c(lng),
                c(lat),
                fillColor = colorPA,
                fillOpacity=0.4,
                weight = 2,
                color = "#000000",
                group = "area plot",
                label = label%>%lapply(htmltools::HTML),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = "black"),
                    textsize = "15px", direction = "auto")
            )
            
            
            
        }
        
        
    })
    observe({ # update the location selectInput on map clicks
        p <- input$MapPlot1_shape_click
        print(p)
    })
    observeEvent(input$date,{
        json_data$ngay_cong_bo <- as.Date(json_data$ngay_cong_bo, "%d/%m/%Y")
        dtPatientByDay <- json_data %>% filter(ngay_cong_bo == input$date)
        print(length(dtPatientByDay))
        m <- leafletProxy("MapPlot1")%>%
            clearShapes()
        for (row in 1:nrow(distrit_data)) {
            polygonStr <- distrit_data[row, "polygon"]
            mo_ta <- distrit_data[row, "mo_ta"]
            ma_mau <- distrit_data[row, "ma_mau"]
            xa_phuong <- distrit_data[row, "xa_phuong"]
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
                    name = xa_phuong,
                    population = 48000,
                    # You can inline styles if you want
                    style = list(
                        popup = mo_ta,
                        fillColor = ma_mau,
                        fillOpacity=0.1,
                        weight = 2,
                        color = "#000000"
                    )
                ),
                id = xa_phuong
            )
            # m <- m %>% addMarkers(lat = a[[1]][2],lng = a[[1]][1],popup=xa_phuong)
            m <- m %>% addGeoJSON(geoj)
            
            
        }
        
        for (row in 1:nrow(area_data)) {
            polygonStr <- area_data[row, "polygon"]
            name <- area_data[row, "name"]
            filterVal <- area_data[row, "filterValue"]
            tkpa <- dtPatientByDay %>% filter(quan_huyen==filterVal) 
            tkpa<-tkpa %>% count(tinh_trang)
            infor <- ""
            for(i in 1:nrow(tkpa)) {
                
                infor <- paste(infor,"<strong>",tkpa$tinh_trang[i],": ",tkpa$n[i],"<strong/><br/>")
            }
            print(filterVal)
            
            colorPA <- "#571B7E"
            if (sum(tkpa$n)<=100) {
                colorPA<-"#77D4A5"
            }
            if (sum(tkpa$n)>100 && sum(tkpa$n)<=300) {
                colorPA<-"#FFC125"
            }
            if (sum(tkpa$n)>300 && sum(tkpa$n)<=500) {
                colorPA<-"#8A4117"
            }
            if (sum(tkpa$n)>500 && sum(tkpa$n)<=800) {
                colorPA<-"#7D0552"
            }
            color_hex_background <- area_data[row, "color_hex_background"]
            vtpoly = vectorize_fromJSON(polygonStr)
            lat <- c()
            lng <- c()
            
            # print(length(vtpoly))
            for (i in seq(1,length(vtpoly)/2,by=1)) {
                lat <- c(lat,vtpoly[i])
                lng <- c(lng,vtpoly[length(vtpoly)/2+i])
            }
            # print(length(lat))
            if (length(lat)==7) {
                return()
            }
            label <- paste("<h4>",name,"</h4><br/><strong>số ca nhiễm:", sum(tkpa$n) ," </strong><br/>",infor)
            # print(label)
            
            
            m %>% addPolygons(
                c(lng),
                c(lat),
                fillColor = colorPA,
                fillOpacity=0.5,
                weight = 2,
                color = "#000000",
                group = "area plot",
                label = label%>%lapply(htmltools::HTML),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = "black"),
                    textsize = "15px", direction = "auto")
            )
            
            
            
        }
    })
    
    observeEvent(input$all_time,{
        json_data$ngay_cong_bo <- as.Date(json_data$ngay_cong_bo, "%d/%m/%Y")
        dtPatientByDay <- json_data
        print(length(dtPatientByDay))
        m <- leafletProxy("MapPlot1")%>%
            clearShapes()
        for (row in 1:nrow(distrit_data)) {
            polygonStr <- distrit_data[row, "polygon"]
            mo_ta <- distrit_data[row, "mo_ta"]
            ma_mau <- distrit_data[row, "ma_mau"]
            xa_phuong <- distrit_data[row, "xa_phuong"]
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
                    name = xa_phuong,
                    population = 48000,
                    # You can inline styles if you want
                    style = list(
                        popup = mo_ta,
                        fillColor = ma_mau,
                        fillOpacity=0.1,
                        weight = 2,
                        color = "#000000"
                    )
                ),
                id = xa_phuong
            )
            # m <- m %>% addMarkers(lat = a[[1]][2],lng = a[[1]][1],popup=xa_phuong)
            m <- m %>% addGeoJSON(geoj)
            
            
        }
        
        for (row in 1:nrow(area_data)) {
            polygonStr <- area_data[row, "polygon"]
            name <- area_data[row, "name"]
            filterVal <- area_data[row, "filterValue"]
            tkpa <- dtPatientByDay %>% filter(quan_huyen==filterVal) 
            tkpa<-tkpa %>% count(tinh_trang)
            infor <- ""
            for(i in 1:nrow(tkpa)) {
                
                infor <- paste(infor,"<strong>",tkpa$tinh_trang[i],": ",tkpa$n[i],"<strong/><br/>")
            }
            print(filterVal)
            
            colorPA <- "#571B7E"
            if (sum(tkpa$n)<=100) {
                colorPA<-"#77D4A5"
            }
            if (sum(tkpa$n)>100 && sum(tkpa$n)<=300) {
                colorPA<-"#FFC125"
            }
            if (sum(tkpa$n)>300 && sum(tkpa$n)<=500) {
                colorPA<-"#8A4117"
            }
            if (sum(tkpa$n)>500 && sum(tkpa$n)<=800) {
                colorPA<-"#7D0552"
            }
            color_hex_background <- area_data[row, "color_hex_background"]
            vtpoly = vectorize_fromJSON(polygonStr)
            lat <- c()
            lng <- c()
            
            # print(length(vtpoly))
            for (i in seq(1,length(vtpoly)/2,by=1)) {
                lat <- c(lat,vtpoly[i])
                lng <- c(lng,vtpoly[length(vtpoly)/2+i])
            }
            # print(length(lat))
            if (length(lat)==7) {
                return()
            }
            label <- paste("<h4>",name,"</h4><br/><strong>số ca nhiễm:", sum(tkpa$n) ," </strong><br/>",infor)
            # print(label)
            
            
            m %>% addPolygons(
                c(lng),
                c(lat),
                fillColor = colorPA,
                fillOpacity=0.5,
                weight = 2,
                color = "#000000",
                group = "area plot",
                label = label%>%lapply(htmltools::HTML),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = "black"),
                    textsize = "15px", direction = "auto")
            )
            
            
            
        }
    })

})
