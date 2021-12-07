#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

current_date = as.Date("2014-07-11","%Y-%m-%d")
# Define UI for application that draws a histogram
shinyUI(bootstrapPage(

    # Application title
    tags$head(
        tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
        tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}"),
        tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                    type="text/javascript"),
        tags$script('
                $(document).ready(function () {
                  navigator.geolocation.getCurrentPosition(onSuccess, onError);
                
                  function onError (err) {
                    Shiny.onInputChange("geolocation", false);
                  }
                
                  function onSuccess (position) {
                    setTimeout(function () {
                      var coords = position.coords;
                      console.log(coords.latitude + ", " + coords.longitude);
                      Shiny.onInputChange("geolocation", true);
                      Shiny.onInputChange("lat", coords.latitude);
                      Shiny.onInputChange("long", coords.longitude);
                    }, 1100)
                  }
                });
                ')
    ),

    # Sidebar with a slider input for number of bins
    
    leafletOutput("MapPlot1",width = "100%", height = "100%"),
    absolutePanel(id = "controls", class = "panel panel-default",
                  top = 75, left = 55, width = 350, fixed=TRUE,
                  draggable = TRUE, height = "auto",
                  h4("Case COVID-19 by month"),
                  plotOutput("line"),
                  # span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
                  # h3(textOutput("reactive_case_count"), align = "right"),
                  # h4(textOutput("reactive_death_count"), align = "right"),
                  # h6(textOutput("clean_date_reactive"), align = "right"),
                  # h6(textOutput("reactive_country_count"), align = "right"),
                  # plotOutput("epi_curve", height="130px", width="100%"),
                  # plotOutput("cumulative_plot", height="130px", width="100%"),
                  
                  dateInput("date", "Date:", value = "2021-11-28",
                            datesdisabled = c("2012-03-01", "2012-03-02")),
                  actionButton("all_time", "All time")
    )
))
