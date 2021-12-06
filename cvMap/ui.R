#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(leaflet)
library(shiny)

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
    
    leafletOutput("MapPlot1",width = "100%", height = "100%")
))
