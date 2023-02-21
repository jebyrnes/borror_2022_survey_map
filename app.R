#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)
library(ggplot2)

#setwd(here::here())

tracks <- readRDS("video_survey_appledore_2022_scaled.rds")

# Define UI for application that draws a histogram

ui <- fluidPage(
  titlePanel("The Subtidal Environment of Appledore Island in 2022"),
  
  fluidRow(
  
    
    column(7,
           HTML("<b>Click a Track to Open a Video</b>"),
           leafletOutput("mymap",
                         height = 550)
    ),
    
    column(5,
           HTML("<b>Video of Track</b>"),
           uiOutput("video")
    )
  ),
  
  fluidRow(
    br(),br(),
    img(src='nasa-logo-web-rgb_small.jpg', align = "left", height = 50),
    img(src='umb_logo.png', align = "left", height = 50),
    HTML("<div style='margin-left:10px;margin-right:10px'>Data from <A href=https://www.nasa.gov/stem/murep/home/index.html>NASA MUREP</a> grant <a href=https://msiexchange.nasa.gov/attachments/4fac5315dbada300f8ebfd0e0f961966/award_abstract_University%20of%20Mass%20Boston%20OCEAN%202020.pdf>Using Hyperspectral Imagery to Assess the Effects of Warming on New England Kelp Forests</a>."),
    HTML("Click on a track to see a video. These videos come from dropcamera footage taken at ~ 3-5m below mean low low water 
         around Appledore Island in the Isles of Shoals, Maine. The survey attempts to recreate surveys done
         previously by Dr. Art Borror in the 1980s. For more, see <a href=https://shiny.umb.edu/shiny/users/jarrett.byrnes/borror_algae_maps_shiny/>here for digitized maps</a>
         or <a href=https://ecoevorxiv.org/repository/view/3759/>here for a manuscript about change over time from Dr. Borror's maps</a>.</div>")
    
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$mymap <- renderLeaflet({
    # generate the map

    
    leaflet() |>
      
      #the setup
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo Map") %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, 
                       group = "ESRI World Gray Canvas",
                       options = providerTileOptions(noWrap = TRUE)) |>
      setView(lat = 42.988, lng = -70.615, zoom = 15) |>
      
      #the data
      addPolylines(data = tracks,
                   col = "orchid",#~pal(`Kelp Dens#`),
                   weight = 7,
                   layerId = ~URL,
                   highlight = highlightOptions(color = "red",weight = 5,
                                                bringToFront = F, opacity = 1),
                   group = "Dropcam Tracks") %>%
      addLayersControl(
        baseGroups = c("World Imagery", "Topo Map", "ESRI World Gray Canvas"),
        overlayGroups = c("Dropcam Tracks", "Diver Biomass Surveys"),
        options = layersControlOptions(collapsed = FALSE)
      ) 
    
    
  })
  
  selected_track <- reactive({
    p <- input$mymap_shape_click
  })
  
  
  
  output$video <- renderUI({
#    print(selected_track())
    url <- selected_track()$id
    #    url <- gsub("youtu\\.be/", "www.youtube.com/embed/", url)
        url <- gsub("youtube\\.com\\/watch\\?v=", "youtube.com/embed/", url)
    
#    HTML(paste0('<iframe width="200" height="100" src="', url, '" frameborder="0" allowfullscreen></iframe>'))
    HTML(paste0(
      '<html>
        <body>
          <iframe id="existing-iframe"
              width="100%" height="360"
              src="',url,'" ###This URL needs to change dynamically based on which link the user clicks in output$table
              frameborder="0"
          ></iframe>

          <script type="text/javascript">
            var tag = document.createElement(\'script\');
            tag.src = \'https://www.youtube.com/iframe_api\';
            var firstScriptTag = document.getElementsByTagName(\'script\')[0];
            firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);

            var player;
            function onYouTubeIframeAPIReady() {
              player = new YT.Player(\'existing-iframe\');
            }
          </script>
        </body>
      </html>'))
      
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
