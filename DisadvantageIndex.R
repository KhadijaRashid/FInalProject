# Green Infrastructure
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(shinyjs)
library(rgeos)
library(GISTools)
require(dplyr)
require(readxl)
require(stringr)
library(RColorBrewer)
library(GISTools)

#reading the data in
acs.data <- read_csv("CombinedTable_1019_ZIndex.csv")
#cleaning up the data
acs.data$zscore_disadvantage <- as.numeric(acs.data$zscore_disadvantage)
acs.data <-
    acs.data %>%
    filter(!is.na(zscore_disadvantage))

#reading the shapefile in
mapper <- readOGR( 
    dsn = paste0(getwd(),"/ShapeFile") , 
    layer="Allegheny_County_Census_Tracts_2016",
    verbose=FALSE
)
#reading in the rivers shapefile
map.rivers <- readOGR( 
    dsn = paste0(getwd(),"/Allegheny_County_Major_Rivers-shp") , 
    layer="Major_Rivers",
    verbose=FALSE
)

#seems like the same regions as my data
length(mapper)

# Just having the matching GEOID's
#they are all mathcing so do we need a match?
geo.match <- mapper[mapper$GEOID %in% acs.data$GEOID2,]
length(geo.match)

geo.match@data <- merge(geo.match@data, acs.data, sort = FALSE, by.x = "GEOID", by.y = "GEOID2")

#getting the centroid of the polygons
#Rcentroids
# class(mapper)
# plot(mapper)

#read data from shape file to a spatial dataframe
writeSpatialShape(mapper, "mapper")
cents <- coordinates(mapper)
cents2 <- SpatialPointsDataFrame(coords = cents, data=mapper@data, 
                                 proj4string=CRS("+proj=longlat +ellps=clrk66"))
# #points(cents, col = "Blue")
plot(cents2)
writeSpatialShape(cents2, "cents2")

# #coordinates and other shit
# cents2
# 
# #just the coords (for plotting)
# cents

# icons <- awesomeIconList(
#     MS4 = makeAwesomeIcon(icon = "road", library = "fa", markerColor = "gray"),
#     Combined = makeAwesomeIcon(icon = "cloud", library = "fa", markerColor = "blue"),
#     `Non-combined` = makeAwesomeIcon(icon = "tint", library = "fa", markerColor = "green"),
#     `On-site management` = makeAwesomeIcon(icon = "building-o", library = "fa", markerColor = "cadetblue")
# )

# Define UI for application
ui <- navbarPage("Disadvantage Index for Pittsburgh",
                 theme = shinytheme("united"),
                 tabPanel("Map",
                          sidebarLayout(
                              sidebarPanel(
                                  # Select tract
                                  selectInput("tractSelect",
                                              "Tract/GEOID:",
                                              choices = unique(sort(geo.match@data$GEOID)),
                                              selected = c("42003060300"),
                                              selectize = T,
                                              multiple = T),
                                  
                                  # Year Selection ----------------------------------------------
                                  sliderInput("yearSelect",
                                              "Year:",
                                              min = min(geo.match@data$year, na.rm = T),
                                              max = max(geo.match@data$year, na.rm = T),
                                              value = c(min(geo.match@data$year, na.rm = T), max(geo.match@data$year, na.rm = T)),
                                              step = 1)
                                  # #years
                                  # radioButtons("boroSelect",
                                  #              "Borough Filter:",
                                  #              choices = unique(sort(greenInf.load$borough)),
                                  #              selected = "Bronx")
                              ),
                              # Map Panel
                              mainPanel(
                                  # Using Shiny JS
                                  shinyjs::useShinyjs(),
                                  # Style the background and change the page
                                  tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #D4EFDF;}"),
                                  # Map Page
                                  leafletOutput("leaflet")
                              )
                          )
                 ),
                 # Data Table Pannel
                 tabPanel("Data",
                          fluidPage(
                              wellPanel(DT::dataTableOutput("table"))
                          )
                 ),
                 # graph Table Pannel
                 tabPanel("Graphs",
                          fluidPage(
                              wellPanel(DT::dataTableOutput("graph"))
                          )
                 )
)

# Define server logic required to create a map
server <- function(input, output) {
    # Basic Map
    output$leaflet <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addProviderTiles(geo.match@data) %>% #or geo.match@data
            setView(lat = 40.440624, lng = -79.995888, zoom = 10) #%>%
#            addLayersControl(baseGroups = c("Google", "Wiki"))
    })
    # output$leaflet <- renderLeaflet({
    #     leaflet() %>%
    #         addProviderTiles(geo.match@data) %>%
    #         addTiles()  %>%
    #         setView(lat = 40.440624, lng = -79.995888, zoom = 10) %>%
    #         addPolygons(
    #             fillColor = ~colours(zscore_disadvantage),
    #             stroke=TRUE,
    #             fillOpacity = 0.9,
    #             color="white",
    #             weight=0.3,
    #             popup = ~paste0("<br/>","Population: ", total_pop,"</br/> ", "Unemployemnt: ", round(100-male_employment_rate,2), "%" , "</br /> ", "Poverty rate: ", all_poverty_pct,"%", "</p/> ", "<b>","Disadvantage score: ", round(zscore_disadvantage, 2), "</b>")
    #         ) %>%
    #         addLegend(position = "bottomright", pal = colours, opacity = 0.9, values = ~zscore_disadvantage, title = "disadvantage score") %>%
    #         addLayersControl(baseGroups = c("Google", "Wiki"))
    # })

    # # acs data
    acs.input <- reactive({
        acs <- acs.data
        
        # tract/GEOID selection 
        acs <- subset(acs, GEOID2 == input$tractSelect)
        # year
        if (length(input$tractSelect) > 0) {
            acs <- subset(acs, year %in% input$yearSelect)
        }

        return(acs)
    })
    # Replace layer
    observe({
        mybins <- c(-2,-1,0,1,2,3)
        colours <- colorBin(palette = "viridis",
                            domain = mapper@data$zscore_disadvantage,
                            na.color = "transparent",
                            bins = mybins)
        # Data is acs now
        leafletProxy("leaflet", data = geo.match) %>%
            clearMarkers() %>%
            clearGroup(group = "acs") %>%
            addPolygons(fillColor = ~colours(zscore_disadvantage),
                        stroke=TRUE,
                        fillOpacity = 0.9,
                        color="white",
                        weight=0.3,
                        popup = ~paste0("<br/>","Population: ", total_pop,"</br/> ", 
                                              "Unemployemnt: ", round(100-male_employment_rate,2), "%" , "</br /> ", 
                                              "Poverty rate: ", all_poverty_pct,"%", "</p/> ", 
                                              "<b>","Disadvantage score: ", round(zscore_disadvantage, 2), "</b>")
            )
             #                 layerId = ~asset_id) #remove a specifc inf object
    })
    # Data Table
    output$table <- DT::renderDataTable(acs.input(), options = list(scrollX = T))
    # Print Inputs
    observe({
        print(reactiveValuesToList(input))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)