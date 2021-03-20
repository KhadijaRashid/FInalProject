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
library(readr)
library(tidyr)

#reading the data in
acs.data <- read_csv("CombinedTable_1019_ZIndex.csv")
#cleaning up the data
acs.data$zscore_disadvantage <- as.numeric(acs.data$zscore_disadvantage)
acs.data <-
    acs.data %>%
    filter(!is.na(zscore_disadvantage))
acs.data1 <- acs.data



# mixin the coulms for all the stats i am interested in: populations
races <- c("asian_pop", "black_pop", "hispanic_pop", "islander_pop", "multi_race_pop", "native_pop", "other_pop", "white_pop")
acs.data <- 
    acs.data %>%
    pivot_longer(
        cols = races,
        names_to = "race_pop",
        values_to = "numbers_race_pop"
    )

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
#plot(cents2)
writeSpatialShape(cents2, "cents2")

# #coordinates and other details
# cents2
# 
# #just the coords (for plotting)
# cents\

################################################  Start of application  ###############################################

# Define UI for application
ui <- navbarPage(span("Disadvantage Index for Pittsburgh"),
                 theme = shinytheme("superhero"),
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
                                              min = min(acs.data$year, na.rm = T),
                                              max = max(geo.match@data$year, na.rm = T),
                                              value = c(min(geo.match@data$year, na.rm = T), max(geo.match@data$year, na.rm = T)),
                                              step = 1)
                              ),
                              # Map Panel
                              mainPanel(
                                  # Using Shiny JS
                                  shinyjs::useShinyjs(),
                                  # Style the background and change the page
                                  tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #033F63;}"),
                                  # Map Page
                                  leafletOutput("leaflet")
                              )
                          )
                 ),
                 
                 tabPanel("Graphs",
                          sidebarLayout(
                              sidebarPanel(
                                  # Select race
                                  checkboxGroupInput(inputId = "raceSelect",
                                                     label = "Racial category:",
                                                     choices = races,
                                                     selected = c("black_pop")),
                                  
                                  # Set point size ----------------------------------------------
                                  sliderInput(inputId = "size", 
                                              label = "Size for Scatterplot:", 
                                              min = 0, max = 5, 
                                              value = 2),
                              ),
                              # Map Panel
                              mainPanel(
                                  fluidRow(
                                      tabBox(width = 20,
                                             tabPanel("Racial Composition (time)", plotlyOutput("plot_years")),
                                             tabPanel("Poverty rate (tracts)", plotlyOutput("plot_tracts")))
                                  ),
                              )
                          )
                 ),
                 
                 # Data Table Pannel
                 tabPanel("Data",
                          fluidPage(
                              wellPanel(DT::dataTableOutput("table"))
                          ),
                          #download button
                          downloadButton(outputId = "download", 
                                         label = "Download Raw Data",
                                         class = "butt1"),
                          tags$head(tags$style(".butt1{background-color:gray;} .butt1{color: black;}")) 
                 )
)

# Define server logic required to create a map
server <- function(input, output) {
    # Basic Map
    output$leaflet <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addProviderTiles("OSM (default)") %>% #or geo.match@data
            setView(lat = 40.440624, lng = -79.995888, zoom = 10)
    })

    # # subsetting the data accoridng to the inputs
    acs.input2 <- reactive({
        acs <- geo.match@data
        # year select
        if (length(input$yearSelect) > 0 ) {
            acs <- subset(acs, acs$year >= input$yearSelect[1] & acs$year <= input$yearSelect[2])
        }
        # tract/GEOID selection 
        if (length(input$tractSelect) > 0 ) {
            acs <- subset(acs, GEOID %in% input$tractSelect)
        }
            # race select
        if (length(input$raceSelect) > 0 ) {
            acs <- subset(acs, race_pop %in% input$raceSelect)
            }
        return(acs)
    })
    
    # Replace layer
    observe({
        data <- acs.input2()
        mybins <- c(-2,-1,0,1,2,3)
        colours <- colorBin(palette = "viridis",
                            domain = data$zscore_disadvantage,
                            na.color = "transparent",
                            bins = mybins)
        # Data is acs now
        leafletProxy("leaflet", data = geo.match) %>%
            clearGroup(group = "disadvantage map") %>%
            addPolygons(fillColor = ~colours(zscore_disadvantage),
                        stroke=TRUE,
                        fillOpacity = 0.9,
                        #color="white",
                        weight = 1.0,
                        group = "disadvantage map", 
                        fill = TRUE, 
                        color = "yellow",
                        popup = ~paste0("<br/>","Population: ", total_pop,"</br/> ", 
                                              "Unemployment: ", round(100-male_employment_rate,2), "%" , "</br /> ", 
                                              "Poverty rate: ", all_poverty_pct,"%", "</p/> ", 
                                              "<b>","Disadvantage score: ", round(zscore_disadvantage, 2), "</b>")
            ) %>%
  #          removeControl("disadvantage map") %>%
            addLegend(position = "bottomright", layerId = ~"leg", pal = colours, opacity = 0.9, values = ~zscore_disadvantage, title = "disadvantage score") 

    })
    
    observe({
    leafletProxy("leaflet", data = geo.match) %>%
        clearGroup(group = "black population heatmap") %>%
        addHeatmap(lng = ~cents[,1],
                   lat = ~cents[,2],
                   radius = 20,
                   group = "black population heatmap",
                   intensity = (geo.match@data$black_pop/geo.match@data$total_pop)*100,
                   blur = 20,
                   max = 0.05) %>%
            addLayersControl(
                baseGroups = c("OSM (default)"),
                overlayGroups = c("disadvantage map", "black population heatmap"),
                options = layersControlOptions(collapsed = FALSE)
)
})
    
    # Data Table
    output$table <- DT::renderDataTable(acs.input2(), options = list(scrollX = T))
    # Print Inputs
    observe({
        print(reactiveValuesToList(input))
    })

    #Download button
    output$download <- downloadHandler('acs.csv', content = function(file){
        geo.match
        write.table(geo.match, file, sep = ",", row.names = FALSE)
    })
    
    # A plot showing the racial categories across time -----------------------------
    output$plot_years <- renderPlotly({
        # Generate Plot ----------------------------------------------
        ggplot(data = acs.input2(), aes(x = as.factor(year), y = as.numeric(numbers_race_pop), fill = race_pop)) + 
            geom_bar(stat = "identity", alpha = 0.5) +
            labs( x = "Years",
                  y = "Number of people") +
            theme(legend.title = element_blank())
    })
    
    # A scatter plot showing the poverty rate across time -----------------------------
    output$plot_tracts <- renderPlotly({
        # Generate Plot ----------------------------------------------
        ggplot(data = acs.input2(), aes(x = as.factor(year), y = all_poverty_pct, fill = GEOID)) + 
            geom_point(stat = "identity", size = input$size, alpha = 0.5) +
            geom_line(linetype = 2, alpha = 0.5) +
#            theme(plot.title = element_text(face = "plain", size = 18)) +
            labs( x = "Years",
                  y = "Poverty (%)") +
            ylim(0,100) +
            stat_summary(fun.y=sum, geom ="line") +
            theme(legend.title = element_blank())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)