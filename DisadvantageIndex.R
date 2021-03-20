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
                 
                 tabPanel("Graphs",
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
                                              step = 1),
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
                                      tabBox(width = 15,
                                             tabPanel("Racial Composition across", plotlyOutput("plot_years")),
                                             tabPanel("Black population across tracts", plotlyOutput("plot_tracts")))
                                  ),
                              )
                          )
                 ),
                 
#                  # graph Panel
#                  tabPanel("Graphs",
#                           #fluidpage(
#                           sidebarLayout(
#                               sidebarPanel(
                                  # selectInput("popSelect",
                                  #             "Racial category:",
                                  #             choices = races,
                                  #             selected = c("black_pop"),
                                  #             selectize = T,
                                  #             multiple = T),
# 
#                                           # # Input and Value Boxes ----------------------------------------------
#                                           # fluidRow(
#                                           #     valueBoxOutput("Runs", width = 3), #runs types
#                                           #     infoBoxOutput("Start_age", width = 4), #average age
#                                           #     valueBoxOutput("Duration_Spells", width = 4) #duration of spells
#                                           )
#                                       ),
#                                   mainPanel(
                                      # fluidRow(
                                      #     tabBox(width = 12,
                                      #            tabPanel("Age", plotlyOutput("plot_age")),
                                      #            tabPanel("Placement type", plotlyOutput("plot_exit")))
                                      # ),
                                      # 
                                      # # Show scatterplot --------------------------------------------
                                      # plotOutput(outputId = "scatterplot"),
                                      # br(),        # a little bit of visual separation
                                      # # #
                                      # # # Show barplot --------------------------------------------
                                      # plotOutput(outputId = "barplot"),
                                      # br(), br(),        # a little bit of visual separation

#                                       # # # Show summary table ---------------------------------------------
#                                       # tableOutput(outputId = "summtable"),
#                                       # br(), br(),
#                                   )
# ),
                 # Data Table Pannel
                 tabPanel("Data",
                          fluidPage(
                              wellPanel(DT::dataTableOutput("table"))
                          ),
                          #download button
                          downloadButton(outputId = "download", 
                                         label = "Download Raw Data",
                                         class = "butt1"),
                          tags$head(tags$style(".butt1{background-color:blue;} .butt1{color: gray;}")) 
                 )
)

# Define server logic required to create a map
server <- function(input, output) {
    # Basic Map
    output$leaflet <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addProviderTiles("OSM (default)") %>% #or geo.match@data
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
        mybins <- c(-2,-1,0,1,2,3)
        colours <- colorBin(palette = "viridis",
                            domain = acs$zscore_disadvantage,
                            na.color = "transparent",
                            bins = mybins)
        # Data is acs now
        leafletProxy("leaflet", data = geo.match) %>%
#            clearMarkers() %>%
            clearGroup(group = "acs.input2()") %>%
            addPolygons(fillColor = ~colours(zscore_disadvantage),
                        stroke=TRUE,
                        fillOpacity = 0.9,
                        #color="white",
                        weight = 0.5,
                        group = "acs.input2()", 
 #                       layerId = ~acsGEOID2, 
                        fill = TRUE, 
                        color = "green",
                        popup = ~paste0("<br/>","Population: ", total_pop,"</br/> ", 
                                              "Unemployemnt: ", round(100-male_employment_rate,2), "%" , "</br /> ", 
                                              "Poverty rate: ", all_poverty_pct,"%", "</p/> ", 
                                              "<b>","Disadvantage score: ", round(zscore_disadvantage, 2), "</b>")
            ) %>%
            addLegend(position = "bottomright", pal = colours, opacity = 0.9, values = ~zscore_disadvantage, title = "disadvantage score") %>%
            addLayersControl(baseGroups = c("Google", "Wiki"))
    })
    
    observe({
        # Data is acs now
        leafletProxy("leaflet", data = geo.match) %>%
            #            clearMarkers() %>%
            clearGroup(group = "acs.input2()") %>%
            addHeatmap(lng = ~cents[,1], lat = ~cents[,2], 
                       radius = 20, 
                       intensity = (geo.match@data$black_pop/geo.match@data$total_pop)*100,
                       blur = 20, 
                       max = 0.05) %>%
            addLegend(position = "bottomright", pal = colours, opacity = 0.9, title = "density of black population") #%>%
#            addLayersControl(baseGroups = c("Google", "Wiki"))
    })

    # Data Table
    output$table <- DT::renderDataTable(acs.input2(), options = list(scrollX = T))
    # Print Inputs
    observe({
        print(reactiveValuesToList(input))
    })
    # # Print Projects
    # output$text <- renderText({
    #     paste("You are viewing", nrow(onScreen()), "projects")
    # })
    #Download button
    output$download <- downloadHandler('acs.csv', content = function(file){
        acs.data
        write.table(acs.data, file, sep = ",", row.names = FALSE)
    })
    
    # # Create a subset of data filtering for selected genre types ------
    # # Selected type is the checkbox
    # sales_subset <- reactive({
    #     req(input$selected_genre) # ensure availablity of value before proceeding
    #     filter(sales, Genre %in% input$selected_genre)
    # })
    # 
    # # # Convert plot_title toTitleCase ----------------------------------
    # # pretty_plot_title1 <- reactive({ toTitleCase(input$plot_title) })
    # # Create scatterplot object the plotOutput function is expecting --
    # output$scatterplot <- renderPlot({
    #     ggplot(data = sales_subset(), aes_string(x = input$x, y = input$y,
    #                                              color = input$z)) +
    #         geom_point(size = input$size, alpha = input$alpha) +
    #         theme(plot.title = element_text(face = "plain", size = 18)) +
    #         labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
    #              y = toTitleCase(str_replace_all(input$y, "_", " ")),
    #              color = toTitleCase(str_replace_all(input$z, "_", " ")),
    #              title = pretty_plot_title1()
    #         )
    # })
    # 
    # Create barplot object  --
#     output$plot_years <- renderPlot({
#         ggplot(data = acs.input(), aes(x = input$yearSelect, y = input$y)) +
# #            stat_summary(fun = sum, geom = "bar") +
#             geom_bar(fill = "pink", alpha = 1, stat = "identity") +
#             theme(plot.title = element_text(face = "plain", size = 18)) +
#             labs(x = "Year",
#                  y = "Black population across years",
#                  title = "Global Sales across Years/Platforms"
#             )
#     })
#     
    # A plot showing the racial categories across time -----------------------------
    output$plot_years <- renderPlotly({
        # acs.input2 <- reactive({
        #     # race select
        #     if (length(input$raceSelect) > 0 ) {
        #         acs <- subset(acs.input(), race_pop %in% input$raceSelect)
        #     }
        # 
        #     return(acs)
        # })
        #dat <- subset(acs.input(), "race_pop") #numbers_race_pop
        
        # Generate Plot ----------------------------------------------
        ggplot(data = acs.input2(), aes(x = round(as.numeric(year),0), y = as.numeric(numbers_race_pop), fill = race_pop)) + 
            geom_bar(stat = "identity", alpha = 0.5) +
            labs( x = "Years",
                  y = "Number of people") +
            theme(legend.title = element_blank())
    })
    
    # A scatter plot showing the racial categories across location -----------------------------
    acs.input <- reactive({
        acs <- geo.match@data
        # year select
        if (length(input$yearSelect) > 0 ) {
            acs <- subset(acs, acs$year >= input$yearSelect[1] & acs$year <= input$yearSelect[2])
        }
        # tract/GEOID selection 
        if (length(input$tractSelect) > 0 ) {
            acs <- subset(acs, GEOID %in% input$tractSelect)
        }
        
        return(acs)
    })
    
    output$plot_tracts <- renderPlotly({
        # Generate Plot ----------------------------------------------
        ggplot(data = acs.input(), aes(x = total_pop, y = input$tractSelect)) +#, fill = race_pop)) + 
#            stat_summary(fun = sum, geom = "point") +
            geom_point(stat = "identity") +
            geom_point(size = input$size) +
            theme(plot.title = element_text(face = "plain", size = 18)) +
            labs( x = "tracts/GEOIDs",
                  y = "Number of people") +
            theme(legend.title = element_blank())
    })
    # # Convert plot_title toTitleCase ----------------------------------
    # pretty_plot_title1 <- reactive({ toTitleCase(input$plot_title) })
    # # Create scatterplot object the plotOutput function is expecting --
    # output$scatterplot <- renderPlot({
    #     ggplot(data = sales_subset(), aes_string(x = input$x, y = input$y,
    #                                              color = input$z)) +
    #         geom_point(size = input$size, alpha = input$alpha) +
    #         theme(plot.title = element_text(face = "plain", size = 18)) +
    #         labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
    #              y = toTitleCase(str_replace_all(input$y, "_", " ")),
    #              color = toTitleCase(str_replace_all(input$z, "_", " ")),
    #              title = pretty_plot_title1()
    #         )
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)