library(shiny)
library(leaflet)
library(sp)
library(tmap)
library(data.table)
tmap_mode("view")

setwd("C:\\Users\\Daniel I. Patterson\\Desktop\\Shiney\\shiny_bike_share_app\\data")

# Define UI ----
ui <- fluidPage(
  
  # Application title
  titlePanel("Bike Expansion Tool"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      h2(strong("Parameters")),
      br(), 
      br(),
#for future data
#      fluidRow(
#        column(12,
#               radioButtons("city", 
#                            h4("Where are we expanding?"),
#                            choices = list(
#                              "New York City"  = "NYC",
#                              "Boston" = "Boston",
#                              "Philadelphia" = "Philadelphia",
#                              "Minneapolis" = "Minneapolis",
#                              "San Francisco" = "San Francisco",
#                              "Columbus" = "Columbus"),
#                            selected = "NYC")
#        )
#      ),

      fluidRow(
        column(12,
               radioButtons("radio", 
                            h4("Will the distribution take place in the day or overnight?"),
                            choices = list(
                              "Day"  = 6,
                              "Night" = 10),
                            selected = 6),
               helpText("This will determine how much time there is to deliver the bikes.")
                            )
        ),
       fluidRow(
        column(12,
               numericInput("addbikes", 
                            h4("How many bikes are we adding?"), 
                            value =100)
               )
        ),
      fluidRow(
        column(12,
               numericInput("vans", 
                            h4("How many van will be available to distribute new bikes?"), 
                            value = 4),
               helpText("Please only include vans that will be available for distributing", em("new"), "bikes during the shift.")
        )
      ),
      fluidRow(
        column(12,
               numericInput("stations", 
                            h4("How many stations will get new bikes?"), 
                            value = 40)
        )
      ),
        fluidRow(
          column(12,
                 img(src = "spots.png", height = 100, width = 100, position = "bottom"),
                 p("This tool was developed by Daniel Patterson, more of his work can be found on", a(href = "http://www.urbandatacyclist.com", "his webpage."),
                   p("Thanks for checking in!")
                 )
          )
        )
    ),
       
    mainPanel(
      h2(strong("Expansion Plan")),
      br(),
      strong(textOutput("Statement")),
      br(),
      textOutput("Statement2"),
      br(),
      br(),
      fluidRow(
        column(12,
               leafletOutput("map"
               )
        )
      ),
      br(),
      br(),
      p("This map will help planning the expansion. You can see on the map how many bikes are assigned to each station."),
      br(),
      fluidRow(
        column(12, downloadButton("downloadDataFromTable", "Download Table Data"))
      ),
      
      fluidRow(
        column(12,
               dataTableOutput("table"
               )
        )
      ),
      
      br(),
      br(),
      p("This list includes the most popular stations and calculates how many bikes should be added to each station given thier relative popularity in the past month."),
      br(),
      p("If during the expansion there arn't enough spaces for additional bikes, continue with the expansion and dispurse the bikes as you would if they were part of the daily grind.",
        em("Keep up the good work!")
        )
  )
)
)

  
  
  
# Define server logic ----
server <- function(input, output) {
  
  
  output$Statement <- renderText({
    A <- input$stations
    B <- as.numeric(input$radio)
    C <- input$vans
    D <- input$addbikes
    E <- ceiling(A/B)
    F <- ceiling(E/C)
    G <- ceiling(D/B)
    paste("During this shift, each van will be expected to reach approximatily,", F,"stations and deliver", G, "bikes per hour. Is this managable?")
  })
  
  
  
  output$Statement2 <- renderText({
    paste("Here's the plan!")
  })
  

  
  output$table <- renderDataTable({
  
    if ( (input$radio==6)
    ) {
         #morning data
         morning_hires <- read.csv("MH.csv", sep=',', header=T)
         #get unique id names to merge with morning hires
         ids <- unique(subset(data, select = c(start.station.id, start.station.name)))
         morning_hires2 <- merge(morning_hires, ids, by = "start.station.id", all.x = T, all.y = F)
         morning_hires2 <- morning_hires2[order(-morning_hires2$hire),] 
         distribution_list <- morning_hires2[1:input$stations,]
         distribution_list$perc <- distribution_list$hire/sum(distribution_list$hire)
         distribution_list$bike_count <- round(distribution_list$perc*input$addbikes,0)
         distribution_list <- subset(distribution_list, bike_count > 0)
         distribution_list <- distribution_list[order(-distribution_list$bike_count),] 
         final_list <- subset(distribution_list, select = c("start.station.name", "bike_count"))
         
         final_list
         
         
    } else {
      #evening data
      evening_hires <- read.csv("EH.csv", sep=',', header=T)
      ids <- unique(subset(data, select = c(start.station.id, start.station.name)))
      evening_hires2 <- merge(evening_hires, ids, by = "start.station.id", all.x = T, all.y = F)
      evening_hires2 <- evening_hires2[order(-evening_hires2$hire),] 
      evening_distribution_list <- evening_hires2[1:input$stations,]
      evening_distribution_list$perc <- evening_distribution_list$hire/sum(evening_distribution_list$hire)
      evening_distribution_list$bike_count <- round(evening_distribution_list$perc*input$addbikes,0)
      evening_distribution_list <- subset(evening_distribution_list, bike_count > 0)
      evening_distribution_list <- evening_distribution_list[order(-evening_distribution_list$bike_count),] 
      final_evening_list <- subset(evening_distribution_list, select = c("start.station.name", "bike_count"))
      
      final_evening_list
   
  }
    }
  )

  
  
  output$map <- renderLeaflet({
    
    if ( (input$radio==6)
    ) {
      #morning data
      morning_hires <- read.csv("MH.csv", sep=',', header=T)
      #get unique id names to merge with morning hires
      ids <- unique(subset(data, select = c(start.station.id, start.station.name)))
      morning_hires2 <- merge(morning_hires, ids, by = "start.station.id", all.x = T, all.y = F)
      morning_hires2 <- morning_hires2[order(-morning_hires2$hire),] 
      distribution_list <- morning_hires2[1:input$stations,]
      distribution_list$perc <- distribution_list$hire/sum(distribution_list$hire)
      distribution_list$bike_count <- round(distribution_list$perc*input$addbikes,0)
      distribution_list <- subset(distribution_list, bike_count > 0)
      distribution_list <- distribution_list[order(-distribution_list$bike_count),] 
      final_list <- subset(distribution_list, select = c("start.station.name", "bike_count"))
      
      pts1 <- subset(distribution_list, select = c(start.station.longitude, start.station.latitude))
      distribution_spatial <- SpatialPointsDataFrame(pts1, data = distribution_list,
                                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addCircleMarkers(data = distribution_spatial, 
                         label = as.character(distribution_spatial$bike_count)
        )
      
      
    } else {
      #evening data
      evening_hires <- read.csv("EH.csv", sep=',', header=T)
      ids <- unique(subset(data, select = c(start.station.id, start.station.name)))
      evening_hires2 <- merge(evening_hires, ids, by = "start.station.id", all.x = T, all.y = F)
      evening_hires2 <- evening_hires2[order(-evening_hires2$hire),] 
      evening_distribution_list <- evening_hires2[1:input$stations,]
      evening_distribution_list$perc <- evening_distribution_list$hire/sum(evening_distribution_list$hire)
      evening_distribution_list$bike_count <- round(evening_distribution_list$perc*input$addbikes,0)
      evening_distribution_list <- subset(evening_distribution_list, bike_count > 0)
      evening_distribution_list <- evening_distribution_list[order(-evening_distribution_list$bike_count),] 
      final_evening_list <- subset(evening_distribution_list, select = c("start.station.name", "bike_count"))
      
      pts2 <- subset(evening_distribution_list, select = c(start.station.longitude, start.station.latitude))
      evening_distribution_spatial <- SpatialPointsDataFrame(pts2, data = evening_distribution_list,
                                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addCircleMarkers(data = evening_distribution_spatial, 
                         label = as.character(evening_distribution_spatial$bike_count)
                         )
    }
  }
  )
}
    
    
    
# Run the app ----
shinyApp(ui = ui, server = server)



