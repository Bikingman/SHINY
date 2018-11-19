library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(sp)
library(maptools)
library(scales)
library(ggplot2)
library(RColorBrewer)

URL <- "https://s3.amazonaws.com/hubway-data/index.html"


fileName <- "C:\\Users\\Daniel I. Patterson\\Desktop\\Shiney\\shiny_boston_data_dive\\201810-bluebikes-tripdata\\201810-bluebikes-tripdata.csv"
data <- read.csv(fileName, sep = ',', header = T)

data$hire <- 1
data$age <- 2018 - data$birth.year
data$agerange <- ifelse(data$age <= 14, "0 to 14", 
                        ifelse(data$age >= 15 & data$age <= 24, "15 to 24",
                               ifelse(data$age >= 25 & data$age <= 34, "25 to 34", 
                                      ifelse(data$age >= 35 & data$age <= 44, "35 to 44", 
                                             ifelse(data$age >= 45 & data$age <= 54, "45 to 54", 
                                                    ifelse(data$age >= 55 & data$age <= 64, "55 to 64",
                                                           ifelse(data$age >= 65, "65+", 999
                                                           )))))))
data$genderword <- ifelse(data$gender == 0, "No data",
                          ifelse(data$gender == 1, "Male",
                                 ifelse(data$gender == 2, "Female", 999)))
data$starttime <- strptime(data$starttime, "%Y-%m-%d %H:%M:%S")
data$hour <- as.POSIXlt(data$starttime)$hour
data$dow <- as.POSIXlt(data$starttime)$wday
data$dow <- factor(data$dow, levels = c(0:6), 
                   labels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                              "Thursday", "Friday", "Saturday"))

hour <- aggregate(hire ~ hour + dow, data = data, FUN = sum)


whiskerdata <- aggregate(hire ~ hour + dow + genderword, data = data, FUN = sum)
whiskerdataf <- subset(whiskerdata, genderword == "Female")
whiskerdatam <- subset(whiskerdata, genderword == "Male")
whiskerdatacom <- aggregate(hire ~ hour + dow, data = whiskerdata, FUN = sum)

data$start_time_stripped <- strptime(data$starttime, format = "%Y-%m-%d")
data$start_time_stripped <- as.character(data$start_time_stripped)
line_data <- aggregate(hire ~ start_time_stripped, data = data, FUN = sum)
line_data$start_time_stripped <- as.Date(line_data$start_time_stripped)

station_unique <- unique(data$start.station.name)

# Define UI for application that draws a histogram
ui <- navbarPage("Boston's Blue Bikes Data Dive",
                 id = "nav", 
                 position = "fixed-top",
                 collapsible = TRUE,
                 inverse = T,
                 
 
                 
#######################FIRST PANEL

tabPanel("Interactive Map", 
         div(class="outer",
             
             #this is where css styling will go 
             leafletOutput("map", width="100%", height="100%"),
             
             absolutePanel(
               id = "controls",
               fixed = "TRUE",
               draggable = "TRUE",
               top = 60, 
               left = "auto", 
               right = 20, 
               bottom = "auto",
               width = 330, 
               height = "auto",
               h3("Parameters"), 
               
               radioButtons("radio", 
                            h4("Cumulative or Sperate Bars?"),
                            choices = list(
                              "Cumulative"  = 6,
                              "Seperate" = 10),
                            selected = 6)
             )
         )
),
                                 
              
##################SECOND PANEL                 
                 
                 tabPanel("Temporal Use Patterns", 
                          div(class="outer",
                              style="overflow-y: scroll",
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css")
                              ),
                              
                              #this is where css styling will go 

                              
                          br(),
                          h3("How was the system used over time?"),
                          h4("Use the selection options to your right to dive deeper into the Boston Blue Bikes bikeshare system. There's a lot of infomration to see!"),
                          br(),
                          fluidRow(
                            column(6,
                                   plotOutput("temp_map", height = 800, width = 1200))
                          ),
                          
                 # Sidebar with a slider input for number of bins 
                 absolutePanel(
                   id = "controls",
                   fixed = "TRUE",
                   draggable = "TRUE",
                   top = 60, 
                   left = "auto", 
                   right = 20, 
                   bottom = "auto",
                   width = 330, 
                   height = "auto",
      
                   radioButtons("radio2", 
                                h4("Heatmap or Line Graph"),
                                choices = list(
                                  "Heatmap"  = 8,
                                  "Line Graph" = 9),
                                selected = 8),
                   
                   radioButtons("radio3", 
                                h4("Timescale"),
                                choices = list(
                                  "Day of Week"  = 28,
                                  "Day of Month" = 29),
                                selected = 28),
                   
                   radioButtons("radio4",
                                h4("All Stations or Single Station"),
                                choices = list(
                                  "All Stations" = "all",
                                  "Single Station" = "single"),
                                selected = "all"),
                                
                   
                   conditionalPanel(
                     condition = ("input.radio4 == 'single'"),
                       selectInput("station_selection1",
                               h3("Station Selection"),
                               station_unique)),
                   
                   conditionalPanel(
                     condition = ("input.radio3 == 29"),
                       sliderInput("slider1", 
                                   label = h3("Date Range"), 
                                   as.Date("2018-10-01","%Y-%m-%d"), 
                                   as.Date("2018-10-31", "%Y-%m-%d"), 
                                   value = c(as.Date("2018-10-1"), as.Date("2018-10-31"))))
                   
                   
                   
                 )  
                 )
                 ),
                 

#########################THIRD PANEL

                 tabPanel("Gender Stats",
                          div(class="outer",
                              style="overflow-y: scroll",
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css")
                              ),
                              
                              
                              br(),
                              h3("Combined data"),
                              h4("This section dives into the bike share data made by all users."),
                              fluidRow(
                                column(6,plotOutput("barplot", height = 300, width = 400)),
                                column(6,plotOutput("comwhisker", height = 300, width = 400))
                              ),
                              br(),
                              hr(),
                              h3("Male"),
                              h4("This section dives into data produced by men."),
                              fluidRow(
                                column(6,plotOutput("male", height = 300, width = 400))
                              ),
                              br(),
                              hr(),
                              h3("Female"),
                              fluidRow(
                                column(6,
                                       plotOutput("female", height = 300, width = 400))
                              ),
                              
                              
                              absolutePanel(
                                id = "controls",
                                #class = "model-body",
                                fixed = "TRUE",
                                draggable = "TRUE",
                                top = 60, 
                                left = "auto", 
                                right = 20, 
                                bottom = "auto",
                                width = 330, 
                                height = "auto",
                                h3("Parameters"),
                                radioButtons("radio",
                                             h4("Cumulative or Seperate Bars?"),
                                             choices = list(
                                               "Cumulative"  = 6,
                                               "Seperate" = 10),
                                             selected = 6)
                              )
                          )
                 )
                 
                 
###################close UI

)


                 
                 
                 


                     
   
   
   
      

# Define server logic required to draw a histogram
server <- function(input, output, session) {
 
  output$comwhisker <- renderPlot({
    
    boxplot(whiskerdatacom$hire ~ whiskerdatacom$dow)
    
  }
  )
  
  
  
  
  output$barplot <- renderPlot({
    
    
    
    if ( (input$radio==6)
    ) {
      f <- ggplot(data = data, aes(x = data$agerange)) +
        geom_bar(colour = "black", aes(fill = as.factor(data$genderword))) +
        scale_x_discrete(drop = FALSE) + 
        scale_fill_brewer(palette = "Dark2") +
        labs(title = "Blue Bike Hires By Age and Gender, October 2018", y = "Total Hires", x = "Age Range", fill = "data$genderword") + 
        guides(fill = guide_legend("Legend")) + 
        theme_bw()
      
      f
      
      
    } else {
      h <- ggplot(data = data, aes(x = data$agerange)) +
        geom_bar(position = "dodge", colour = "black", aes(fill = as.factor(data$genderword))) +
        scale_x_discrete(drop = FALSE) + 
        scale_fill_brewer(palette = "Dark2") +
        labs(title = "Blue Bike Hires By Age and Gender, October 2018", y = "Total Hires", x = "Age Range", fill = "data$genderword") + 
        guides(fill = guide_legend("Legend")) + 
        theme_bw()
      
      h
      
    }
  }
  )
  
  
  
  
  
  output$male <- renderPlot({
   
    boxplot(whiskerdatam$hire ~ whiskerdatam$dow)
    
  }
  )
  
  
  
  output$female <- renderPlot({
    
    
    fbp <- ggplot(whiskerdataf, aes(dow, hire)) + 
      geom_boxplot(aes(fill=whiskerdataf$dow)) + 
      guides(fill=FALSE,color=FALSE)+
      labs(title="Female Hires by Day of Week",
           x="Day of Week",
           y="Hires") + 
      theme_classic()
    fbp
  
      }
  )
  
  
  output$temp_map <- renderPlot({
    
    
    
    if ( (input$radio2==8)
    ) {
      p <- ggplot(hour, aes(hour, dow)) + 
        geom_tile(aes(fill = hire),     
                  colour = "white") + 
        scale_fill_gradient(low = "paleturquoise",
                            high = "midnightblue") + 
        labs(title = "Popularity of Hires by Day and Hour", y = "Day of Week", x = "Hour of Day") + 
        guides(fill = guide_legend("Hires")) + 
        theme_classic()
      
      
      p
      
      
    } else {
      l <- ggplot(line_data, aes(as.Date(start_time_stripped), hire)) + 
        geom_line(aes(),     
                  colour = "red") + 
        scale_x_date(limits = as.Date(c(input$slider1[1],input$slider1[2]))) +
        labs(title = "Popularity over Time", y = "Hires", x = "Time") + 
        guides(fill = guide_legend("Hires")) + 
        theme_bw()
      l
      
    }
    
   
    
    
    
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)

