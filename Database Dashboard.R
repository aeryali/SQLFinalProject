# Before running the code below, install all the packages 
library(shiny)
library(RMySQL)
library(shiny)
library(shinythemes)
library(rgdal)
library(DT)
library(corrplot)
library(RCurl)
library(rsconnect)
library(dplyr)

mydb = dbConnect(MySQL(), user='sql5486670', password='kuavMKDwBL', dbname='sql5486670', host='sql5.freemysqlhosting.net')


rs = dbSendQuery(mydb, "select Flight_Key, Flight_Date,TIME_FORMAT(Depart_Time, '%H %i %s') as Depart_Time,Flight_Num, left(stops,8) as stop, TIME_FORMAT(Arrival_Time, '%H %i %s') as Arrival_Time, price, Airline_Name, (select city from City as c where f.Depart_City_Id =  c.City_Key ) as Depart_City, (select city from City as c where f.Arrival_City_ID = c.City_Key ) as Arrival_City from Flights as f join Airline as a on f.Airline_key = a.Airline_Key")

data = fetch(rs, n=-1)


library(shiny)
library(shinythemes)

rs1 = dbSendQuery(mydb, "select Airline_name ,count(flight_key) count_num from Flights as f join Airline as a on f.airline_key = a.airline_key group by Airline_name" )

data1 = fetch(rs1, n=-1)
# Define UI
ui <- fluidPage(theme = shinytheme("sandstone"),
                navbarPage(
                  theme = "sandstone",
                  "Database Project",
                  tabPanel("About",
                           mainPanel(
                             h1("Flight App", align = "left"),
                             h2("Alisha Aery- Data Visualization", align = "left"),
                             h2("Hannah Corso - Report Builder ", align = "left"),
                             h2("Saymon Mameza - Database Design and Server Deployment ", align = "left"),
                             
                           )),
                  tabPanel("Data Visualization 1",
                           sidebarPanel(
                             sliderInput(inputId = "bins",
                                         label = "Number of bins:",
                                         min = 1,
                                         max = 50,
                                         value = 30)
                             
                           ),
                           
                           
                           # Main panel for displaying outputs ----
                           mainPanel(
                             
                             # Output: Histogram ----
                             plotOutput(outputId = "distPlot"),
                             plotOutput("plot")
                           )),
                  
                  
                  tabPanel("Flight Report",
                           mainPanel(
                             # titlepanel(h2("Report Builder",align="left")) dont need this
                             DTOutput(outputId = "table")
                             
                             
                           ))
                  
                ) # nPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  output$table <- renderDT(data)
  
  output$distPlot <- renderPlot({
    
    x    <- data$price
    x    <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = "frequency",
         main = "Histogram of prices")
    
  })
  
  output$plot <- renderPlot({
    sales = data1
    
    #Summarize Data and Plot Chart
    df <- sales %>% group_by(Airline_name) %>% summarize(Air = sum(count_num))
    #Plot 
    barplot(df$Air,
            names.arg = c("Air India", "Air Asia", "Go First", "Indigo", "Spice Jet", "Vistara"),
            col = "darkred",
            horiz = FALSE,
            xlab = "airlines",
            ylab = "count")
  })
  
  
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)
