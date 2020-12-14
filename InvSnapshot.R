library("dplyr")
library("shiny") 
library("streamgraph")
library("htmlwidgets")

stream<-read.csv("snapshot.csv")

##Cleaning Date Format
stream$snapshotDate <- as.factor(stream$snapshotDate)
abis<-strptime(stream$snapshotDate,format = "%m/%d/%Y")
stream$snapshotDate<-as.Date(abis,format = "%Y-%m-%d")

##Cleaning Data
stream$dieLocation<-trimws(stream$dieLocation)
stream$itemNumber<-trimws(stream$itemNumber)
stream$quantityOnHand <- gsub(",","",stream$quantityOnHand)
stream$quantityOnHand <- as.numeric(stream$quantityOnHand)

##Formatting User Interface
ui <- fluidPage(
  h2("Inventory History by Item", style="text-align:left"),
  br(),
  h4("Select Date Range, Die Location", style="text-align:left"),

  
  sidebarLayout(
      sidebarPanel(
      
      dateRangeInput(inputId = "snapshotDate",label = "Date Range",
                     start = as.Date('2020-01-01'), end = as.Date('2020-12-31')),
      
      selectInput(inputId = "dieLocation",label = "Die Location",
                  choices = sort(unique(stream$dieLocation)),selected = "M-085")
      
                  ),
    
      mainPanel(
      
      streamgraphOutput(outputId = "streamPlot")
               )
                   )
                )

#Connecting UI Inputs to streamPlot Outputs
server <- function(input,output){
  
  output$streamPlot <- renderStreamgraph({
    
    filtered <-
      stream %>%
      filter(dieLocation==input$dieLocation,
             snapshotDate>=input$snapshotDate[1],
             snapshotDate<=input$snapshotDate[2]
             )
    
    streamgraph(filtered,key="itemNumber",value="quantityOnHand",date="snapshotDate",
                offset="zero", height="800px", width="1000px",
                left=70) 
    
                                          })
  
                                  }

shinyApp(ui=ui,server=server)
