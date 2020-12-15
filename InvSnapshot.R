library(dplyr)
library(shiny) 
library(streamgraph)
library(htmlwidgets)

#Reading file from defined directory S:\ModPac\Shared\Max W\R
stream <- read.csv("snapshot.csv")

#Cleaning Date Format
stream$snapshotDate <- as.factor(stream$snapshotDate)
abis <- strptime(stream$snapshotDate,format = "%m/%d/%Y")
stream$snapshotDate <- as.Date(abis,format = "%Y-%m-%d")

#Removing white space in strings
stream$dieLocation <- trimws(stream$dieLocation)
stream$itemNumber <- trimws(stream$itemNumber)

#Removing commas, convert qty to numeric
stream$quantityOnHand <- gsub(",","",stream$quantityOnHand)
stream$quantityOnHand <- as.numeric(stream$quantityOnHand)

#Formatting User Interface
ui <- fluidPage(
  h2("Inventory History by Item", style="text-align:left"),
  br(),
  h4("Select Date Range, Die Location", style="text-align:left"),
  
  sidebarLayout(
      sidebarPanel(
      
          selectInput(inputId = "dieLocation",label = "Die Location",
                      choices = sort(unique(stream$dieLocation)),selected="M-085"),

          dateRangeInput(inputId = "snapshotDate",label = "Date Range",
                         start = as.Date('2020-01-01'), end = as.Date('2020-12-31')),

          actionButton("Reset","Reset Dates")
      
                      ),
    
      mainPanel(
      
      #Defining UI Outputs (Graph, Current On-Hand Qty)
      streamgraphOutput(outputId = "streamPlot"),
      h2(textOutput("CurrentInv"), style="text-align:right"))
                )
                )

#Mapping UI Inputs to Outputs
server <- function(input,output,session){

  output$streamPlot <- renderStreamgraph({
    
    filter_1<-
      stream %>%
      filter(dieLocation==input$dieLocation,
             snapshotDate>=input$snapshotDate[1],
             snapshotDate<=input$snapshotDate[2]) %>%
      select(itemNumber,snapshotDate,quantityOnHand) %>%
      arrange(desc(snapshotDate),desc(quantityOnHand))
    
    #Check to make sure die location has data in set date range
    validate(need(nrow(filter_1)!=0, "Adjust Date Range"))
    
    qual<-filter_1[!duplicated(filter_1$itemNumber),]
    qual<-qual %>% arrange(desc(snapshotDate))
    qual$seq<-1:length(qual$itemNumber)
    
    merge<-merge(filter_1,qual[,c(1,4)],by="itemNumber")
    merge$code<-paste0(merge$seq,".",merge$itemNumber)
    merge$code<-formatC(merge$code,digits=ncar(merge$itemNumber),format='f')
    
    streamgraph(merge,key="code",value="quantityOnHand",date="snapshotDate",
                offset="zero", height="800px", width="1000px",
                left=70,order="asis") %>%
      sg_axis_x(tick_format = "%b%y")
  })
  
  output$CurrentInv <- renderText({
      
    filter_2 <-
        stream %>%
        filter(dieLocation==input$dieLocation) %>%
        select(itemNumber,snapshotDate,quantityOnHand) %>%
        arrange(desc(snapshotDate),desc(quantityOnHand))
    
    CurrentInv <- filter_2 %>% filter(snapshotDate==filter_2[1,2])
    
    paste0("Current Units On-Hand for ",input$dieLocation,": ",as.numeric(sum(CurrentInv$quantityOnHand)))
                                   })
  
  observeEvent( input$Reset , {
    cat("Reset Dates")
    updateDateRangeInput(session, "snapshotDate", start = '2020-01-01', end = '2020-12-31')
                              })
                                    }

shinyApp(ui=ui,server=server)
