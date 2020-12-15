library(dplyr)
library(shiny) 
library(streamgraph)
library(htmlwidgets)

stream<-read.csv("snapshot.csv")

#Cleaning Date Format
stream$snapshotDate <- as.factor(stream$snapshotDate)
abis<-strptime(stream$snapshotDate,format = "%m/%d/%Y")
stream$snapshotDate<-as.Date(abis,format = "%Y-%m-%d")

#Cleaning Data
stream$dieLocation<-trimws(stream$dieLocation)
stream$itemNumber<-trimws(stream$itemNumber)
stream$quantityOnHand <- gsub(",","",stream$quantityOnHand)
stream$quantityOnHand <- as.numeric(stream$quantityOnHand)

#Formatting User Interface
ui <- fluidPage(
  h2("Inventory History by Item", style="text-align:left"),
  br(),
  h4("Select Date Range, Die Location", style="text-align:left"),

  
  sidebarLayout(
    sidebarPanel(
      
      dateRangeInput(inputId = "snapshotDate",label = "Date Range",
                     start = as.Date('2020-01-01'), end = as.Date('2020-12-31')),
      
      selectInput(inputId = "dieLocation",label = "Die Location",
                  choices = sort(unique(stream$dieLocation)),selected="M-085")
      
    ),
    
    mainPanel(
      
      streamgraphOutput(outputId = "streamPlot"),
      h2(textOutput("CurrentInv"), style="text-align:right")
    )
  )
)

#Connecting UI Inputs to streamPlot Outputs
server <- function(input,output){

  output$streamPlot <- renderStreamgraph({
    
    filter_1<-
      stream %>%
      filter(dieLocation==input$dieLocation,
             snapshotDate>=input$snapshotDate[1],
             snapshotDate<=input$snapshotDate[2]) %>%
      select(itemNumber,snapshotDate,quantityOnHand) %>%
      arrange(desc(snapshotDate),desc(quantityOnHand))
    
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
      sg_axis_x(tick_format = "%b-%y")
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
  
}

shinyApp(ui=ui,server=server)
