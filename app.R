#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(DT)
library(videoR)
library(hms)
source('R/audio_analysis.R')


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  #tags$script(src = "video_controls.js"),
  tags$style(HTML("video {
    width: 100%    !important;
    height: auto   !important;
  }")),
  
  useShinyjs(),  # Include shinyjs
  #extendShinyjs(text = jsCode),
  extendShinyjs(script = "extend.js"),
  
  # Application title
  titlePanel("Bee Bioacoustics"),
  
  fluidRow(   
    column(12,
        videoR::videoROutput("videoScreen")
    )
  ),  
  
  fluidRow(
    column(3,
           h3("Recording Sessions", align="center")
    ),
    column(8,
           h3("Points of Interest", align="center")
    ),
    column(1,
           actionButton("deleteSelectedRow","Delete")
           )
  ),
  
  fluidRow(
    column(3,
           DT::dataTableOutput("avrecordsTable") 
    ),
    column(9,
           #actionButton("snippet1Button", "1"),
           #textInput("snippet1Text", "comments?")
           DT::dataTableOutput("commentsTable")
    )
  )
    
   # Sidebar with a slider input for number of bins 
  #fluidRow(   
      ## Show a plot of the generated distribution
      #column(12,
        #tags$video(id="showreel", type="video/mp4", src="http://offig.net/video/bees/20190318_CERS001.mp4", controls = "controls")

        # plotOutput("distPlot")
      #)
  #),
  
  #fluidRow(
  #  column(3, h6("ShowReel")),
  #  column(3, h6("Full Recording"))),
  
  #fluidRow(
  #  column(3, a(href="https://offig.net/video/bees/20190318_CERS001.mp4","20190318_CERS001.mp4")),
  #  column(3, a(href="https://offig.net/video/bees/20190318_CERS001.mp4","20190318_CERS001.mp4")))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   # output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- faithful[, 2] 
   #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   #    
   #    # draw the histogram with the specified number of bins
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
   # })
  
  avrecords <- data.frame(recordname = list.dirs('www/data',recursive=FALSE,full.names=FALSE), stringsAsFactors = FALSE)
  #avrecord <- avrecords[[1]]
  
  selectedRecord <- reactive({
    indices = input$avrecordsTable_rows_selected
    if (length(indices) > 0) {
      avrecords$recordname[[last(indices)]]
    } else {
      avrecords$recordname[[1]]
    }
  })
  
  commentsFile <- reactive({
    rec <- selectedRecord()
    paste0('www/data/',rec,'/',rec,'.csv')
  })

  commentsLogFile <- reactive({
    rec <- selectedRecord()
    paste0('www/data/',rec,'/',rec,'_log.csv')
  })
  
  videoFile <- reactive({
    rec <- selectedRecord()
    paste0('https://offig.net/video/bees/', rec, '.mp4')
  })
  
  comments <- reactive({
    cm <- read.csv(commentsFile(), header=TRUE,stringsAsFactors=FALSE)
    cm$time <- as.hms(cm$time)
    cm
  })
      
  markers <- reactive({
    cm <- comments()
    data.frame("time"=as.numeric(cm$time), "comment"=cm$comment)
  })
  
  commentsProxy = dataTableProxy('commentsTable')
  #avrecordsProxy = dataTableProxy('avrecordsTable')
  
  observeEvent(input$commentsTable_cell_edit, {
    info = input$commentsTable_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    cm <- comments()
    cm[i, j] <- DT::coerceValue(v, cm[i, j])
    cat(sprintf("setting comment for row: %d col: %d to value: %s\n",i,j,cm[i,j]))
    replaceData(commentsProxy, cm, resetPaging = FALSE)  # important
    file.append(commentsLogFile(), commentsFile())    
    write.csv(cm, commentsFile(), row.names=FALSE)
  })
  
 
  observeEvent(input$commentsTable_rows_selected, {
    index = input$commentsTable_rows_selected
    #str(index)
    #i = info$row
    #j = info$col
    #v = info$value
    cm <- comments()
    #js$seek("time"= as.numeric(cm[index,1]));
    js$seek("time"= as.numeric(cm$time[[index]]));
  })
  
  
  observeEvent(input$deleteSelectedRow,{
    if (!is.null(input$commentsTable_rows_selected)) {
      cm <- comments()
      cm <- cm[-as.numeric(input$commentsTable_rows_selected),]
      replaceData(commentsProxy, cm, resetPaging = TRUE)  # important
      file.append(commentsLogFile(), commentsFile())    
      write.csv(cm, commentsFile(), row.names=FALSE)
    }
  })
  
  
  observeEvent(input$markers, {
    
    #new_data <- data.frame("seconds.into.showreel"=input$markers$time, "comment"=input$markers$comment)
    #str(unlist(input$markers$time))
    #str(unlist(input$markers$comment))
    cm <- comments()
    cm <- merge(cm, data.frame("time"=unlist(input$markers$time), "comment"=unlist(input$markers$comment)), 
                                             by=c("time","comment"), all=TRUE, sort=TRUE)
    replaceData(commentsProxy, cm, resetPaging = TRUE)  # important
    file.append(commentsLogFile(), commentsFile())    
    write.csv(cm, commentsFile(), row.names=FALSE)
  })
  
  
  output$commentsTable <- renderDT(
    comments(), # data
    class = "display nowrap compact", # style
    editable = TRUE,
    selection = 'single',
    # colnames = c("Showreel","Full Recording","Time in Showreel","Time","Comment"),
    # options = list(autoWidth = TRUE, 
    #               columnDefs = list(list(width = '100px', targets = c(0,1,4)),
    #                                 list(visible=FALSE, targets=c(2,3))))
    colnames = c("Time", "Comment"),
    options = list(autoWidth = TRUE)    
  )
  
  
  output$avrecordsTable <- renderDT(
    avrecords, # data
    class = "display nowrap compact", # style
    editable = FALSE,
    selection = 'single',
    colnames = c("Record Name"),
    options = list(autoWidth = TRUE, 
                   columnDefs = list(list(width = '100px', targets = c(0))))
  )
  
  
  output$videoScreen <- renderVideoR(videoR("videoURL"=videoFile(), "videoName"="showreel", "videoMarkers"=markers()))
    
  #observeEvent(input$saveComments, {
  #  write.csv(comments,"www/comments.csv")  # toggle is a shinyjs function
  #})
}

# Run the application 
shinyApp(ui = ui, server = server)

