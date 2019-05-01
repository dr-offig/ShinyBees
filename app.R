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
  #titlePanel("Bee Bioacoustics"),
  
  # tags$script('
  #   $(document).on("keydown", function (e) {
  #      Shiny.onInputChange("keydown", e.key);
  #   });
  # '), 
  # 
  fluidRow(   
    column(8, # The left 2/3 of the page
      fluidRow(
        column(12,
          videoR::videoROutput("videoScreen")),
        column(6, DT::dataTableOutput("avrecordsTable")),
        column(6, 
          fluidRow(
            column(10,
              textInput("defaultComment", label="Default comment", value="--")),
            column(2, actionButton("deleteSelectedRow","Delete"))
          )))),
    column(4, DT::dataTableOutput("commentsTable"))
  )  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  avrecords <- data.frame(recordname = list.dirs('www/data',recursive=FALSE,full.names=FALSE), stringsAsFactors = FALSE)
  #avrecord <- avrecords[[1]]
  
  # utility functions
  formatMarkers <- function(cm) { data.frame("time"=as.numeric(cm$time), "comment"=cm$comment)}
  
  
  # rv <- reactiveValues(rec=avrecords[[1]], 
  #                      commentsFile=paste0('www/data/',rec,'/',rec,'.csv'),
  #                      commentsLogFile=paste0('www/data/',rec,'/',rec,'_log.csv'),
  #                      videoFile=paste0('https://offig.net/video/bees/', rec, '.mp4'),
  #                      comments=parseComments(commentsFile),
  #                      markers=formatMarkers(comments))
  #   
  
  selectedAVRecord <- reactive({
    indices = input$avrecordsTable_rows_selected
    if (length(indices) > 0) {
      avrecords$recordname[[last(indices)]]
    } else {
      avrecords$recordname[[1]]
    }
  })
  
  commentsFile <- reactive({
    rec <- selectedAVRecord()
    paste0('www/data/',rec,'/',rec,'.csv')
  })

  commentsLogFile <- reactive({
    rec <- selectedAVRecord()
    paste0('www/data/',rec,'/',rec,'_log.csv')
  })
  
  videoFile <- reactive({
    rec <- selectedAVRecord()
    #paste0('https://offig.net/video/bees/', rec, '.mp4')
    paste0('http://localhost:8080/www/data/', rec,'/',rec, '.mp4')
  })

  parseComments <- function(somefile) {
    if (file.exists(somefile)) {
      data <- read.csv(somefile, header=TRUE, stringsAsFactors = FALSE)
      if (nrow(data) > 0) {
        data$time <- as.hms(data$time)
      }
    } else {
      data <- data.frame(time=numeric(0), comment=character(0))
      write.csv(data,somefile,row.names = FALSE)
    }
    return(data)
  }
  
  comments <- reactiveFileReader(250, session, commentsFile, parseComments)
  
  markers <- reactive({
    # cm <- rv$comments
    # data.frame("time"=as.numeric(cm$time), "comment"=cm$comment)
    formatMarkers(comments())
  })
  
  commentsProxy = dataTableProxy('commentsTable', deferUntilFlush=TRUE)
  #avrecordsProxy = dataTableProxy('avrecordsTable')
  
  observeEvent(input$commentsTable_cell_edit, {
    
    # save table pagination
    saved_state <- input$commentsTable_state
    saved_page <- (saved_state$start / saved_state$length) + 1
    print(sprintf("Saving page: %d ", saved_page))
    # parse the new cell value
    info = input$commentsTable_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value
    cm <- comments()
    if (names(cm)[[j]] == "time") { cm[i, j] <- as.hms(v) }
    else { cm[i, j] <- DT::coerceValue(v, cm[i, j]) }
    #comments <- cm
    cat(sprintf("setting comment for row: %d col: %d to value: %s\n",i,j,cm[i,j]))
    #replaceData(commentsProxy, cm, resetPaging = FALSE)  # important
    file.append(commentsLogFile(), commentsFile())    
    write.csv(cm, commentsFile(), row.names=FALSE)
    session$sendCustomMessage(
      type = "updateMarkers", 
      data.frame("time"=as.numeric(cm$time), "comment"=cm$comment))
    
    # restore saved state
    #reloadData(commentsProxy)
    selectPage(commentsProxy,saved_page)
  })
  
  
  observeEvent(input$defaultComment, {
    session$sendCustomMessage(
      type = "updateDefaultComment", 
      input$defaultComment)
  })
  
  # observeEvent(input$avrecordsTable_rows_selected, {
  #   selectedIndex <- input$avrecordsTable_rows_selected
  #   rv$rec <- avrecords[[selectedIndex]]
  #   #cm <- rv$comments
  # })
  #  
  
  observeEvent(input$commentsTable_rows_selected, {
      index = input$commentsTable_rows_selected
      cm <- comments()
      js$seek("time"= as.numeric(cm$time[[index]]))
      #updateNumericInput(session, "selectedCommentRow", value=index)
  })

    
  # observeEvent(input$commentsTable_state, {
  #   #print(input$commentsTable_state)
  #   val <- input$commentsTable_state$start / input$commentsTable_state$length + 1
  #   print(sprintf("Page: %d", val))
  #   #updateNumericInput(session, "selectedPage", "Selected Page", val, min = 1)
  # })
  # 
  
  observeEvent(input$deleteSelectedRow,{
    if (!is.null(input$commentsTable_rows_selected)) {
      cm <- comments()
      cm <- cm[-as.numeric(input$commentsTable_rows_selected),]
      #replaceData(commentsProxy, cm, resetPaging = FALSE)  # important
      file.append(commentsLogFile(), commentsFile())    
      write.csv(cm, commentsFile(), row.names=FALSE)
      session$sendCustomMessage(
        type = "updateMarkers", 
        data.frame("time"=as.numeric(cm$time), "comment"=cm$comment))
    }
  })
  
  
  observeEvent(input$markers, {
    
    #new_data <- data.frame("seconds.into.showreel"=input$markers$time, "comment"=input$markers$comment)
    #str(unlist(input$markers$time))
    #str(unlist(input$markers$comment))
      cm <- comments()
      cm <- merge(cm, data.frame("time"=unlist(input$markers$time), "comment"=unlist(input$markers$comment)), 
                                             by=c("time","comment"), all=TRUE, sort=TRUE)
      #replaceData(commentsProxy, cm, resetPaging = FALSE)  # important
      file.append(commentsLogFile(), commentsFile())    
      write.csv(cm, commentsFile(), row.names=FALSE)

  })
  

  # observeEvent(input$selectedPage, {
  #   reloadData(commentsProxy)
  #   #selectPage(commentsProxy,input$selectedPage)
  # })
  #  
  
  output$commentsTable <- renderDT(
      comments(), # data
      class = "display nowrap compact", # style
      editable = TRUE,
      selection = 'single',
      colnames = c("Time", "Comment"),
      options = list(autoWidth = TRUE, stateSave = TRUE)
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
  
  
  output$videoScreen <- renderVideoR(videoR("videoURL"=videoFile(), "videoName"="showreel", "videoMarkers"=isolate(markers())))
  
  #observeEvent(input$saveComments, {
  #  write.csv(comments,"www/comments.csv")  # toggle is a shinyjs function
  #})
}

# Run the application 
shinyApp(ui = ui, server = server)

