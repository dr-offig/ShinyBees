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
library(spectR)
library(hms)
#source('R/audio_analysis.R')

# registerInputHandler("videoR.markers",
#   function(x, session, inputname) 
#   { 
#     lapply(x, function(a) { lapply(a, function(b){ if(is.null(b)) NA else b })})
#   })

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(
    tags$meta(property='Content-Security-Policy', content='upgrade-insecure-requests')),
    
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
      spectR::spectROutput("videoScreen")),
    column(4, 
      fluidRow(
         column(12, DT::dataTableOutput("commentsTable"))),
      fluidRow(
         column(12, DT::dataTableOutput("avrecordsTable"))),
      fluidRow(
         column(5,
            textInput("defaultComment", label="Default comment", value="--")),
         column(5,
            selectInput("defaultColour", label="Default colour", 
                                choices=c("indianred","darkgreen","darkslateblue","hotpink"))),
         column(2, actionButton("deleteSelectedRow","Delete"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  avrecords <- data.frame(recordname = list.dirs('www/data',recursive=FALSE,full.names=FALSE), stringsAsFactors = FALSE)
  #avrecord <- avrecords[[1]]
  
  # utility functions
  formatMarkers <- function(cm) { data.frame("timeA"=as.numeric(cm$timeA), "timeB"=as.numeric(cm$timeB), 
                                             "type"=cm$type, "colour"=cm$colour, "comment"=cm$comment)}
  
  safeHMS <- function(erp) { if(is.null(erp) || is.na(erp)) NA else as.hms(erp) }
    
  # sanitiseComment <- function(x, session, inputname) { 
  #   print(x)
  #   x 
  # }
  # 
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
      avrecords$recordname[[indices[[length(indices)]]]]
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
    paste0('https://offig.net/video/bees/', rec, '.mp4')
    #paste0('http://localhost:8080/www/data/', rec,'/',rec, '.mp4')
  })

  spectrogramDir <- reactive({
    rec <- selectedAVRecord()
    paste0('https://offig.net/video/bees/', rec)
  })
  
  spectrogramBaseName <- reactive({
    rec <- selectedAVRecord()
    rec
  })
  
  parseComments <- function(somefile) {
    if (file.exists(somefile)) {
      data <- read.csv(somefile, header=TRUE, stringsAsFactors = FALSE)
      if (nrow(data) > 0) {
        data$timeA <- safeHMS(data$timeA)
        data$timeB <- safeHMS(data$timeB)
      }
    } else {
      data <- data.frame(timeA=numeric(0), timeB=numeric(0), type=character(0), colour=character(0), comment=character(0))
      write.csv(data, somefile, quote=FALSE, row.names = FALSE)
      data$timeA <- as.hms(data$timeA)
      data$timeB <- as.hms(data$timeB)
    }
    return(data)
  }
  
  comments <- reactiveFileReader(250, session, commentsFile, parseComments)
  
  markers <- reactive({
    # cm <- rv$comments
    # data.frame("time"=as.numeric(cm$time), "comment"=cm$comment)
    formatMarkers(comments())
  })
  
  commentsProxy = dataTableProxy('commentsTable', deferUntilFlush=FALSE)
  #avrecordsProxy = dataTableProxy('avrecordsTable')
  
  observeEvent(input$commentsTable_cell_edit, {
    
    # save table pagination
    saved_state <- input$commentsTable_state
    saved_page <- (saved_state$start / saved_state$length) + 1
    # print(sprintf("Saving page: %d ", saved_page))
    # parse the new cell value
    info = input$commentsTable_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value
    cm <- comments()
    if (names(cm)[[j]] == "timeA" || names(cm)[[j]] == "timeB") { cm[i, j] <- as.numeric(safeHMS(v)) }
    else { cm[i, j] <- DT::coerceValue(v, cm[i, j]) }
    #comments <- cm
    cm <- cm[order(cm$timeA),]
    #cat(sprintf("setting comment for row: %d col: %d to value: %s\n",i,j,cm[i,j]))
    #replaceData(commentsProxy, cm, resetPaging = FALSE)  # important
    file.append(commentsLogFile(), commentsFile())    
    write.csv(cm, commentsFile(), row.names=FALSE)
    session$sendCustomMessage(
      type = "updateMarkers", 
      data.frame("timeA"=as.numeric(cm$timeA), "timeB"=as.numeric(cm$timeB),
                 "type"=cm$type, "colour"=cm$colour, "comment"=cm$comment))
    
    # restore saved state
    #reloadData(commentsProxy)
    selectPage(commentsProxy,saved_page)
  })
  
  
  observeEvent(input$defaultComment, {
    session$sendCustomMessage(
      type = "updateDefaultComment", 
      input$defaultComment)
  })
  
  
  observeEvent(input$defaultColour, {
    session$sendCustomMessage(
      type = "updateDefaultColour", 
      input$defaultColour)
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
      js$seek("time"= as.numeric(cm$timeA[[index]]))
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
      saved_state <- input$commentsTable_state
      saved_page <- (saved_state$start / saved_state$length) + 1
      
      cm <- comments()
      cm <- cm[-as.numeric(input$commentsTable_rows_selected),]
      cm <- cm[order(cm$timeA),]
      #replaceData(commentsProxy, cm, resetPaging = FALSE)  # important
      file.append(commentsLogFile(), commentsFile())    
      write.csv(cm, commentsFile(), row.names=FALSE)
      session$sendCustomMessage(
        type = "updateMarkers", 
        data.frame("timeA"=as.numeric(cm$timeA), "timeB"=as.numeric(cm$timeB), 
                   "type"=cm$type, "colour"=cm$colour, "comment"=cm$comment)
      )
      selectPage(commentsProxy,saved_page)
    }
  })
  
  
  observeEvent(input$spectR.markers, {
    
    #new_data <- data.frame("seconds.into.showreel"=input$markers$time, "comment"=input$markers$comment)
    #str(unlist(input$markers$time))
    #str(unlist(input$markers$comment))
      #cm <- comments()
      saved_state <- input$commentsTable_state
      saved_page <- (saved_state$start / saved_state$length) + 1
      
      sanitised <- lapply(input$spectR.markers, 
                      function(a) { lapply(a, function(b){ if(is.null(b)) NA else b })})
      timeA <- unlist(sanitised$timeA)
      timeB <- unlist(sanitised$timeB)
      type <- unlist(sanitised$type)
      colour <- unlist(sanitised$colour)
      comment <- unlist(sanitised$comment)

      cm <- data.frame("timeA"=as.numeric(safeHMS(timeA)), "timeB"=as.numeric(safeHMS(timeB)),
                       "type"=type,"colour"=colour,
                       "comment"=comment)
      
      cm <- cm[order(cm$timeA),]
      # cm <- merge(cm, data.frame("timeA"=timeA, "timeB"=timeB,
      #                            "type"=type,"colour"=colour,
      #                            "comment"=comment),
      #             by=c("timeA","type","colour","comment"), all=TRUE, sort=TRUE)
      
      #replaceData(commentsProxy, cm, resetPaging = FALSE)  # important
      
      file.append(commentsLogFile(), commentsFile())    
      write.csv(cm, commentsFile(), quote = FALSE, row.names=FALSE)
      selectPage(commentsProxy,saved_page)
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
      colnames = c("Time In", "Time Out", "Type", "Colour", "Comment"),
      options = list(pageLength = 25, autoWidth = TRUE, stateSave = TRUE)
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
  
  
  output$videoScreen <- renderspectR(spectR("mediaURL"=videoFile(), "mediaName"="showreel", "mediaHasVideo"=TRUE,
                                            "spectrogramDir"=spectrogramDir(), "spectrogramBaseName"=spectrogramBaseName(), 
                                            "spectrogramHeight"=512, "mediaMarkers"=isolate(markers())))
  
  # spectR <- function(mediaURL, mediaName, mediaHasVideo,
  #                    spectrogramDir, spectrogramBaseName, spectrogramHeight,
  #                    mediaMarkers=NULL, width = NULL, height = NULL, elementId = NULL)
  
  #observeEvent(input$saveComments, {
  #  write.csv(comments,"www/comments.csv")  # toggle is a shinyjs function
  #})
}

# Run the application 
shinyApp(ui = ui, server = server)

