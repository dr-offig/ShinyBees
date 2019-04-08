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

jsCode <- "
shinyjs.seek = function(params) {
var defaultParams = {
time : 0.0
};
params = shinyjs.getParams(params, defaultParams);
document.getElementById('showreel').currentTime = params.time;
}"

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  #tags$script(src = "video_controls.js"),
  tags$style(HTML("video {
    width: 100%    !important;
    height: auto   !important;
  }")),
  
  useShinyjs(),  # Include shinyjs
  extendShinyjs(text = jsCode),
  
  # Application title
  titlePanel("Bee Bioacoustics"),
    
  
  fluidRow(
    column(12,
           #actionButton("snippet1Button", "1"),
           #textInput("snippet1Text", "comments?")
           DT::dataTableOutput("commentsTable")
    )
  ),
    
   # Sidebar with a slider input for number of bins 
  fluidRow(   
      # Show a plot of the generated distribution
      column(12,
        tags$video(id="showreel", type="video/mp4", src="http://offig.net/video/bees/20190318_CERS001.mp4", controls = "controls")

        # plotOutput("distPlot")
      )
  ),
  
  fluidRow(
    column(3, h6("ShowReel")),
    column(3, h6("Full Recording"))),
  
  fluidRow(
    column(3, a(href="http://offig.net/video/bees/20190318_CERS001.mp4","20190318_CERS001.mp4")),
    column(3, a(href="http://offig.net/video/bees/20190318_CERS001.mp4","20190318_CERS001.mp4")))
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
  comments <- read.csv('www/comments.csv',header=TRUE,stringsAsFactors=FALSE)
  
  commentsProxy = dataTableProxy('commentsTable')
  
  observeEvent(input$commentsTable_cell_edit, {
    info = input$commentsTable_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    comments[i, j] <<- DT::coerceValue(v, comments[i, j])
    replaceData(commentsProxy, comments, resetPaging = FALSE)  # important
    file.append("www/comments_log.csv","www/comments.csv")    
    write.csv(comments,"www/comments.csv",row.names=FALSE)
  })
  
 
  observeEvent(input$commentsTable_rows_selected, {
    index = input$commentsTable_rows_selected
    #str(index)
    #i = info$row
    #j = info$col
    #v = info$value
    js$seek("time"= comments[index,3]);
  })
  
  
  output$commentsTable <- renderDT(
    comments, # data
    class = "display nowrap compact", # style
    editable = TRUE,
    selection = 'single',
    colnames = c("Showreel","Full Recording","Time in Showreel","Time","Comment"),
    options = list(autoWidth = TRUE, 
                   columnDefs = list(list(width = '100px', targets = c(0,1,4)),
                                     list(visible=FALSE, targets=c(2,3))))
  )
  
  
  observeEvent(input$saveComments, {
    write.csv(comments,"www/comments.csv")  # toggle is a shinyjs function
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

