library(shiny)
library(tidyverse)
library(tuneR)
require(cowplot)
source("series2sound.R")

# load data
demoDat <- read_csv("demoData.csv")
demoDat <- demoDat %>%
  pivot_wider(names_from = Site, values_from = Growth)

addResourcePath("www", "www")


ui <- fluidPage(
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      h3("Upload Series"),
      #includeMarkdown("text_upload.rmd"),
      p("First col must be time, subsequent cols are series. Headers required"),
      hr(),
      h4("CSV File"),
      fileInput(inputId="file1",
                label=NULL,
                multiple = FALSE,
                accept = c("text/plain",
                           ".rwl",
                           ".raw",
                           ".csv",
                           ".txt")),
      checkboxInput(inputId="useDemoDated",
                    label="Use example data",
                    value=TRUE),
      uiOutput("series2useUI"),
    ),
    mainPanel(
      tags$div(id = "AUDIO_MY"),
      fluidRow(
        plotOutput("datPlotSeries")
      ),
      fluidRow(
        column(2,
               checkboxInput(inputId = "logx",label = "Log x",value = FALSE)
        ),
        column(2,
               checkboxInput(inputId = "logy",label = "Log y",value = FALSE)
        )
      ),
      hr(),
      fluidRow(
        # m=10 - number of "notes"
        column(6,
               sliderInput(inputId = "m", label = "Complexity",
                           min = 1,max = 50,value = 5)
        ),
        # minScaledFreq - lowest kept frequency in Hz
        # maxScaledFreq - highest kept frequency in Hz
        column(6,
               sliderInput(inputId = "freqRange",
                           label = "Output Freq Range (Hz)",
                           min = 55, max = 7040,
                           value = c(220,1760),
                           step = 110)
        )
      ),
      fluidRow(
        # wavLength - length of output wav
        column(6,
               numericInput(inputId = "tSec",
                            label = "Length in Seconds",
                            min = 1,
                            max = 60,
                            step=1,
                            value = 5)
        )
      ),
      hr(),
      fluidRow(
        column(4,
               actionButton(inputId = "genWav", label = "Generate and Play")
        ),
        column(2,
               downloadButton("saveWav", "Save")
        )
      )
    )
  )
)



server <- function(input, output, session) {
  RVs <- reactiveValues()

  ##############################################################
  #
  # Reactives
  #
  ##############################################################

  # Get the RWL file from the user at the start or use demo data
  # shouldn't need to return anything, right?
  # Since it's getting written to RVs
  getData <- reactive({
    if (input$useDemoDated) {
      dat <- demoDat
      RVs$theData <- dat
      RVs$timeVec <- dat[,1] %>% pull()
      dat <- dat[,-1]
      RVs$nSeries <- ncol(dat)
      RVs$theNames <- names(dat)
      return(dat)
    }
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
    else{
      dat <- read_csv(inFile$datapath)
      RVs$theData <- dat
      RVs$timeVec <- dat[,1] %>% pull()
      dat <- dat[,-1]
      RVs$nSeries <- ncol(dat)
      RVs$theNames <- names(dat)
      return(dat)
    }

  })

  getSeries <- reactive({
    req(getData())
    if(!is.null(input$series2use)){
      RVs$theSeries <- RVs$theData %>% pull(input$series2use)
    }
  })


  ##############################################################
  #
  # Server logic for loading and describing the input data
  #
  ##############################################################

  output$series2useUI <- renderUI({
    req(getData())
    selectInput(inputId = "series2use", label = "Select Series",
                choices = RVs$theNames,
                selected = RVs$theNames[1])
  })


  output$datPlotSeries <- renderPlot({
    req(getData())
    req(getSeries())
    tmp <- data.frame(x=RVs$timeVec,y=RVs$theSeries)
    tmpSpec <- spec.pgram(tmp[,2], taper=0, plot=FALSE)
    tmpSpec <- data.frame(x=tmpSpec$freq,y=tmpSpec$spec)

    pTS <- ggplot(tmp,aes(x,y)) +
      geom_line() +
      theme_minimal()

    pSpec <- ggplot(tmpSpec,aes(x,xend=x,y=y,yend=0)) +
      geom_segment() +
      theme_minimal()

    if(input$logx){
      pSpec <- pSpec + scale_x_continuous(trans="log")
    }
    if(input$logy){
      pSpec <- pSpec + scale_y_continuous(trans="log")
    }

    pComb <- plot_grid(
      pTS, pSpec,
      ncol = 1
    )
    return(pComb)
  })

  observeEvent(input$genWav, {
    theWave <- series2sound(series = RVs$theSeries,
                            m=input$m,
                            minScaledFreq = input$freqRange[1],
                            maxScaledFreq = input$freqRange[2],
                            wavLength = input$tSec)
    RVs$theWave <- theWave
    writeWave(theWave, filename="www/out.wav", extensible = TRUE)
    insertUI(selector = "#AUDIO_MY",
             where = "afterEnd",
             ui = tags$audio(src = "www/out.wav", type = "audio/wav",
                             autoplay = NA, controls = NA,
                             style="display:none;"),
             immediate = TRUE)
  })
  output$saveWav <- downloadHandler(
    filename = function() {
      paste("out", "wav", sep=".")
    },
    content = function(file) {
      writeWave(RVs$theWave, filename=file, extensible = TRUE)
    }
  )
}

shinyApp(ui, server)