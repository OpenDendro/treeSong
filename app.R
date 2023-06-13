library(shiny)
library(shinyWidgets)
library(tidyverse)
library(tuneR)
library(cowplot)
library(markdown)
library(shinythemes)
library(shinyjs)

source("series2sound.R")

# load data
demoDat <- read_csv("demoData.csv")
demoDat <- demoDat %>%
  pivot_wider(names_from = Site, values_from = Growth)

octaves <- 27.5*2^(0:7)

addResourcePath("www", "www")


ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("superhero"),
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      style = "position:fixed;width:22%;height:95%;overflow-y:auto;",
      # TOOL TIP?
      includeMarkdown("text_upload.rmd"),
      hr(),
      p("Upload Data"),
      fileInput(inputId="file1",
                label=NULL,
                multiple = FALSE,
                accept = c("text/plain",
                           ".rwl",
                           ".raw",
                           ".csv",
                           ".txt")),
      checkboxInput(inputId="useDemoData",
                    label="Use example data",
                    value=TRUE),
      uiOutput("series2useUI"),
      hr(),
      h4("Output"),
      actionButton(inputId = "genWav", label = "Generate and Play"),
      downloadButton("saveWav", "Save"),
      # minScaledFreq - lowest kept frequency in Hz
      # maxScaledFreq - highest kept frequency in Hz
      sliderTextInput(inputId = "freqRange",
                      label = "Output Freq Range (Hz)",
                      choices = octaves,
                      selected = octaves[c(4,7)]),
      sliderTextInput(inputId = "tSec",
                      label = "Length in Seconds",
                      choices = seq(5,60,by=5),
                      selected = 5)
    ),
    mainPanel(
      #width = 9,
      tags$div(id = "AUDIO_MY"),

      h1("treeSong"),
      includeMarkdown("text_intro.rmd"),
      hr(),
      fluidRow(
        plotOutput("datPlotTimeSeries")
      ),
      hr(),
      fluidRow(
        column(12,align="center",

               # m=10 - number of "notes"
               sliderTextInput(inputId = "m",
                               label = "Complexity",
                               choices = seq(1,50,by=1),
                               selected = 10,
                               hide_min_max = TRUE)
        )
      ),
      fluidRow(
        column(12, align="center",
               plotOutput("datPlotSin")
        ),
        hr(),
        materialSwitch(inputId = "showSpec", label = "Show spectrum", value = FALSE, status = "primary"),
        # start toggle
        conditionalPanel(
          condition = "input.showSpec",
        fluidRow(
          column(12,
                 plotOutput("datPlotSpec")
          )
        ),
        fluidRow(
          column(2),
          column(2,
                 checkboxInput(inputId = "logx",label = "Log x",value = FALSE)
          ),
          column(2,
                 checkboxInput(inputId = "logy",label = "Log y",value = TRUE)
          )
        )
        )# end toggle
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
    if (input$useDemoData) {
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
  ## Uncheck demo data if file is uploaded

  observeEvent(input$file1,{
    updateCheckboxInput(session,"useDemoData",value=FALSE)
  }, ignoreInit = TRUE)

  ##############################################################
  #
  # Server logic for loading and describing the input data
  #
  ##############################################################

  output$series2useUI <- renderUI({
    req(getData())
    selectInput(inputId = "series2use", label = "Select Series",
                choices = RVs$theNames,
                selected = RVs$theNames[2])
  })

  output$datPlotTimeSeries <- renderPlot({
    req(getData())
    req(getSeries())
    tmp <- data.frame(x=RVs$timeVec,y=RVs$theSeries)
    tmp <- tmp %>% drop_na()
    # do spec here so we have it later. should just make a reactive
    seriesSpec <- spec.pgram(tmp[,2], taper=0, plot=FALSE)
    seriesSpec <- data.frame(x=seriesSpec$freq,y=seriesSpec$spec)
    # make a RV so we can use it in another plot
    RVs$seriesSpec <- seriesSpec
    pTS <- ggplot(tmp,aes(x,y)) +
      geom_line(color="grey80") +
      labs(x="",y="") +
      theme_void()
    return(pTS)
  },bg="transparent")

  output$datPlotSpec <- renderPlot({
    req(getData())
    req(getSeries())
    seriesSpec <- RVs$seriesSpec
    specThresh <- seriesSpec$y[order(seriesSpec$y,decreasing = TRUE)[input$m+1]]
    seriesSpec$keepers <- ifelse(seriesSpec$y>specThresh,TRUE,FALSE)
    # make these the colors in the sin wav as segs instead of blue, make light blue grey
    seriesSpec0 <- seriesSpec %>% filter(keepers == FALSE)
    seriesSpec1 <- seriesSpec %>% filter(keepers == TRUE)
    seriesSpec1$idx <- paste0(1:nrow(seriesSpec1),"Hz")

    # plot - periodogram
    pSpec <- ggplot() +
      geom_segment(data=seriesSpec0,aes(x,xend=x,y=y,yend=0),
                   color="grey80") +
      geom_hline(yintercept = specThresh,linetype="dashed",color="white") +
      geom_segment(data=seriesSpec1,aes(x,xend=x,y=y,yend=0,color=idx),size=1) +
      annotate("text",
               x = Inf, #mean(seriesSpec$x),
               y = specThresh,
               label = "Complexity Threshold",
               family="serif",
               vjust = -0.5, hjust = 1, color="white") +
      labs(x="Frequency",y="Spectral Density") +
      theme_cowplot() +
      theme(legend.position = "none",
            axis.title = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            axis.ticks = element_line(color = "white"),
            axis.line = element_line(color = "white"))


    if(input$logx){
      pSpec <- pSpec + scale_x_continuous(trans="log")
    }
    if(input$logy){
      pSpec <- pSpec + scale_y_continuous(trans="log")
    }

    return(pSpec)
  },bg="transparent")

  output$datPlotSin <- renderPlot({
    seriesSpec <- RVs$seriesSpec
    # plot three - sine waves
    minObsFreq <- min(seriesSpec$x)
    maxObsFreq  <- max(seriesSpec$y)
    minScaledFreq <- input$freqRange[1]
    maxScaledFreq <- input$freqRange[2]
    xScaled <- (seriesSpec$x - minObsFreq) /
      (maxObsFreq - minObsFreq) *
      (maxScaledFreq - minScaledFreq) + minScaledFreq

    # get m biggest peaks
    indicies <- order(seriesSpec$y,decreasing = TRUE)[1:input$m]

    # make into wave -- vectorize this?
    # do low first
    idm <- indicies[1]
    tm <- seq(0,1/xScaled[idm], length.out = 44100/xScaled[idm])
    outSine <- matrix(0,ncol = input$m, nrow=length(tm))

    for(i in 1:input$m){
      idm <- indicies[i]
      outSine[,i] <-  seriesSpec$y[idm]*sin(2*pi*xScaled[idm]*tm)
    }
    colnames(outSine) <- paste0(round(xScaled[indicies],3),"Hz")
    sins2plot <- as_tibble(outSine) %>% add_column(tm) %>%
      pivot_longer(-tm)

    pSin <- ggplot(sins2plot) +
      geom_hline(yintercept = 0,color="white") +
      geom_line(aes(x=tm,y=value,color=name)) +
      geom_ribbon(aes(x=tm,ymax=value,ymin=0,fill=name),alpha=0.2) +
      scale_x_continuous(expand=c(0,0)) +
      theme_void() +
      theme(legend.position = "none")

    return(pSin)
  },bg="transparent")

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