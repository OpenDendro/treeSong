library(shiny)
library(shinyWidgets)
library(tidyverse)
library(tuneR)
require(cowplot)
source("series2sound.R")

# load data
demoDat <- read_csv("demoData.csv")
demoDat <- demoDat %>%
  pivot_wider(names_from = Site, values_from = Growth)

octaves <- 27.5*2^(0:8)

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
      checkboxInput(inputId="useDemoData",
                    label="Use example data",
                    value=TRUE),
      uiOutput("series2useUI"),
    ),
    mainPanel(
      tags$div(id = "AUDIO_MY"),

      h1("treeSong"),
      p("This is treeSong, a shiny app for listening to tree-ring data. Using either a preloaded dataset or by uploading data, you can listen to to a time series in the frequency domain by converting a filtered spectral density function into a waveform in the range of human hearing. You can select the complexity of the waveform (the number of tones to include), the range of pitches following the a low A on a piano to the highest A (A0 to A8), and indicate how long the wave form should play. You can also download an audio file (wav). This app is in a super early stage."),
      hr(),
      fluidRow(
        plotOutput("datPlotSeries")
      ),
      fluidRow(
        column(2,
               checkboxInput(inputId = "logx",label = "Log x",value = FALSE)
        ),
        column(2,
               checkboxInput(inputId = "logy",label = "Log y",value = TRUE)
        )
      ),
      hr(),
      fluidRow(
        # m=10 - number of "notes"
        column(6,
               sliderInput(inputId = "m", label = "Complexity Threshold",
                           min = 1,max = 50,value = 10)
        ),
        # minScaledFreq - lowest kept frequency in Hz
        # maxScaledFreq - highest kept frequency in Hz
        column(6,
               # sliderInput(inputId = "freqRange",
               #             label = "Output Freq Range (Hz)",
               #             min = 55, max = 6600,
               #             value = c(220,1760),
               #             step = 110)
               sliderTextInput(inputId = "freqRange",
                               label = "Output Freq Range (Hz)",
                               choices = octaves,
                               selected = octaves[c(4,7)])

        )
      ),
      hr(),
      fluidRow(
        column(12, align="center",
               plotOutput("datPlotSin")
        )
      ),
      fluidRow(
        numericInput(inputId = "tSec",
                     label = "Length in Seconds",
                     min = 1,
                     max = 60,
                     step=1,
                     value = 5)
      ),
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


  output$datPlotSeries <- renderPlot({
    req(getData())
    req(getSeries())
    tmp <- data.frame(x=RVs$timeVec,y=RVs$theSeries)
    tmp <- tmp %>% drop_na()
    seriesSpec <- spec.pgram(tmp[,2], taper=0, plot=FALSE)
    seriesSpec <- data.frame(x=seriesSpec$freq,y=seriesSpec$spec)
    # make a RV so we can use it in another plot
    RVs$seriesSpec <- seriesSpec
    specThresh <- seriesSpec$y[order(seriesSpec$y,decreasing = TRUE)[input$m+1]]
    seriesSpec$keepers <- ifelse(seriesSpec$y>specThresh,TRUE,FALSE)
    # make these the colors in the sin wav as segs instead of blue, make light blue grey
    seriesSpec0 <- seriesSpec %>% filter(keepers == FALSE)
    seriesSpec1 <- seriesSpec %>% filter(keepers == TRUE)
    seriesSpec1$idx <- paste0(1:nrow(seriesSpec1),"Hz")
    # plot one -- TS
    pTS <- ggplot(tmp,aes(x,y)) +
      geom_line(color="grey80") +
      labs(x="",y="") +
      theme_cowplot()

    # plot two - periodogram -- old
    pSpecOld <- ggplot(seriesSpec,aes(x,xend=x,y=y,yend=0,color=keepers)) +
      geom_segment() +
      geom_hline(yintercept = specThresh,linetype="dashed",color="blue") +
      annotate("text",
               x = Inf, #mean(seriesSpec$x),
               y = specThresh,
               label = "Complexity Threshold",
               family="serif",
               vjust = -0.5, hjust = 1, color="blue") +
      scale_color_manual(values=c("lightblue","blue")) +
      labs(x="Frequency",y="Spectral Density") +
      theme_cowplot() +
      theme(legend.position = "none")

    # plot two - periodogram -- change
    pSpec <- ggplot() +
      geom_segment(data=seriesSpec0,aes(x,xend=x,y=y,yend=0),
                   color="grey50",alpha=0.1) +
      geom_hline(yintercept = specThresh,linetype="dashed",color="blue") +
      geom_segment(data=seriesSpec1,aes(x,xend=x,y=y,yend=0,color=idx),size=1) +
      annotate("text",
               x = Inf, #mean(seriesSpec$x),
               y = specThresh,
               label = "Complexity Threshold",
               family="serif",
               vjust = -0.5, hjust = 1, color="blue") +
      labs(x="Frequency",y="Spectral Density") +
      theme_cowplot() +
      theme(legend.position = "none")

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

    pSin <- ggplot(sins2plot,aes(x=tm,y=value,color=name)) +
      geom_hline(yintercept = 0) +
      geom_line() +
      theme_cowplot() +
      labs(x="Time (sec)",y="Amplitude",
           subtitle = "Out Sound") +
      theme(legend.position = "none",
            #axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())

    pSin
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