library(shiny)
library(shinyWidgets)
library(tidyverse)
library(tuneR)
library(cowplot)
library(markdown)
library(shinythemes)
library(shinyjs)
library(shinyalert)

# load data
demoDat <- read_csv("demoDatWide.csv")
octaves <- 27.5*2^(0:7)

addResourcePath("www", "www")


ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("superhero"),
  tags$div(id = "AUDIO_MY"),

  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      #style = "position:fixed;width:22%;height:95%;overflow-y:auto;",
      width=3,
      h4("Input"),
      fileInput(inputId="file1",
                label=NULL,
                multiple = FALSE,
                accept = c("text/plain",
                           ".csv",
                           ".txt"),
                buttonLabel = "Browse",
                placeholder = "Choose File"),
      actionButton(inputId = "uploadHelp", label = "Upoad Data",
                   icon=icon("upload")),
      checkboxInput(inputId="useDemoData",
                    label="Or use example data",
                    value=TRUE),
      uiOutput("series2useUI"),
      hr(),
      h4("Output"),
      fluidRow(
        column(6,actionButton(inputId = "genWav", label = "Play",
                              icon=icon("play"))
        ),
        column(6,
               downloadButton("saveWav", "Save")
        ),
        sliderTextInput(inputId = "tSec",
                        label = "Length in Seconds",
                        choices = seq(5,60,by=5),
                        selected = 5,
                        hide_min_max = TRUE)
      ),
      hr(),
      h4("Control"),
      fluidRow(
        uiOutput("mUI"),
        # doesn't work with the dynamic UI
        #          tags$script(HTML("
        #   $(document).ready(function() {setTimeout(function() {
        #     supElement = document.getElementById('mUI').parentElement;
        #     $(supElement).find('span.irs-max, span.irs-min, span.irs-single, span.irs-from, span.irs-to').remove();
        #   }, 50);})
        # "))
      ),

      # minScaledFreq - lowest kept frequency in Hz
      # maxScaledFreq - highest kept frequency in Hz
      sliderTextInput(inputId = "freqRange",
                      label = "Output Freq Range (Hz)",
                      choices = octaves,
                      selected = octaves[c(4,7)],
                      hide_min_max = TRUE),


      materialSwitch(inputId = "showSpec",
                     label = "Show spectrum",
                     value = FALSE, status = "primary")
    ),
    mainPanel(
      width = 9,
      h1("treeSong"),
      actionButton(inputId = "about", label = "About",
                   icon = icon("circle-info")),
      hr(),
      fluidRow(
        plotOutput("datPlotTimeSeries",height = "200px") # height?
      ),
      hr(),
      conditionalPanel(
        condition = "!input.showSpec",
        fluidRow(
          column(12, align="center",
                 plotOutput("datPlotSin")
          )
        )
      ),
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
                 checkboxInput(inputId = "logx",label = "Log x",value = TRUE)
          ),
          column(2,
                 checkboxInput(inputId = "logy",label = "Log y",value = TRUE)
          )
        )
      )# end toggle
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

  getSpec <- reactive({
    req(getSeries())
    tmp <- data.frame(y=RVs$theSeries) %>% drop_na()

    seriesSpec <- spec.pgram(tmp[,1], taper=0, plot=FALSE)
    seriesSpec <- data.frame(x=seriesSpec$freq,y=seriesSpec$spec)
    # add rescaled freqs here. no reason not to
    minObsFreq <- min(seriesSpec$x)
    maxObsFreq  <- max(seriesSpec$x)
    minScaledFreq <- input$freqRange[1]
    maxScaledFreq <- input$freqRange[2]

    # scale from 0 to 1
    tmp <- (seriesSpec$x - minObsFreq) / (maxObsFreq - minObsFreq)

    seriesSpec$xScaled <-  tmp * (maxScaledFreq - minScaledFreq) + minScaledFreq

    # make a RV
    RVs$seriesSpec <- seriesSpec
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
    selectInput(inputId = "series2use", label = "Series",
                choices = RVs$theNames,
                selected = RVs$theNames[1])
  })


  output$mUI <- renderUI({
    req(getSpec())
    # get up to 1/10th of total possible freqs
    maxFreqs <- floor(length(RVs$seriesSpec$x)/10)
    sliderTextInput(inputId = "m",
                    label = "Complexity",
                    choices = seq(1,maxFreqs,by=1),
                    selected = 10,
                    hide_min_max = TRUE,
                    grid = FALSE)
  })

  output$datPlotTimeSeries <- renderPlot({
    req(getData())
    req(getSeries())

    tmp <- data.frame(x=1:(length(RVs$theSeries)),
                      y=RVs$theSeries)
    tmp <- tmp %>% drop_na()

    pTS <- ggplot(tmp,aes(x,y)) +
      geom_line(color="grey80") +
      scale_x_continuous(expand=c(0,0)) +
      theme_void()
    return(pTS)
  },bg="transparent")

  output$datPlotSpec <- renderPlot({
    req(getSpec())
    seriesSpec <- RVs$seriesSpec
    specThresh <- seriesSpec$y[order(seriesSpec$y,decreasing = TRUE)[input$m+1]]
    seriesSpec$keepers <- ifelse(seriesSpec$y>specThresh,TRUE,FALSE)
    seriesSpec0 <- seriesSpec %>% filter(keepers == FALSE)
    seriesSpec1 <- seriesSpec %>% filter(keepers == TRUE)
    seriesSpec1$idx <- paste0(1:nrow(seriesSpec1),"Hz")

    # plot - periodogram
    pSpec <- ggplot() +
      geom_segment(data=seriesSpec0,aes(xScaled,xend=xScaled,
                                        y=y,yend=0),
                   color="grey80") +
      geom_hline(yintercept = specThresh,linetype="dashed",color="white") +
      geom_segment(data=seriesSpec1,aes(xScaled,xend=xScaled,
                                        y=y,yend=0,color=idx),size=1) +
      annotate("text",
               x = Inf, #mean(seriesSpec$x),
               y = specThresh,
               label = "Complexity Threshold",
               family="serif",
               vjust = -0.5, hjust = 1, color="white") +
      labs(x="Frequency (Hz)",y="Amplitude") +
      theme_cowplot() +
      theme(legend.position = "none",
            axis.title = element_text(color = "white"),
            axis.line = element_line(color = "white"),
            axis.text.x = element_text(color = "white"),
            axis.ticks.x = element_line(color = "white"),
            axis.text = element_blank(),
            axis.ticks = element_blank())


    if(input$logx){
      pSpec <- pSpec + scale_x_continuous(trans="log")
    }
    if(input$logy){
      pSpec <- pSpec + scale_y_continuous(trans="log")
    }

    return(pSpec)
  },bg="transparent")

  output$datPlotSin <- renderPlot({
    req(getSpec())
    req(input$m)
    seriesSpec <- RVs$seriesSpec

    # get m biggest peak
    indicies <- order(seriesSpec$y,decreasing = TRUE)[1:input$m]

    # make into wave
    idm <- indicies[1]
    tm <- seq(0,1/seriesSpec$xScaled[idm],
              length.out = 44100/seriesSpec$xScaled[idm])
    outSine <- matrix(0,ncol = input$m, nrow=length(tm))

    for(i in 1:input$m){
      idm <- indicies[i]
      outSine[,i] <-  seriesSpec$y[idm]*sin(2*pi*seriesSpec$xScaled[idm]*tm)
    }
    colnames(outSine) <- paste0(round(seriesSpec$xScaled[indicies],3),"Hz")
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
    req(getSpec())
    req(input$m)
    req(input$tSec)
    sampRate <- 4.41e4 # 44.1 kHz is std samp rate.
    seriesSpec <- RVs$seriesSpec
    xScaled <- seriesSpec$xScaled
    y <- seriesSpec$y
    # get m biggest peaks
    indicies <- order(y,decreasing = TRUE)[1:input$m]

    # make into wave -- vectorize this?
    tVec <- seq(from = 0, to = input$tSec, by = sampRate^-1)
    theWave <- rep(0,nrow = length(tVec))
    for(i in 1:input$m){
      idm <- indicies[i]
      theWave <- theWave + y[idm]*sin(2*pi*xScaled[idm]*tVec)
    }

    theWave <- Wave(left = theWave, right= theWave,
                    samp.rate = sampRate, bit=16)
    theWave <- normalize(object = theWave, unit = "16") # 16 bit

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


  observeEvent(input$uploadHelp, {
    showModal(modalDialog(
      title = "Format",
      includeMarkdown("text_upload.rmd"),
      easyClose = TRUE
    ))
  })

  observeEvent(input$about, {
    showModal(modalDialog(
      title = "treeSong",
      includeMarkdown("text_intro.rmd"),
      easyClose = TRUE
    ))
  })
}

shinyApp(ui, server)