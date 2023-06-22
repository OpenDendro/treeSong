#list of packages required
list.of.packages <- c("shiny","shinyWidgets","tidyverse","tuneR","cowplot","markdown","shinythemes","shinyjs","shinyBS","shinyjs","scales","gganimate","transformr", "gifski", "av", "shinycssloaders")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(tuneR)
library(cowplot)
library(markdown)
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(scales)
library(gganimate)
library(transformr)
library(gifski)
library(av)
library(shinycssloaders)

source("theme_oscilloscope.R")

# load data
demoDat <- read_csv("demoDatWide.csv")
octaves <- 27.5*2^(0:7)
sampRateAudio <- 4.41e4
downSampRate <- 2

addResourcePath(prefix = "www", directoryPath = "www")
addResourcePath(prefix = "videos", directoryPath = "www")

ui <- fluidPage(

  ##############################################################
  #
  # Theme and style
  #
  ##############################################################

  useShinyjs(),
  theme = shinytheme("superhero"),

  tags$head(
    tags$div(id = "AUDIO_MY"),
    tags$style(
      # css to change the position of the progress bar
      # and then to modify  gifModal
      HTML(".shiny-notification {
           height: 100px;
           width: 200px;
           position:fixed;
           top: calc(50% - 300px);
           left: calc(50% - 100px);
           text-align: center;
           }
           #mp4Modal .modal-footer { display:none }
           #mp4Modal .modal-sm { width: 440px; }"
      )
    )
  ),

  ##############################################################
  #
  # Sidebar
  #
  ##############################################################

  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      #style = "position:fixed;width:22%;height:95%;overflow-y:auto;",
      width=3,
      div(style="display: inline-block;",tags$h4("Input")),
      actionButton(inputId = "uploadHelp", label = NULL,icon=icon("circle-info"),style = "padding: 0"),

      fileInput(inputId="file1",
                label=NULL,
                multiple = FALSE,
                accept = c("text/plain",
                           ".csv",
                           ".txt"),
                buttonLabel = "Browse",
                placeholder = "Choose File"),

      checkboxInput(inputId="useDemoData",
                    label="Or use example data",
                    value=TRUE),
      uiOutput("series2useUI"),
      hr(),
      h4("Output"),
      hr(),
      h5("Audio"),
      fluidRow(
        column(6,
               actionButton(inputId = "genWavFile", label = "Play",
                            icon=icon("play"))
        ),
        column(6,
               hidden(downloadButton(outputId = "saveWav",
                                     label = "Save",
                                     icon=icon("file-audio")))
        )
      ),
      hr(),
      div(style="display: inline-block;",tags$h5("Animation")),
      actionButton(inputId = "animHelp", label = NULL,icon=icon("circle-info"),style = "padding: 0"),
      fluidRow(
        column(6,
               actionButton(inputId = "genVideo",
                            label = "Play",
                            icon=icon("play"))
        ),
        column(6,
               hidden(downloadButton(outputId = "saveOsc",
                                     label = "Save",
                                     icon=icon("file-video")))
        )

      ),
      hr(),
      fluidRow(
        sliderTextInput(inputId = "tSec",
                        label = "Length in Seconds",
                        choices = seq(1,60,by=1),
                        selected = 2,
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

    ##############################################################
    #
    # Main panel
    #
    ##############################################################

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
      ),# end toggle

      ##############################################################
      #
      # Modals: Still in main panel even though some modals
      #         are in sidepanel? I think this is where they
      #         appear.
      #
      ##############################################################

      bsModal(id = "aboutModal",
              title = "About treeSong",
              trigger = "about",
              size = "medium",
              includeMarkdown("text_intro.rmd")),

      bsModal(id = "uploadModal",
              title = "File Format",
              trigger = "uploadHelp",
              size = "medium",
              includeMarkdown("text_upload.rmd")),

      bsModal(id = "animAboutModal",
              title = "About Animation",
              trigger = "animHelp",
              size = "medium",
              p("Generating an animation takes some time. The complexity and length of the animation, as well as the processing power of the server can influence the duration")),

      bsModal(id = "mp4Modal",
              title="Oscilloscope",
              trigger = "genVideo",
              size = "small",
              shinycssloaders::withSpinner(uiOutput("playVideo")))
    ) # end main panel
  )
)


##############################################################
#
# Server
#
##############################################################

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


  getWave <- reactive({
    req(getSpec())
    req(input$m) # this is from a renderUI so make sure it is here.
    seriesSpec <- RVs$seriesSpec

    tm <- seq(from = 0, to = input$tSec, by = 1/sampRateAudio)

    # get m biggest peak
    indicies <- order(seriesSpec$y,decreasing = TRUE)[1:input$m]
    # make into wave
    idm <- indicies[1]
    sinMat <- matrix(0,ncol = input$m, nrow=length(tm))

    for(i in 1:input$m){
      idm <- indicies[i]
      sinMat[,i] <-  seriesSpec$y[idm]*sin(2*pi*seriesSpec$xScaled[idm]*tm)
    }
    colnames(sinMat) <- paste0(round(seriesSpec$xScaled[indicies],3),"Hz")

    # audio
    theWave <- rowSums(sinMat)
    theWave <- Wave(left = theWave, right= theWave,
                    samp.rate = sampRateAudio, bit=16)
    theWave <- normalize(object = theWave, unit = "16") # 16 bit

    RVs$theWave <- theWave
    writeWave(theWave, filename="www/out.wav", extensible = TRUE)

    # get the all the sin waves for plotting
    n <- length(tm)
    idx <- 1:n
    # the lowest freq
    f1 <- seriesSpec$xScaled[indicies[1]]

    # how many observations does it take to complete ONE cycle of f1
    # between 201 and 202 1/f1 < tm[201] 1/f1 < tm[202]
    # the number of obs it takes to complete one cycle of f1
    nObs_per_f1_cycle <- 1/f1 * sampRateAudio

    # how many times we will complete a f1 cycle
    nTimes_f1_cycles <- n/nObs_per_f1_cycle

    # idx cycle transition
    idx_f1_cycle_switch <- nObs_per_f1_cycle * 1:nTimes_f1_cycles
    cycleIdx <- findInterval(idx,idx_f1_cycle_switch) + 1

    # we want these things in the df relating to time
    # 1. the tm vector -- tm
    # 2. the inidivual cycle: first, second, third to nTimes_f1_cycles
    tmp <- data.frame(tm=tm,
                      cycleIdx)
    # 3. relative time in each cycle off by 1?
    tmp <- tmp %>% group_by(cycleIdx) %>%
      mutate(tmCycle = seq(1,n())) %>%
      ungroup()

    # merge with waves used for the audio
    outSine <- tmp %>% bind_cols(as_tibble(sinMat))

    RVs$outSine <- outSine

  })

  getVideo <- reactive({
    # run spec and wav with current inputs, yes?
    cat("yo\n")
    req(getWave())

    file.remove("www/osc.gif")
    file.remove("www/osc.mp4")

    outSine <- RVs$outSine
    # downsample for speed
    outSine <- outSine[seq(1,nrow(outSine),by=downSampRate),]

    nCycles <- max(outSine$cycleIdx[is.finite(outSine$cycleIdx)],na.rm=TRUE)

    sins2plot <- outSine %>%
      pivot_longer(-c(tm,cycleIdx,tmCycle)) %>%
      rename(x=tmCycle,y=value) %>%
      mutate(x=rescale(x, to=c(-1, 1)),
             y=rescale(y, to=c(-0.8, 0.8)))

    # make the plot bnd
    bndBox <- data.frame(x=c(-Inf,-Inf,Inf,Inf, -Inf),
                         y=c(-Inf,Inf,Inf,-Inf, -Inf),
                         id="bnd")

    # make the porthole mask
    hole <- data.frame(
      x = cos(seq(0, 2*pi, length.out = 360)),
      y = sin(seq(0, 2*pi, length.out = 360)),
      id="circle"
    )

    porthole <- rbind(bndBox,hole)

    # make plot
    p <- ggplot(sins2plot,aes(x=x,y=y,color=name)) +
      geom_line() + coord_fixed() +
      geom_polygon(data=porthole,
                   aes(x = x, y = y), fill = "black",
                   inherit.aes = FALSE) +
      geom_polygon(data=hole,
                   aes(x = x, y = y), color = "grey90",fill=NA,
                   inherit.aes = FALSE) +
      theme_oscilloscope()

    # add transition
    pAnim <- p +  transition_time(cycleIdx)

    # animate
    # note progress enhancement requested. https://github.com/thomasp85/gganimate/pull/331
    # but not in current gganimate pacakge so don't bother yet.

    theAmin <- animate(
      plot = pAnim,
      fps = 10,
      duration=input$tSec,
      height = 400,
      width = 400,
      units = "px"
    )


    anim_save(filename = "www/osc.gif",animation = theAmin)

    # then do mp
    av_encode_video(
      input = "www/osc.gif",
      output = "www/osc.mp4",
      framerate = 10,
      vfilter = "null",
      codec = NULL,
      audio = "www/out.wav",
      verbose = TRUE
    )

    # start_time <- Sys.time()
    #
    # end_time <- Sys.time()
    # print(end_time - start_time)

  })

  ##############################################################
  #
  # Observations
  #
  ##############################################################


  # Uncheck demo data if file is uploaded
  observeEvent(input$file1,{
    updateCheckboxInput(session,"useDemoData",value=FALSE)
  }, ignoreInit = TRUE)

  # fetch audio and make a UI when button is pushed
  observeEvent(input$genWavFile, {
    getWave()

    # show save button after the file is generated
    # if somebody changes the complexity, for instance, this
    # will be out of date though.

    show("saveWav")

    insertUI(selector = "#AUDIO_MY",
             where = "afterEnd",
             ui = tags$audio(src = "www/out.wav", type = "audio/wav",
                             autoplay = NA, controls = NA,
                             style="display:none;"), # can comment out to have player appear
             # the ui is NOT displayed ATM
             immediate = TRUE)
  })

  # don't show download buttons until wave and osc are made
  observeEvent(input$genVideo, {
    show("saveOsc")
  })

  ##############################################################
  #
  # UIs
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
                    selected = 5,
                    hide_min_max = TRUE,
                    grid = FALSE)
  })

  output$playVideo <- renderUI({
    req(getVideo())
    tags$video(src = "www/osc.mp4", type = "video/mp4",
               autoplay = TRUE, controls = TRUE)
  })

  ##############################################################
  #
  # Plots
  #
  ##############################################################


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
                                        y=y,yend=0,color=idx),linewidth=1) +
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
    req(getWave())
    outSine <- RVs$outSine
    # downsample for speed
    outSine <- outSine[seq(1,nrow(outSine),by=downSampRate),]

    sins2plot <- outSine %>%
      filter(cycleIdx<3) %>%
      select(-c("tmCycle","cycleIdx")) %>%
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




  ##############################################################
  #
  # File handling
  #
  ##############################################################

  output$saveWav <- downloadHandler(
    #req(getWave()),
    filename = function() {
      paste("out", "wav", sep=".")
    },
    content = function(file) {
      writeWave(RVs$theWave, filename=file, extensible = TRUE)
    }
  )

  output$saveOsc <- downloadHandler(
    filename = function() {
      paste("oscilloscope", "mp4", sep=".")
    },
    content = function(file) {
      file.copy(from = "www/osc.mp4",file,overwrite = TRUE)
    }, contentType = "video/mp4"
  )

}

shinyApp(ui, server)