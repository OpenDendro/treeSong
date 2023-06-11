# series - numeric
# m=10 - number of "notes"
# minScaledFreq - lowest kept frequency in Hz
# maxScaledFreq - highest kept frequency in Hz
# wavLength - length of output wav
series2sound <- function(series, m=10,
                         minScaledFreq = 440/2, # A3
                         maxScaledFreq = 440*4, # A6
                         wavLength = 10){
  series <- na.omit(series)
  # Make a time vector in seconds at sampRate
  sampRate <- 4.41e4 # 44.1 kHz is std samp rate.
  tSecs <- seq(from = 0, to = wavLength, by = sampRate^-1)
  # get spec -- could do with fft but this is fine
  seriesSpec <- spec.pgram(series, taper=0, plot=FALSE)
  x <- seriesSpec$freq
  y <- seriesSpec$spec

  minObsFreq <- min(x)
  maxObsFreq  <- max(x)
  xScaled <- (x - minObsFreq) /
    (maxObsFreq - minObsFreq) *
    (maxScaledFreq - minScaledFreq) + minScaledFreq

  # get m biggest peaks
  indicies <- order(y,decreasing = TRUE)[1:m]

  # make into wave -- vectorize this?
  outWave <- rep(0,nrow = length(tSecs))
  for(i in 1:m){
    idm <- indicies[i]
    outWave <- outWave + y[idm]*sin(2*pi*xScaled[idm]*tSecs)
  }
  outWave <- Wave(left = outWave, right= outWave,
                    samp.rate = sampRate, bit=16)
  outWave <- normalize(object = outWave, unit = "16") # 16 bit

  return(outWave)
}
