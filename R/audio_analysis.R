library(listenR)
library(ggplot2)
library(pracma)

# Prototype BBB
bbb1 <- Audiofile$new("data/20190226_CLAY001/18-21.074-18-38.698.wav")
audiofile1 <- Audiofile$new("data/20190318_CERS001/20190318_CERS001_48k.wav")
sn1 <- audiofile1$snippet(units="seconds",from=9*60+17,to=9*60+18)


within <- function(x,y,d) {
  abs(x-y) < d
}

nearlyWhole <- function(x,tol) {
  g <- x %% 1
  (g <= tol || 1 - g <= tol)
}
  


harmonic <- function(f,f0,tol) {
  nearlyWhole(f/f0, 0.2)
}


reportBuzz <- function(af,k) {
  minBin <- floor(af$freq2spectrumBin(200))
  maxBin <- floor(af$freq2spectrumBin(2000))+1
  z <- unlist(af$spectrogram[k,minBin:maxBin],use.names = FALSE)
  peeks <- findpeaks(z,threshold=mean(z)+2*std(z))
  #print(peeks)
  freqs <- af$spectrumBin2Freq(peeks[,2]+minBin)
  #print(freqs)
  output <- FALSE
  if(length(freqs) > 0) {
    f0 <- freqs[[1]]
    if (within(f0,280,80)) {
      output <- TRUE
      if (length(freqs) > 1) {
        for (i in 2:length(freqs)) {
          if (!harmonic(freqs[[i]],f0,0.2)) {
            output <- FALSE
          }
        }
      }
    }
  }
  return(output)  
}


post_process <- function(candidates)
{
  # filter out isolated examples
  sapply(1:length(candidates), function(i) {
    i > 1 && i < (length(candidates)-1) && candidates[[i-1]] && candidates[[i]] && candidates[[i+1]]
  })

  # output <- vector(mode="logical",length=length(candidates))
  # for (i in 2:(length(candidates)-1)) {
  #   if (candidates[[i-1]] && candidates[[i]] && candidates[[i+1]])
  #     output[[i]] <- TRUE
  #   else
  #     output[[i]] <- FALSE
  # }
  
}



######## CRUFT #########
hb1 <- Audiofile$new("data/20190210_CLAY001/30-13.601-30-23.392.wav")
#buzz2 <- Audiofile$new("data/20190210_CLAY001/23-24.079-23-29.741.wav")
buzzpollen <- Audiofile$new("data/buzzpollination.wav")





