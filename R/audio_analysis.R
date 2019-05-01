library(listenR)
library(hms)
#library(ggplot2)
#library(pracma)

# Prototype BBB
#bbb1 <- Audiofile$new("data/20190226_CLAY001/18-21.074-18-38.698.wav")
#audiofile1 <- Audiofile$new("data/20190318_CERS001/20190318_CERS001_48k.wav")
#sn1 <- audiofile1$snippet(units="seconds",from=9*60+17,to=9*60+18)


# Structure of database
# annotations <- data.frame("AudiorecordID"=character(),
#                           "Part"=integer(),
#                           "Parts"=integer(),
#                           "filename"=character(),
#                           "time"=numeric(),
#                           "comment"=character(),
#                           stringsAsFactors = FALSE)



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


mainIdentifier <- function(filepath) {
  tokens <- unlist(strsplit(filepath,"[/]"))
  bits <- unlist(strsplit(tokens[[length(tokens)]],"[.]"))
  paste0(bits[1:(length(bits)-1)],collapse=".")
}


### testing ###
ar <- Audiorecord$new(filenames=list("www/data/20190318_CERS001/20190318_CERS001_48k.wav"))
af1 <- ar$audiofiles[[1]]
af2 <- ar$audiofiles[[2]]
bbb <- read.csv('www/data/20190318_CERS001/20190318_CERS001.csv',header=TRUE,stringsAsFactors=FALSE)
bbb$time <- as.numeric(as.hms(bbb$time))
ind1 <- bbb$time < ar$start_times[[2]]
ind2 <- bbb$time >= ar$start_times[[2]]
bbt1 <- bbb$time[ind1]
bbc1 <- bbb$comment[ind1]
bbt2 <- bbb$time[ind2]
bbc2 <- bbb$comment[ind2]
crossover <- max(which(ind1))

##### Crank out Spectrograms #####
renderSpectrogram <- function(index) { 
  if (index > crossover) {
    marked_times <- bbt2
    descs <- bbc2
    audio_file <- af2
    k <- index - crossover
  } else {
    marked_times <- bbt1
    audio_file <- af1
    descs <- bbc1
    k <- index
  }

  from <- marked_times[[k]]-0.25
  to <- from + 0.25
  gp <- audio_file$plot.spectrogram(n=1024, h=512, units="seconds", from=from, to=to, minFreq=150, maxFreq=2000, contrast=0.5)   
  desc <- if(descs[[k]] == "BBB") { "BBB" } else { "NotBBB" }
  fname <- gsub(pattern = ":", 
                replacement = "-", 
                x = paste0(desc,"_", 
                           mainIdentifier(af1$filename), "_", 
                           as.character(as.hms(from)), "_to_",
                           as.character(as.hms(to)), 
                           ".png"))
  ggsave(filename=fname, plot=gp, width=6.4, height=3.6, path="/home/ybot/code/R/Bees/www/tmp/")
}


######## CRUFT #########
#hb1 <- Audiofile$new("data/20190210_CLAY001/30-13.601-30-23.392.wav")
#buzz2 <- Audiofile$new("data/20190210_CLAY001/23-24.079-23-29.741.wav")
#buzzpollen <- Audiofile$new("data/buzzpollination.wav")





