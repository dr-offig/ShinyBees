# ShinyBees
Shiny App for annotating audio recordings with spectrograms

## INSTALL
To install into RStudio

### 1. Clone this repository via the command line (for example using the RStudio terminal)  
```console
$ git clone https://github.com/dr-offig/ShinyBees.git  
```

### 2. In an RStudio console type  
```console
> install.packages("devtools")  
> install.packages("shiny")  
> install.packages(c("DT", "hms", "shinyjs"))  
> devtools::install_github("dr-offig/listenR")  
> devtools::install_github("dr-offig/videoR")
> devtools::install_github("dr-offig/spectR")
```
  
### 3. Setup at least one media directory in `/path/to/data`  
The media directory needs to have the following structure:  
```console
path/to/data/medialabel/  
    |-- medialabel.mp4
    |-- medialabel.wav
    |-- medialabel.csv
    |-- medialabel_log.csv
```  


### 4. Optionally setup a spectrogram movie (which is really just a series of .gif frames)
```console
FIXME: document how to setup spectrogram
```

### 5. Run the app from the R console
```console
shiny::runApp("/path/to/app/dir",host=0.0.0.0,port=5608)
```

### 6. Visit the app in a browser at
```console
127.0.0.1:5608
```
