#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Initializing libraries, more than we need but I forget what each one contains and figure this is easier
if (!require("shiny")) {
  install.packages("shiny")
  library("shiny")
}
if (!require("plyr")) {
  install.packages("plyr")
  library("plyr")
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library("dplyr")
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library("tidyverse")
}
if (!require("readxl")) {
  install.packages("readxl")
  library("readxl")
}
if (!require("classInt")) {
  install.packages("classInt")
  library("classInt")
}
if (!require("RCurl")) {
  install.packages("RCurl")
  library("RCurl")
}
if (!require("grid")) {
  install.packages("grid")
  library("grid")
}
if (!require("gridExtra")) {
  install.packages("gridExtra")
  library("gridExtra")
}
if (!require("lubridate")) {
  install.packages("lubridate")
  library("lubridate")
}
if (!require("skimr")) {
  install.packages("skimr")
  library("skimr")
}
if (!require("purrr")) {
  install.packages("purrr")
  library("purrr")
}
if (!require("data.table")) {
  install.packages("data.table")
  library("data.table")
}
if (!require("janitor")) {
  install.packages("janitor")
  library("janitor")
}
if (!require("shinyFiles")) {
  install.packages("shinyFiles")
  library("shinyFiles")
}
if (!require("gt")) {
  install.packages("gt")
  library("gt")
}

## Here are some functions that the app will need to run over and over.


# Takes an input filename and a list of the columns to extract -> returns the dataset, filtered for objects with no nucleus 
pullColumns<-function(filename,os){
  
  if(os=="osx"){
    dataset<-read.csv(filename,header=TRUE,skip = 9,fileEncoding = "ISO8859-7")
  }else{dataset<-read.csv(filename,header=TRUE,skip = 9)}
  
  filteredDataset<-dataset%>% dplyr::select(c('Count..1st.','Count..2nd.','Area..1st.integration.','Area..2nd.integration.','Area..target.area.')) %>% dplyr::filter(Count..1st.>0)
  
  return(filteredDataset)
}

# This function returns three measurements from the .csv: bacteria/nucleus, bacterial area/infected cell area, nuclear area/cell area. 
## I'll need to work on making this more flexible for if we want to do anything with intensity or whatever.
analyzeCSV<-function(filename,os){
  print(paste0("Analyzing ",filename))
  
  File<-basename(filename)%>%substr(1,nchar(filename)-4)
  
  
  dataset<-pullColumns(filename,os)
  filteredForInfected<-dataset%>%dplyr::filter(Count..2nd.>0)
  
  BactPerNuc<-sum(dataset$Count..2nd.)/sum(dataset$Count..1st.)
  BactAreaPerCellArea<-sum(filteredForInfected$Area..2nd.integration.)/sum(filteredForInfected$Area..target.area.)
  NucAreaPerCellArea<-sum(dataset$Area..1st.integration.)/sum(dataset$Area..target.area.)
  
  outputDataset<-data.frame(Sample=File,BactPerNuc=BactPerNuc,BactAreaPerCellArea=BactAreaPerCellArea,NucAreaPerCellArea=NucAreaPerCellArea)
  
  
  return(outputDataset)
}

## A function I stole from R-bloggers.com which spits out the OS a user is on
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}


## Initializes a dataframe which will be the main export of the tool. Right now it's relatively rigid and only spits out three simple analysis points for each .csv analyzed.
combinedAnalyzedData<-data.frame(File=as.character(),
                                 BactPerNuc=as.numeric(),
                                 BactAreaPerCellArea=as.numeric(),
                                 NucAreaPerCellArea=as.numeric())
#names(combinedAnalyzedData)<-c("Sample","Bacteria/Nucleus","Avg Bacteria Area/Cell Area","Avg Nucleus Area/Cell Area")





# Define UI for application that extracts data from input .csv files
ui <- bootstrapPage(
  
  # Application title
  titlePanel("Keyence Cell Count Data Analyzer"),
  
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    position="left",
    sidebarPanel(
      h5("If using Windows, select a folder here:"),
      shinyDirButton('WinDir',
                     'Select a folder',
                     'Upload'),
      br(),
      br(),
      h5("If using iOS/Mac, select a folder here:"),
      shinyDirButton('MacDir',
                     'Select a folder',
                     'Upload'),
    ),
    
    
    
    # Basic instructions at top of page
    
    mainPanel(
      h3("Instructions"),
      p("This Shiny app is designed to help quickly analyze the .csv files that you get after using the Hybrid Cell Counter tool in the Keyence Image analysis program. It assumes a few things:"),
      p("    - The selected folder includes only .csv files that you want to analyze"),
      p("    - You first counted the nuclei, and then counted the bacteria/particle/whatever"),
      p("    - You wish to do a pretty basic analysis (nothing totally crazy)"),
      p("Right now, it'll spit out a table with some summary data from each .csv in the input folder. Included right now are the following: 1) #bacteria/#nuclei, 2) total bacteria area/total infected cell area (filters out uninfected cells!!!), and 3) total nucleus area/total cell area (no filter! Looks at all cells!) "),
      p("I'm still working on flexibility for other analyses, so we'll see if there are updates.... For now, local versions of this app work just fine as long as you use the appropriate directory input button."),
      br(),
      br(),
      h3("Data Preview for input directory:"),
      verbatimTextOutput("directory",placeholder = TRUE),
      downloadButton("downloadData", "Download"),
      tableOutput("folderContents"),
    )
  )
)

server <- function(input, output,session) {
  shinyDirChoose(
    input,
    'WinDir',
    roots = c(wd = 'C:'),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )
  shinyDirChoose(
    input,
    'MacDir',
    roots = c(home = '~'),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )
  
  
  global<-reactiveValues(os=get_os())
  global <- reactiveValues(datapath = getwd())
  global<- reactiveValues(filelist=list.files(getwd()))
  global<-reactiveValues(combinedAnalyzedData=data.frame(File=as.character(),
                                                         BactPerNuc=as.numeric(),
                                                         BactAreaPerCellArea=as.numeric(),
                                                         NucAreaPerCellArea=as.numeric()))
  global<-reactiveValues(os=get_os())
  
  dir <- reactive(if(global$os=="osx"){
    return(input$MacDir)
  }
  else{return(input$WinDir)
  }
  )
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$WinDir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath(".")
                 global$datapath <-paste0("C:",unlist(map(input$WinDir,str_flatten,collapse="/")))
                 setwd(global$datapath)
                 global$filelist<-list.files(getwd())
                 print(getwd())
               })
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$MacDir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 global$datapath <-paste0("~",unlist(map(input$MacDir,str_flatten,collapse="/")))
                 setwd(global$datapath)
                 global$filelist<-list.files(getwd())
                 print(getwd())
               })
  
  output$directory <- renderText({
    
    return(global$datapath)
  })
  
  output$folderContents<-renderTable({
    
    #setwd(global$datapath)
    
    for(i in 1:length(global$filelist)){
      tryCatch({
        combinedAnalyzedData<-data.frame(rbind(combinedAnalyzedData,as.matrix(analyzeCSV(global$filelist[i],global$os))))
      },error=function(e){cat("ERROR :",conditionMessage(e),"\n")})
    }
    print("All .csvs analyzed!")
    global$combinedAnalyzedData<-combinedAnalyzedData
    return(global$combinedAnalyzedData)
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("AnalyzedKeyenceData.csv", sep = "")
    },
    content = function(file) {
      write.csv(global$combinedAnalyzedData, file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
