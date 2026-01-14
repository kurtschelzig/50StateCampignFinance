#Library Managment
  if(require("RSelenium") == FALSE){
    install.packages(RSelenium)
  }
  if (require("readxl") == FALSE ){
    install.packages()
  }
  library(RSelenium)
  library(readxl)
################################################################################
  remDr <- remoteDriver( # Connects to Selenium Instance
              remoteServerAddr = "localhost",
              port = 4445L,
              browserName = "firefox",
              extraCapabilities = FireFox
              )
  remDr$open()
  
################################################################################     
#Convert Docker Tmp folder .part files
  ClearTmp <- function() {
    files <- list.files( # Get all the files in tmp
                path = "./DockerHome/mozilla_mozillaUser0",
                include.dirs = FALSE,
                full.names = FALSE     
                )
    for(i in 1:length(files)){  # converts to CSV and moves them to Processing
      file.rename(from = paste("./DockerHome/mozilla_mozillaUser0","/",files[i], sep = ""), to = paste("./Processing","/report",i,".csv",sep=""))
    }
  }
################################################################################    
  CompileAndPort <- function(State,Year){
    CompiledReport <- NULL
    files <- list.files( # Get all the files in Processing
                path = "./Processing",
                include.dirs = FALSE,
                full.names = FALSE     
                )
    CompiledReport <- read.csv("./Processing/report1.csv")
    file.remove("./Processing/report1.csv")
    for(i in 2:length(files)){
      CompiledReport <-  rbind(CompiledReport,read.csv(paste("./Processing/report",i,".csv", sep = "")))
      file.remove(paste("./Processing/report",i,".csv", sep = ""))
    }
    write.csv(CompiledReport,paste("./",State,"/",State,Year,".csv", sep = ""))
  }
################################################################################  
  Tennessee <-function(Year){
    # Search Year with Settings
    SearchYear <- function(SampleYear){
      remDr$navigate("https://apps.tn.gov/tncamp/public/cesearch.htm")
      
      #Contribution/Expenditure, Set label:nth-child to 1 for Contribution, and 2 for expenditure
      remDr$findElement("css", "#frmContributions > div:nth-child(4) > div.seven.columns.control.inline-block > label:nth-child(1)")$clickElement()
      #Source
      remDr$findElement("css", "#frmContributions > div:nth-child(5) > div.seven.columns.control.inline-block > label:nth-child(3)")$clickElement()
      
      #From
      remDr$findElement("css", "#frmContributions > div:nth-child(6) > div > div.seven.columns.control.inline-block > label:nth-child(1)")$clickElement()  # Canidate
      remDr$findElement("css", "#frmContributions > div:nth-child(6) > div > div.seven.columns.control.inline-block > label:nth-child(2)")$clickElement()  # PACs
      remDr$findElement("css", "#frmContributions > div:nth-child(6) > div > div.seven.columns.control.inline-block > label:nth-child(3)")$clickElement()  # Private Individuals
      remDr$findElement("css", "#frmContributions > div:nth-child(6) > div > div.seven.columns.control.inline-block > label:nth-child(4)")$clickElement()  # Biusness/Organizations
      #Year
      remDr$findElement("css", "#yearSelection_chosen")$clickElement()
      remDr$findElement("css", "#yearSelection_chosen > div > div > input[type=text]")$sendKeysToElement(list(as.character(SampleYear),key = "enter"))
      #Feilds Of Data
      remDr$findElement("css", "div.six:nth-child(1) > label:nth-child(5)")$clickElement() #Election Year
      remDr$findElement("css", "div.six:nth-child(1) > label:nth-child(6)")$clickElement() #Report Name
      remDr$findElement("css", "#contAdrCheck")$clickElement() # Contributor Adress
      remDr$findElement("css", "#contOccCheck")$clickElement() # Contributor Employer
      remDr$findElement("css", "#contEmpCheck")$clickElement() # Desciption
      
      #Search
      remDr$findElement("css", "#descCheck")$clickElement()
      
      remDr$findElement("css", "#_continue")$clickElement()
    }
    SavePage <- function(){
      tryCatch({ remDr$findElement("css", "#content > form > div.exportlinks > a:nth-child(1)")$clickElement(); TRUE}, error = function(e){ remDr$refresh(); Sys.sleep(2);print("Try"); SavePage(); return(FALSE)})
    }
    NextPage <- function(){
      if(MorePages() == FALSE){
        return()
      }
      else{
      remDr$findElement("css", "p.center:nth-child(2) > a:nth-child(1)")$clickElement()
      }
    }
    MorePages <- function(){
      Test <- tryCatch({remDr$findElement("css", "p.center:nth-child(2) > a:nth-child(1)"); TRUE}, error = function(e){return(FALSE)})
      return(Test)
    } # Checks if it is the final Page or if there is More
    for(i in 1:length(Year)){
      SearchYear(Year[i])
      Sys.sleep(2)
      More <- TRUE
      count <- 1
      while(More == TRUE){
        print(count)
        SavePage()
        remDr$screenshot(display = TRUE )
        More <- MorePages()
        NextPage()
        Sys.sleep(2)
        count <- count +1
      }
    ClearTmp()
    CompileAndPort("Tennessee",Year[i])
  }
}
  
Tennessee(c(2024:2025))  
  
remDr$close()
  