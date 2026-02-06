#Library Managment
  if(require("RSelenium") == FALSE){
    install.packages(RSelenium)
  }
  if (require("readxl") == FALSE ){
    install.packages(readxl)
  }
  if (require("readxl") == FALSE ){
    install.packages(readr)
  }
  library(RSelenium)
  library(readxl)
  library(readr)
################################################################################
  remDr <- remoteDriver( # Connects to Selenium Instance
              remoteServerAddr = "localhost",
              port = 4445L,
              browserName = "firefox"
              )
  

  remDr$open()
  
ss <- function(){
  remDr$screenshot(display = TRUE)
}
element_exists <- function(remDr, using, value) {
  tryCatch({
    elem <- remDr$findElement(using = using, value = value)
    return(!is.null(elem))
  }, error = function(e) {
    return(FALSE)  # Element not found
  })
} 
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
    if(State == "Florida"){
      for(i in 1:length(files)){
        file.rename(from = paste("./Processing","/report",i,".csv",sep=""), to = paste("./Processing","/report",i,".tsv",sep=""))
      }
      CompiledReport <- read_tsv("./Processing/report1.tsv")
      file.remove("./Processing/report1.tsv")
      if(length(files)>2){
        for(i in 2:length(files)){
          CompiledReport <-  rbind(CompiledReport, read_tsv(paste("./Processing/report",i,".tsv",sep="")))
          file.remove(paste("./Processing/report",i,".tsv", sep = ""))
        }
      }
    }
    else{
    CompiledReport <- read.csv("./Processing/report1.csv")
    file.remove("./Processing/report1.csv")
    if(length(files)>2){
    for(i in 2:length(files)){
      CompiledReport <-  rbind(CompiledReport,read.csv(paste("./Processing/report",i,".csv", sep = "")))
      file.remove(paste("./Processing/report",i,".csv", sep = ""))
    }
    }
    }
    write.csv(CompiledReport,paste("./",State,"/",State,Year,".csv", sep = ""), na="")
  }
################################################################################
  Connecticut<- function(Year){
    for(i in Year){
      link <- paste("https://seec.ct.gov/ecrisreporting/Data/eCrisDownloads/exportdatafiles/Receipts",i,"ElectionYearCandidateExploratoryCommittees.csv",sep="")
      remDr$navigate(link)
      ClearTmp()
      CompileAndPort("Connecticut",i)
    }
  }
  Georgia <- function(Year){
    for(i in Year){
      link <- paste("https://media.ethics.ga.gov/search/Campaign/Campaign_ByContributionsearchresults.aspx?Contributor=&Zip=&City=&ContTypeID=0&PAC=&Employer=&Occupation=&From=1/1/",i,"&To=12/31/",i,"&Cash=&InK=&Filer=&Candidate=&Committee=",sep="")
      remDr$navigate(link)
      remDr$findElement("css","#ctl00_ContentPlaceHolder1_Export")$clickElement()
      ClearTmp()
      CompileAndPort("Georgia",i)
    }
  }
  Florida <- function(Year){
    GetPage <- function(GetYear, Limit, Export){
      remDr$navigate("https://dos.elections.myflorida.com/campaign-finance/contributions/#criteria")
      Sys.sleep(1)
      remDr$findElement("css","#rightContent > form > select:nth-child(5)")$sendKeysToElement(list("A", "\uE007"))
      remDr$findElement("css","#rightContent > form > div.dateRange > div > input:nth-child(1)")$sendKeysToElement(list(paste("1/1/",GetYear, sep = "")))
      remDr$findElement("css","#rightContent > form > div.dateRange > div > input:nth-child(2)")$sendKeysToElement(list(paste("12/31/",GetYear, sep = "")))
      remDr$findElement("css","#rightContent > form > div:nth-child(61) > input")$clearElement()
      remDr$findElement("css","#rightContent > form > div:nth-child(61) > input")$sendKeysToElement(list(as.character(Limit)))
      if(Export == TRUE){
        remDr$findElement("css","#rightContent > form > ul:nth-child(67) > li:nth-child(2) > input[type=radio]")$clickElement()
      }
      remDr$findElement("css","#rightContent > form > div:nth-child(68) > input[type=submit]:nth-child(2)")$clickElement()
    }
    GetLength <- function(SampleYear){
      UpperBound <- 99999
      LowerBound <- 0
      SampleLimit <- 50000
      while((UpperBound-LowerBound) >1){
        GetPage(SampleYear,SampleLimit, FALSE)
        Sys.sleep(1)
        if(element_exists(remDr,"css","body > pre > a > address")){
          UpperBound <- SampleLimit
          SampleLimit <- round((UpperBound+LowerBound)/2,0)
        }
        else{
          LowerBound <- SampleLimit
          SampleLimit <- round((UpperBound+LowerBound)/2,0)
        }
        print(paste(UpperBound,LowerBound,SampleLimit))
      }
    return(LowerBound)
    }
    for(i in Year){
      Records <- GetLength(i)
      print(Records)
      GetPage(i,Records,TRUE)
      ClearTmp()
      CompileAndPort("Florida",i)
    }
  }
  Illinois <- function(Year){
    for(i in Year){
      for(j in 1:12){
        remDr$navigate("https://www.elections.il.gov/CampaignDisclosure/ContributionSearchByAllContributions.aspx")
        Sys.sleep(2)
        if(j == 1){
          remDr$findElement("css"," #ContentPlaceHolder1_txtRcvDate")$sendKeysToElement(list(paste("1/1/",i, sep = "")))
          remDr$findElement("css"," #ContentPlaceHolder1_txtRcvDateThru")$sendKeysToElement(list(paste(j+1,"/1/",i, sep = ""),key = "enter"))
        } else if(j == 12){
          remDr$findElement("css"," #ContentPlaceHolder1_txtRcvDate")$sendKeysToElement(list(paste(j,"/2/",i, sep = "")))
          remDr$findElement("css"," #ContentPlaceHolder1_txtRcvDateThru")$sendKeysToElement(list(paste(j,"/31/",i, sep = ""),key = "enter"))
        } else{
          remDr$findElement("css"," #ContentPlaceHolder1_txtRcvDate")$sendKeysToElement(list(paste(j,"/2/",i, sep = "")))
          remDr$findElement("css"," #ContentPlaceHolder1_txtRcvDateThru")$sendKeysToElement(list(paste(j+1,"/1/",i, sep = ""),key = "enter"))
        }
        Sys.sleep(2)
        remDr$findElement("css","#ContentPlaceHolder1_lnkDownloadList")$clickElement()
        Sys.sleep(2)
        remDr$findElement("css","#ContentPlaceHolder1_btnCSV")$clickElement()
        Sys.sleep(2)
      }
     ClearTmp()
     CompileAndPort("Illinois",i)
    }
  }
  Kansas <- function(Year){
    for(i in Year){
      remDr$navigate("https://sos.ks.gov/elections/cfr_viewer/cfr_examiner_contribution.aspx")
      remDr$findElement("css","#txtStartDate")$sendKeysToElement(list(paste("1/1/",Year),key="enter"))
      remDr$findElement("css","#txtEndDate")$sendKeysToElement(list(paste("12/31/",Year),key="enter"))
      remDr$findElement("css","#btnSubmit.smallbutton")$clickElement()
      remDr$findElement("css","#btnExport")$clickElement()
      ClearTmp()
      CompileAndPort("Kansas",i)
    }
  }
  Kentucky <- function(Year){
    for(i in Year){
    link <- paste("https://secure.kentucky.gov/kref/publicsearch/AllContributors?PageIndex=0&FirstName=&LastName=&FromOrganizationName=&ElectionDate=01%2F01%2F0001&City=&State=&Zip=&Employer=&Occupation=&OtherOccupation=&MinAmount=&MaxAmount=&MinimalDate=",i,"-01-01&MaximalDate=",i,"-12-31&ContributionMode=&ContributionSearchType=All&PageSize=10&PageIndex=0&ReportId=",sep = "")
    remDr$navigate(link)
    Sys.sleep(2)
    remDr$findElement("css","#main-wrapper > div > div > div:nth-child(2) > a > i")$clickElement()
    ClearTmp()
    CompileAndPort("Kentucky",i)
    }
  }
  Louisiana <- function(Year){
    for(i in Year){
      remDr$navigate("https://www.ethics.la.gov/CampaignFinanceSearch/SearchEfilingContributors.aspx")
      remDr$findElement("css","#ctl00_ContentPlaceHolder1_DateFromRadDateInput")$sendKeysToElement(list(paste("1/1/",i)))
      remDr$findElement("css","#ctl00_ContentPlaceHolder1_DateToRadDateInput")$sendKeysToElement(list(paste("12/31/",i)))
      remDr$findElement("css","#ctl00_ContentPlaceHolder1_PerformSearchLinkButton")$clickElement()
      Sys.sleep(2)
      remDr$findElement("css","#ctl00_ContentPlaceHolder1_ExportToCSVLinkButton")$clickElement()
      ClearTmp()
      CompileAndPort("Louisiana",i)
    }
  }
  
  Maine <- function(Year){
    remDr$navigate("https://mainecampaignfinance.com/#/dataDownload")
    remDr$findElement("css","#girdrow > tbody > tr > td.page-selector > div > select")$sendKeysToElement(list("\uE015","\uE015","\uE007"))
    for(i in Year){
      remDr$findElement("css",paste("#page-content-wrapper > div > div.table-responsive > table > tbody > tr:nth-child(",((2*(18-(i-2008)))+1),") > td:nth-child(3) > a",sep=""))$clickElement()
      Sys.sleep(2)
      ClearTmp()
      CompileAndPort("Maine",i)
    }
  }
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

 
remDr$close()
  
