#Library Managment
  if(require("RSelenium") == FALSE){
    install.packages(RSelenium)
  }
  if (require("readxl") == FALSE ){
    install.packages(readxl)
  }
  if (require("readr") == FALSE ){
    install.packages(readr)
  }
  if (require("rvest") == FALSE ){
    install.packages("rvest")
  }
  if (require("dplyr") == FALSE ){
    install.packages("dplyr")
  }
  library(RSelenium)
  library(readxl)
  library(readr)
  library(rvest)
  library(dplyr)
################################################################################
  firefox_caps <- list(
    browserName = "firefox",
    "moz:firefoxOptions" = list(
      prefs = list(
        "dom.disable_open_during_load" = FALSE,     # allow popups
        "privacy.popups.showBrowserMessage" = FALSE, # optional: suppress popup warning bar
        "javascript.enabled" = TRUE
      )
    )
  )
  
  remDr <- remoteDriver(
    remoteServerAddr = "localhost",
    port = 4445L,
    browserName = "firefox",
    extraCapabilities = firefox_caps
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
tryclick <- function(using,value){
  tryCatch({
    elem <- remDr$findElement(using = using, value = value)$clickElement()
    return(TRUE)
  }, error = function(e) {
    return(FALSE)  # Element not found
  })
} 


waitforLoad <- function(TimeOut = 10){
  if(is.null(TimeOut)){
    files <- length(list.files(path = "./DockerHome/mozilla_mozillaUser0"))
    while(files == length(list.files(path = "./DockerHome/mozilla_mozillaUser0"))){
      Sys.sleep(0.5)
    }
  } else if(is.integer(TimeOut)){
    counter <-1
    files <- length(list.files(path = "./DockerHome/mozilla_mozillaUser0"))
    while((files == length(list.files(path = "./DockerHome/mozilla_mozillaUser0")) &&  TimeOut < 4)){
      Sys.sleep(0.5)
      counter <- counter +1
    }
  }
  else{
    counter <-1
    files <- length(list.files(path = "./DockerHome/mozilla_mozillaUser0"))
    while((files == length(list.files(path = "./DockerHome/mozilla_mozillaUser0")) &&  counter < 4)){
      Sys.sleep(0.5)
      counter <- counter +1
    }
  }
}

################################################################################     
#Convert Docker Tmp folder .part files
  ClearTmp <- function() {
    files <- list.files(path = "./DockerHome/mozilla_mozillaUser0")
    for(i in 1:length(files)){  # converts to CSV and moves them to Processing
      file.rename(from = paste("./DockerHome/mozilla_mozillaUser0","/",files[i], sep = ""), to = paste("./Processing","/report",i,".csv",sep=""))
    }
  }
################################################################################    
  CompileAndPort <- function(State,Year){
    CompiledReport <- NULL
    files <- list.files(path = "./Processing")
    if(State == "Florida" | State == "Illinois"){
      for(i in 1:length(files)){
        file.rename(from = paste("./Processing","/report",i,".csv",sep=""), to = paste("./Processing","/report",i,".tsv",sep=""))
      }
      CompiledReport <- read_tsv("./Processing/report1.tsv")
      file.remove("./Processing/report1.tsv")
    }else if(State == "Ohio"){
      tmp <- readLines(paste("./Processing/report",2,".csv", sep = ""))
      tmp <- gsub('JIANG FOR ASSEMBLY LD 39 "','JIANG FOR ASSEMBLY LD 39 ',tmp)
      write.csv(tmp,paste("./Processing/report",2,".csv", sep = ""))
      CompiledReport <- read.csv("./Processing/report1.csv")
      file.remove("./Processing/report1.csv")
      if(length(files)>=2){
        for(i in 2:length(files)){
          print(i)
          hold <- read.csv(paste("./Processing/report",i,".csv", sep = ""))
          CompiledReport <-  rbind(CompiledReport,hold)
         file.remove(paste("./Processing/report",i,".csv", sep = ""))
        }
      }
    }
    else if(State == "Montana"){
      for(i in 1:length(files)){
        textdoc <- read_lines(paste("./Processing","/report",i,".txt",sep=""))
        textdoc <- iconv(textdoc, to ="ASCII//TRANSLIT")
        textdoc <- gsub(',',"-",textdoc)
        textdoc <- gsub('\\|',",",textdoc)
        write_lines(textdoc,paste("./Processing/report",i,".txt",sep=""))
        file.rename(from = paste("./Processing/report",i,".txt",sep=""), to = paste("./Processing","/report",i,".csv",sep=""))
      }
     CompiledReport <- read.csv("./Processing/report1.csv")
     file.remove("./Processing/report1.csv")
      if(length(files)>=2){
        for(i in 2:length(files)){
          print(i)
          hold <- read.csv(paste("./Processing/report",i,".csv", sep = ""), col.names = names(CompiledReport))
          CompiledReport <-  rbind(CompiledReport,hold)
         file.remove(paste("./Processing/report",i,".csv", sep = ""))
        }
      }
    }
    else if(State == "California"){
      CompiledReport <- read.csv("./Processing/report1.csv")
      CompiledReport <- CompiledReport[c(1:(length(CompiledReport[,1])-5)),]
      file.remove("./Processing/report1.csv")
    }
    else if(State == "Minnesota"){
      CompiledReport <- read.csv("./Processing/report1.csv")
      CompiledReport <- CompiledReport[which(CompiledReport$Year == Year),]
      file.remove("./Processing/report1.csv")
    }
    else{
      CompiledReport <- read.csv("./Processing/report1.csv")
      file.remove("./Processing/report1.csv")
      if(length(files)>=2){
        for(i in 2:length(files)){
          print(i)
          hold <- read.csv(paste("./Processing/report",i,".csv", sep = ""))
          CompiledReport <-  rbind(CompiledReport,hold)
         file.remove(paste("./Processing/report",i,".csv", sep = ""))
        }
      }
    }
    write.csv(CompiledReport,paste("./",State,"/",State,Year,".csv", sep = ""), na="")
  }
################################################################################
Alaska <- function(Year){
  export <- function(){
    remDr$findElement("css","#M_C_sCDTransactions_csfFilter_btnSearch")$clickElement()
    while(element_exists(remDr,"css","#M_C_sCDTransactions_grid_ctl00 > tbody > tr > td > div > div") == TRUE){
      Sys.sleep(0.5)
    }
    remDr$findElement("css","#M_C_sCDTransactions_csfFilter_btnExport")$clickElement()
    while(element_exists(remDr,"css","#M_C_sCDTransactions_csfFilter_ExportDialog_hlAllCSV") == FALSE){
      Sys.sleep(0.5)
    }
    remDr$findElement("css","#M_C_sCDTransactions_csfFilter_ExportDialog_hlAllCSV")$clickElement()
    waitforLoad(5)
  }
  for(i in Year){
    for(j in 1:12){
      remDr$navigate("https://aws.state.ak.us/apocreports/campaigndisclosure/CDIncome.aspx")
      remDr$findElement("css","#M_C_sCDTransactions_csfFilter_ddlReportYear")$clickElement()
      remDr$findElement("css","#M_C_sCDTransactions_csfFilter_ddlReportYear")$sendKeysToElement(list("A",key="enter"))
      if(j == 1){
        remDr$findElement("css","#M_C_sCDTransactions_csfFilter_txtBeginDate")$sendKeysToElement(list(paste("1/1/",i, sep = "")))
        remDr$findElement("css","#M_C_sCDTransactions_csfFilter_txtEndDate")$sendKeysToElement(list(paste(j+1,"/1/",i, sep = ""),key = "enter"))
      } else if(j == 12){
        remDr$findElement("css","#M_C_sCDTransactions_csfFilter_txtBeginDate")$sendKeysToElement(list(paste(j,"/2/",i, sep = "")))
        remDr$findElement("css","#M_C_sCDTransactions_csfFilter_txtEndDate")$sendKeysToElement(list(paste(j,"/31/",i, sep = ""),key = "enter"))
      } else{
        remDr$findElement("css","#M_C_sCDTransactions_csfFilter_txtBeginDate")$sendKeysToElement(list(paste(j,"/2/",i, sep = "")))
        remDr$findElement("css","#M_C_sCDTransactions_csfFilter_txtEndDate")$sendKeysToElement(list(paste(j+1,"/1/",i, sep = ""),key = "enter"))
      }
      export()
    }
    ClearTmp()
    CompileAndPort("Alaska",i)
  }
}
Alabama <- function(Year){
  for(i in Year){
    RangeURL <- paste("https://fcpa.alabamavotes.gov/page.request.do?page=com.acf.common.page.contributionsearchresults&pageNumber=1&pageSize=1&sortDirection=ASC&sortBy=contributor&criteria=%5B%7B%22field_key%22%3A%22beginningDate%22%2C%22comparison_type%22%3A%22after%22%2C%22comparison_value_1%22%3A%22",i,"-01-01%22%7D%2C%7B%22field_key%22%3A%22endDate%22%2C%22comparison_type%22%3A%22before%22%2C%22comparison_value_1%22%3A%22",i,"-12-31%22%7D%5D")
    remDr$navigate(RangeURL)
    RecCount <- strsplit(strsplit(as.character(read_html(remDr$getPageSource()[[1]]) %>% html_nodes("pre")),'totalRecords\":')[[1]][2],',')[[1]][1]
    RangeURL <- paste0("https://fcpa.alabamavotes.gov/page.request.do?page=com.acf.common.page.contributionsearchresults&pageNumber=1&pageSize=",RecCount,"&sortDirection=ASC&sortBy=contributor&criteria=%5B%7B%22field_key%22%3A%22beginningDate%22%2C%22comparison_type%22%3A%22after%22%2C%22comparison_value_1%22%3A%22",i,"-01-01%22%7D%2C%7B%22field_key%22%3A%22endDate%22%2C%22comparison_type%22%3A%22before%22%2C%22comparison_value_1%22%3A%22",i,"-12-31%22%7D%5D")
    remDr$navigate(RangeURL)
    remDr$findElement("css","#endDate")$sendKeysToElement(list(paste("12/31/2025",sep="")))
    strsplit(as.character(read_html(remDr$getPageSource()[[1]]) %>% html_nodes("pre")),"[")
  
  }
}
Arizona <- function(Year){
  for(i in Year){
    Final <- TRUE
    Page <- 1
    while(Final == TRUE){
      for(j in 1:100){
        remDr$navigate(paste("https://seethemoney.az.gov/Reporting/Explore#JurisdictionId=0|Page=7|startYear=",i,"|endYear=",i,"|IsLessActive=false|ShowOfficeHolder=false|View=Detail|TablePage=",Page,"|TableLength=100",sep=""))
        if(element_exists(remDr,"css",paste("#IndividualInformationData > tr:nth-child(",j,") > td.sorting_1 > a", sep = ""))){
          Sys.sleep(0.25)
          elem <- remDr$findElement("css",paste("#IndividualInformationData > tr:nth-child(",j,") > td.sorting_1 > a", sep = ""))
          
          html <- remDr$getPageSource()
          html <- read_html(paste("https://seethemoney.az.gov/Reporting/Explore#JurisdictionId=0|Page=7|startYear=",i,"|endYear=",i,"|IsLessActive=false|ShowOfficeHolder=false|View=Detail|TablePage=",Page,"|TableLength=100",sep="")) %>% html_elements("xpath","/html/body/div/div[64]/div/div[2]/div/table/tbody/tr[1]/td[2]")
          Sys.sleep(0.5)
          remDr$findElement("css","#ExportIndividualAmount")$clickElement()
          waitforLoad((100*page)+j)
        }else{
          Final <- FALSE
        }
      }
      Page <- Page +1
    }
    
  }
}


California <- function(Year){
  Start <- Sys.time()
  for( i in Year){
    remDr$navigate("https://powersearch.sos.ca.gov/advanced.php")
    remDr$findElement("css","#all_cands")$clickElement()
    remDr$findElement("css","#range_dates")$clickElement()
    remDr$findElement("css","#start_date")$clearElement()
    remDr$findElement("css","#end_date")$clearElement()
    remDr$findElement("css","#start_date")$sendKeysToElement(list(paste("1/1/",i, sep = "")))
    remDr$findElement("css","#end_date")$sendKeysToElement(list(paste("12/31/",i, sep = "")))
    remDr$findElement("css","#caps_search_btn2")$clickElement()
    remDr$findElement("css","#caps_field_btn")$clickElement()
    remDr$findElement("css","#caps_filter_box > div > a.download_csv")$clickElement()
    Sys.sleep(30)
    ClearTmp()
    CompileAndPort("California",i)
  }
  print(Sys.time()-Start)
}
  Colorado <- function(Year){
    clear <- function(){
      waitForDownload()
      remDr$findElement("css","#_ctl0_Content_txtContributionDateBegin")$clearElement()
      remDr$findElement("css","#_ctl0_Content_txtContributionDateEnd")$clearElement()
    }
    remDr$navigate("https://tracer.sos.colorado.gov/PublicSite/SearchPages/ContributionSearch.aspx")
    remDr$findElement("css","#_ctl0_Content_chkExportOnly")$clickElement()
    for(i in Year){
      for(j in 1:12){
        if(j == 1){
          for(k in 1:3){
            remDr$findElement("css","#_ctl0_Content_txtContributionDateBegin")$sendKeysToElement(list(paste("1/",1+(7*(k-1)),"/",i, sep = "")))
            remDr$findElement("css","#_ctl0_Content_txtContributionDateEnd")$sendKeysToElement(list(paste("1/",7*(k),"/",i, sep = ""),key = "enter"))
            clear()
          }
          remDr$findElement("css","#_ctl0_Content_txtContributionDateBegin")$sendKeysToElement(list(paste("1/22/",i, sep = "")))
          remDr$findElement("css","#_ctl0_Content_txtContributionDateEnd")$sendKeysToElement(list(paste(j+1,"/1/",i, sep = ""),key = "enter"))
          clear()
        } else if(j == 12){
          for(k in 1:3){
            remDr$findElement("css","#_ctl0_Content_txtContributionDateBegin")$sendKeysToElement(list(paste(j,"/",2+(7*(k-1)),"/",i, sep = "")))
            remDr$findElement("css","#_ctl0_Content_txtContributionDateEnd")$sendKeysToElement(list(paste(j,"/",1+(7*k),"/",i, sep = ""),key = "enter"))
            clear()
          }
          remDr$findElement("css","#_ctl0_Content_txtContributionDateBegin")$sendKeysToElement(list(paste(j,"/29/",i, sep = "")))
          remDr$findElement("css","#_ctl0_Content_txtContributionDateEnd")$sendKeysToElement(list(paste(j,"/31/",i, sep = ""),key = "enter"))
          clear()
        } else{
          for(k in 1:3){
          remDr$findElement("css","#_ctl0_Content_txtContributionDateBegin")$sendKeysToElement(list(paste(j,"/",2+(7*(k-1)),"/",i, sep = "")))
          remDr$findElement("css","#_ctl0_Content_txtContributionDateEnd")$sendKeysToElement(list(paste(j,"/",1+(7*k),"/",i, sep = ""),key = "enter"))
          clear()
          }
          remDr$findElement("css","#_ctl0_Content_txtContributionDateBegin")$sendKeysToElement(list(paste(j,"/27/",i, sep = "")))
          remDr$findElement("css","#_ctl0_Content_txtContributionDateEnd")$sendKeysToElement(list(paste(j+1,"/1/",i, sep = ""),key = "enter"))
          clear()
        }
      }
      ClearTmp()
      CompileAndPort("Colorado",i)
    }
  }
  Connecticut<- function(Year){
    Start <- Sys.time()
    for(i in Year){
      link <- paste("https://seec.ct.gov/ecrisreporting/Data/eCrisDownloads/exportdatafiles/Receipts",i,"ElectionYearCandidateExploratoryCommittees.csv",sep="")
      remDr$navigate(link)
      Sys.sleep(0.5)
      ClearTmp()
      CompileAndPort("Connecticut",i)
    }
    print(Sys.time()-Start)
  }
  Delaware <- function(Year){
    Start <- Sys.time()
    for(i in Year){
      remDr$navigate("https://cfrs.elections.delaware.gov/Public/ViewReceipts?theme=vista")
      remDr$findElement("css","#dtStartDate")$sendKeysToElement(list(paste("01/01/",i,sep="")))
      remDr$findElement("css","#dtEndDate")$sendKeysToElement(list(paste("12/31/",i,sep="")))
      remDr$findElement("css","#btnSearch")$clickElement()
      remDr$findElement("css","#export > img")$clickElement()
      Sys.sleep(3)
      ClearTmp()
      CompileAndPort("Delaware",i)
    }
    print(Sys.time() - Start)
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
      Sys.sleep(0.5)
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
      }
      print(LowerBound)
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
      print(i)
      for(j in 1:12){
        print(j)
        remDr$navigate("https://www.elections.il.gov/CampaignDisclosure/ContributionSearchByAllContributions.aspx")
        while(element_exists(remDr,"css","#ContentPlaceHolder1_txtRcvDate") == FALSE){
          Sys.sleep(0.5)
        }
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
        while(element_exists(remDr,"css","#ContentPlaceHolder1_lnkDownloadList") == FALSE){
          Sys.sleep(0.5)
        }
        Sys.sleep(0.1)
        remDr$findElement("css","#ContentPlaceHolder1_lnkDownloadList")$clickElement()
        while(element_exists(remDr,"css","#ContentPlaceHolder1_btnText") == FALSE){
          Sys.sleep(0.5)
        }
        ss()
        Sys.sleep(1)
        remDr$findElement("css","#ContentPlaceHolder1_btnText")$clickElement()
        waitforLoad(5)

      }
     ClearTmp()
     CompileAndPort("Illinois",i)
    }
  }
  Indiana <- function(Year){
    for(i in Year){
      remDr$navigate("https://campaignfinance.in.gov/PublicSite/SearchPages/ContributionSearch.aspx")
      remDr$findElement("css","#_ctl0_Content_chkExportOnly")$clickElement()
      StartDate <- as.Date(paste(i,"-01-01",sep=""))
      EndDate <- as.Date( paste(i,"-12-31",sep=""))
      Days <- seq(from = StartDate, to = EndDate, by = "day")
      c<-1
      for(j in 1:length(Days)){
        remDr$findElement("css","#_ctl0_Content_txtContributionDateBegin")$sendKeysToElement(list(Days[j]))
        remDr$findElement("css","#_ctl0_Content_txtContributionDateEnd")$sendKeysToElement(list(Days[j]))
        remDr$findElement("css","#_ctl0_Content_btnSearch")$clickElement()
        ss()
        waitforLoad(c)
        waitForDownload()
        remDr$findElement("css","#_ctl0_Content_txtContributionDateBegin")$clearElement()
        remDr$findElement("css","#_ctl0_Content_txtContributionDateEnd")$clearElement()
        c <- c+1
      }
      ClearTmp()
      CompileAndPort("Indiana",i)
    }
  }
  Kansas <- function(Year){
    for(i in Year){
      remDr$navigate("https://sos.ks.gov/elections/cfr_viewer/cfr_examiner_contribution.aspx")
      while(!element_exists(remDr,"css","#txtStartDate")){
        Sys.sleep(0.5)
      }
      remDr$findElement("css","#txtStartDate")$sendKeysToElement(list(paste("1/1/",Year),key="enter"))
      remDr$findElement("css","#txtEndDate")$sendKeysToElement(list(paste("12/31/",Year),key="enter"))
      Sys.sleep(1)
      remDr$findElement("css","#btnSubmit.smallbutton")$clickElement()
      while(!element_exists(remDr,"css","#btnExport")){
        Sys.sleep(0.5)
      }
      remDr$findElement("css","#btnExport")$clickElement()
      waitForDownload()
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
    waitForDownload()
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
      waitForDownload()
      ClearTmp()
      CompileAndPort("Louisiana",i)
    }
  }
  
  Maine <- function(Year){
    remDr$navigate("https://mainecampaignfinance.com/#/dataDownload")
    remDr$findElement("css","#girdrow > tbody > tr > td.page-selector > div > select")$sendKeysToElement(list("\uE015","\uE015","\uE007"))
    Sys.sleep(1)
    for(i in Year){
      remDr$findElement("css",paste("#page-content-wrapper > div > div.table-responsive > table > tbody > tr:nth-child(",((2*(18-(i-2008)))+1),") > td:nth-child(3) > a",sep=""))$clickElement()
      waitforLoad()
      waitForDownload()
      ClearTmp()
      CompileAndPort("Maine",i)
    }
  }
  Minnesota <- function(Year){
    remDr$navigate("https://cfb.mn.gov/reports-and-data/self-help/data-downloads/campaign-finance/")
    remDr$findElement("css","#main > section > table:nth-child(6) > tbody > tr:nth-child(2) > td:nth-child(3) > a")$clickElement()
    waitforLoad()
    Sys.sleep(5)
    waitForDownload()
    ClearTmp()
    CompileAndPort("Minnesota",Year)
  }
  Missouri <- function(Year){
    for(i in Year){
      remDr$navigate("https://www.mec.mo.gov/MEC/Campaign_Finance/CF12_ContrExpend.aspx#gsc.tab=0")
      remDr$findElement("css","#ContentPlaceHolder_ContentPlaceHolder1_ddYear")$clickElement()
      remDr$findElement("css","#ContentPlaceHolder_ContentPlaceHolder1_ddYear")$sendKeysToElement(list( as.character(i),key = "enter"))
      remDr$findElement("css","#ContentPlaceHolder_ContentPlaceHolder1_lbtnAdvanced")$clickElement()
      Sys.sleep(2)
      remDr$findElement("css","#ContentPlaceHolder_ContentPlaceHolder1_txtStartAmt")$clearElement()
      remDr$findElement("css","#ContentPlaceHolder_ContentPlaceHolder1_txtStartAmt")$sendKeysToElement(list("0.00"))
      remDr$findElement("css","#ContentPlaceHolder_ContentPlaceHolder1_btnSearch")$clickElement()
      Sys.sleep(2)
      Page1 <- remDr$getWindowHandles()[[1]]
      Page2 <- remDr$getWindowHandles()[[2]]
      
      remDr$switchToWindow(Page2)
      remDr$findElement("css","#ContentPlaceHolder_btnExport")$clickElement()
      remDr$closeWindow()
      remDr$switchToWindow(Page1)
      Sys.sleep(5)
      files <- list.files(path = "./DockerHome/mozilla_mozillaUser0",include.dirs = TRUE,full.names = TRUE)
      fileload <- file.size(files[1])
      Sys.sleep(1)
      while(file.size(files[1]) != fileload){
        fileload <- file.size(files[1])
        Sys.sleep(1)
      }
      file.rename(files[1],"./Processing/report1.txt")
      
      textdoc <- paste(read_lines("./Processing/report1.txt"),sep="")
      textdoc <- textdoc[5:length(textdoc)]
      textdoc <- gsub('\t\t\t<th scope=\"col\">',"",textdoc)
      textdoc <- gsub('</th><th scope=\"col\">',",",textdoc)
      textdoc<- gsub('\t\t</tr><tr style=\"color:#333333;background-color:#F7F6F3;\">',"",textdoc)
      textdoc<- gsub('\t\t</tr><tr style=\"color:#284775;background-color:White;\">',"",textdoc)
      textdoc <- gsub('\t\t\t<td>',"",textdoc)
      textdoc <- gsub('</td><td>',",",textdoc)
      textdoc <- gsub('</td>',"",textdoc)
      textdoc <- gsub('&nbsp;',"",textdoc)
      textdoc <- textdoc[which(textdoc != "")]
      write_lines(textdoc,"./Processing/report1.txt")
      file.rename("./Processing/report1.txt",paste("./Missouri/Missouri",i,".csv",sep=""))
    }
  }
  Montana <- function(Year){
    AllExport <- FALSE
    Page <- 0
    for(i in Year){
    while(AllExport == FALSE){
      remDr$navigate("https://cers-ext.mt.gov/CampaignTracker/public/search#")
      Sys.sleep(1)
      remDr$findElement("css","#searchTabId > li:nth-child(3) > a")$clickElement()
      remDr$findElement("css","#searchContributionsForm > div.row > div:nth-child(2) > div:nth-child(5) > div > div > div > div > button.btn.spinner-up.btn-xs.btn-info")$clickElement()
      for(k in 1:(as.integer(substr(Sys.Date(),1,4))-i)){
        remDr$findElement("css","#searchContributionsForm > div.row > div:nth-child(2) > div:nth-child(5) > div > div > div > div > button.btn.spinner-down.btn-xs.btn-info")$clickElement()
      }
      remDr$findElement("css","#searchContributionsButton")$clickElement()
      remDr$findElement("css","#financialCandidateResultsTable_length > label > select")$sendKeysToElement(list("\uE015","\uE015","\uE015","\uE015","\uE015","\uE007"))
      Sys.sleep(3)
      if(Page >0){
        for(k in 1:Page){
          remDr$findElement("xpath","/html/body/div[2]/div/div[2]/div[2]/div/div/div[4]/div/div[3]/div[2]/div[2]/div/div[2]/div[2]/div/ul/li[5]/a")$clickElement()
        }
      }
      for(j in 1:100){
        if(element_exists(remDr,"css",paste("#financialCandidateResultsTable > tbody > tr:nth-child(",j,") > td.center.sorting_1 > label > input", sep ="")) == FALSE){
          AllExport <- TRUE
          break
        }        else{
          print(j)
          webElem <- remDr$findElement("css",paste("#financialCandidateResultsTable > tbody > tr:nth-child(",j,") > td.center.sorting_1 > label > input", sep = ""))
          remDr$executeScript("arguments[0].click();",list(webElem))
        remDr$findElement("css","#downloadFinancialCandidateButton")$clickElement()
        ss()
        waitforLoad(TRUE)
        waitForDownload()
        }
      }
      Page <- Page +1
    }
      files <- list.files(path = "./DockerHome/mozilla_mozillaUser0")
        file.rename(from = paste("./DockerHome/mozilla_mozillaUser0","/",files, sep = ""), to = paste("./Processing","/report",1:length(files),".txt",sep=""))

      CompileAndPort("Montana",i)
    }
  }
  Nebraska <- function(Year){
    for(i in Year){
      remDr$navigate("https://nadc-e.nebraska.gov/PublicSite/SearchPages/Search.aspx?SearchTypeCodeHook=F0FEA582-08C3-42BA-B008-0F5067C5791B")
      remDr$findElement("xpath","/html/body/form/div[5]/table/tbody/tr[1]/td/table/tbody/tr[2]/td[2]/div/span/span[4]/span[2]/span[17]/input")$sendKeysToElement(list(paste("1/1/",i,sep="")))
      remDr$findElement("xpath","/html/body/form/div[5]/table/tbody/tr[1]/td/table/tbody/tr[2]/td[2]/div/span/span[4]/span[2]/span[18]/input")$sendKeysToElement(list(paste("06/30/",i,sep="")))
      remDr$findElement("css","#ctl00_Content_f0fea582-08c3-42ba-b008-0f5067c5791b_Contribution_SearchManager_Contribution_SearchButton > input")$clickElement()
      while(!element_exists(remDr,"xpath","/html/body/form/div[5]/table/tbody/tr[1]/td/table/tbody/tr[2]/td[2]/div/span/span[4]/span[6]/span[1]/input[1]")){
        Sys.sleep(0.25)
      }
      remDr$findElement("xpath","/html/body/form/div[5]/table/tbody/tr[1]/td/table/tbody/tr[2]/td[2]/div/span/span[4]/span[6]/span[1]/input[1]")$clickElement()
      waitforLoad(1)
      waitForDownload()
      remDr$findElement("xpath","/html/body/form/div[5]/table/tbody/tr[1]/td/table/tbody/tr[2]/td[2]/div/span/span[4]/span[2]/span[17]/input")$clearElement()
      remDr$findElement("xpath","/html/body/form/div[5]/table/tbody/tr[1]/td/table/tbody/tr[2]/td[2]/div/span/span[4]/span[2]/span[18]/input")$clearElement()
      remDr$findElement("xpath","/html/body/form/div[5]/table/tbody/tr[1]/td/table/tbody/tr[2]/td[2]/div/span/span[4]/span[2]/span[17]/input")$sendKeysToElement(list(paste("7/1/",i,sep="")))
      remDr$findElement("xpath","/html/body/form/div[5]/table/tbody/tr[1]/td/table/tbody/tr[2]/td[2]/div/span/span[4]/span[2]/span[18]/input")$sendKeysToElement(list(paste("12/31/",i,sep="")))
      remDr$findElement("css","#ctl00_Content_f0fea582-08c3-42ba-b008-0f5067c5791b_Contribution_SearchManager_Contribution_SearchButton > input")$clickElement()
      while(!element_exists(remDr,"xpath","/html/body/form/div[5]/table/tbody/tr[1]/td/table/tbody/tr[2]/td[2]/div/span/span[4]/span[6]/span[1]/input[1]")){
        Sys.sleep(0.25)
      }
      remDr$findElement("xpath","/html/body/form/div[5]/table/tbody/tr[1]/td/table/tbody/tr[2]/td[2]/div/span/span[4]/span[6]/span[1]/input[1]")$clickElement()
      waitforLoad(2)
      waitForDownload()
      ClearTmp()
      CompileAndPort("Nebraska",i)
    }
  }
  Nevada <- function(Year){
    remDr$navigate("https://www.nvsos.gov/SOSCandidateServices/AnonymousAccess/CEFDSearchUU/Search.aspx#contribution_search")
    for(i in Year){
      for(j in 1:12){
        if(j == 1){
          remDr$findElement("css","#ctl00_MainContent_txtRadContDateMin_dateInput")$sendKeysToElement(list(paste("1/1/",i, sep = "")))
          remDr$findElement("css","#ctl00_MainContent_txtRadContDateMax_dateInput")$sendKeysToElement(list(paste(j+1,"/1/",i, sep = ""),key = "enter"))
        } else if(j == 12){
          remDr$findElement("css","#ctl00_MainContent_txtRadContDateMin_dateInput")$sendKeysToElement(list(paste(j,"/2/",i, sep = "")))
          remDr$findElement("css","#_ctl0_Content_txtContributionDateEnd")$sendKeysToElement(list(paste(j,"/31/",i, sep = ""),key = "enter"))
        } else{
          remDr$findElement("css","#ctl00_MainContent_txtRadContDateMin_dateInput")$sendKeysToElement(list(paste(j,"/2/",i, sep = "")))
          remDr$findElement("css","#ctl00_MainContent_txtRadContDateMax_dateInput")$sendKeysToElement(list(paste(j+1,"/1/",i, sep = ""),key = "enter"))
        }
        
      }
    }
  }
  NewJersy <- function(Year){
    for(i in Year){
      for(k in 1:2){
        remDr$navigate("https://www.njelecefilesearch.com/SearchContributionByContributorAdvanced")
        
        remDr$findElement("css","#divYears > div:nth-child(2) > div > div.ef-text-right.btnspace > a")$clickElement()
        remDr$findElement("css",paste("#chk-year",i,sep=""))$clickElement()
        
        remDr$findElement("css","#divParties > div:nth-child(2) > div > div.ef-text-right.btnspace > a")$clickElement()
        for(j in 0:3){
          remDr$findElement("css",paste("#chk-party",(2*j)+k,sep=""))$clickElement()
        }
        if(k == 1){
          remDr$findElement("css",paste("#chk-party",99,sep=""))$clickElement()
        }else{remDr$findElement("css",paste("#chk-party",9,sep=""))$clickElement()}
        
        remDr$findElement("css","#btnSearch")$clickElement()
        remDr$findElement("css","#btnDownloadData")$clickElement()
        waitforLoad()
        waitForDownload()
      }
      ClearTmp()
      CompileAndPort("New Jersey",i)
    }
  }  
  NewMexico <- function(Year){
    for(i in Year){
      remDr$navigate("https://login.cfis.sos.state.nm.us/#/transaction/CON")
      remDr$findElement("css","#bs-sidebar-navbar-collapse-1 > ul > li:nth-child(6) > div > a")$clickElement()
      remDr$findElement("css","#id1 > div.md-container.md-ink-ripple")$clickElement()
      remDr$findElement("css","#id4 > div.md-container.md-ink-ripple")$clickElement()
      remDr$findElement("css","#id5 > div.md-container.md-ink-ripple")$clickElement()
      remDr$findElement("css","#addFilterModalForm > md-dialog-actions > button")$clickElement()
      remDr$findElement("xpath","/html/body/div[2]/div[2]/div/div[2]/div[1]/nav/div/div/ul/li[4]/md-input-container/md-datepicker/div/input")$sendKeysToElement(list(paste("1/1/",i, sep = "")))
      remDr$findElement("xpath","/html/body/div[2]/div[2]/div/div[2]/div[1]/nav/div/div/ul/li[5]/md-input-container/md-datepicker/div[1]/input")$sendKeysToElement(list(paste("12/31/",i, sep = ""),key = "enter"))
      
      remDr$findElement("xpath","/html/body/div[2]/div[2]/div/div[1]/div[2]/a/i")$clickElement()
      }
  }

  NorthCarolina <- function(Year){
    for(i in Year){
        remDr$navigate("https://cf.ncsbe.gov/CFTxnLkup/")
        remDr$findElement("css","#TransType > option:nth-child(2)")$clickElement()
        remDr$findElement("css","#DateFrom")$sendKeysToElement(list(paste("1/1/",i,sep="")))
        remDr$findElement("css","#DateTo")$sendKeysToElement(list(paste("12/31/",i,sep="")))
        remDr$findElement("css","#btnSearch")$clickElement()
        while(!element_exists(remDr,"css","#gridResults > tbody > tr:nth-child(1) > td:nth-child(13)")){
          Sys.sleep(1)
        }
        remDr$findElement("css","#btnExportResults")$clickElement()
        waitforLoad()
        Sys.sleep(40)
        ClearTmp()
        CompileAndPort("North Carolina",i)
    }
  }
  Ohio <- function(Year){
    dl <- function(){
        c <- length(list.files(path = "./DockerHome/mozilla_mozillaUser0"))
        remDr$findElement("css","#B4377232681591563962")$clickElement()
        Sys.sleep(1)
        remDr$findElement("css","#report_R4236090801753315998 > div > div.t-Report-links > a:nth-child(1)")$clickElement()
        ss()
        waitforLoad(c+1)
        waitForDownload()
        remDr$navigate("https://www6.ohiosos.gov/ords/f?p=CFDISCLOSURE:2:::NO:RP,2::")
        Sys.sleep(1)
    }
    for(i in Year){
      for(j in 1:12){
        remDr$navigate("https://www6.ohiosos.gov/ords/f?p=CFDISCLOSURE:2:::NO:RP,2::")
        Sys.sleep(1)
      if(j == 1){
        for(k in 1:14){
          remDr$findElement("css","#P2_START_DATE")$sendKeysToElement(list(paste("1/",1+(2*(k-1)),"/",i, sep = "")))
          remDr$findElement("css","#P2_END_DATE")$sendKeysToElement(list(paste("1/",2*k,"/",i, sep = ""),key = "enter"))
          dl()
        }
        remDr$findElement("css","#P2_START_DATE")$sendKeysToElement(list(paste("1/30/",i, sep = "")))
        remDr$findElement("css","#P2_END_DATE")$sendKeysToElement(list(paste("2/01/",i, sep = ""),key = "enter"))
        dl()
      } else if(j == 12){
          for(k in 1:15){
            remDr$findElement("css","#P2_START_DATE")$sendKeysToElement(list(paste(j,"/",2+((k-1)*2),"/",i, sep = "")))
            remDr$findElement("css","#P2_END_DATE")$sendKeysToElement(list(paste(j,"/",(k*2)+1,"/",i, sep = ""),key = "enter"))
            dl()
          }
      }  else if(j == 2){
        for(k in 1:13){
          remDr$findElement("css","#P2_START_DATE")$sendKeysToElement(list(paste(j,"/",2+(2*(k-1)),"/",i, sep = "")))
          remDr$findElement("css","#P2_END_DATE")$sendKeysToElement(list(paste(j,"/",(2*k)+1,"/",i, sep = ""),key = "enter"))
          dl()
        }
        remDr$findElement("css","#P2_START_DATE")$sendKeysToElement(list(paste(j,"/28/",i, sep = "")))
        remDr$findElement("css","#P2_END_DATE")$sendKeysToElement(list(paste(j+1,"/1/",i, sep = ""),key = "enter"))
        dl()
      } else{
          for(k in 1:14){
            remDr$findElement("css","#P2_START_DATE")$sendKeysToElement(list(paste(j,"/",2+((k-1)*2),"/",i, sep = "")))
            remDr$findElement("css","#P2_END_DATE")$sendKeysToElement(list(paste(j,"/",(k*2)+1,"/",i, sep = ""),key = "enter"))
            dl()
          }
            remDr$findElement("css","#P2_START_DATE")$sendKeysToElement(list(paste(j,"/30/",i, sep = "")))
            remDr$findElement("css","#P2_END_DATE")$sendKeysToElement(list(paste(j+1,"/01/",i, sep = ""),key = "enter"))
            dl()
          }
      }
      ClearTmp()
      CompileAndPort("Ohio",i)
    }
  }
  Oklahoma <- function(Year){
    for(i in Year){
      remDr$navigate("https://guardian.ok.gov/PublicSite/DataDownload.aspx")
      remDr$findElement("css",paste("#ctl00_Content_dlstDownloadFiles > tbody > tr:nth-child(",2+((as.integer(substr(Sys.Date(),1,4))-i)*3),") > td:nth-child(3) > a",sep=""))$clickElement()
      wait()
      file.rename(from = paste("./DockerHome/mozilla_mozillaUser0","/",files, sep = ""), to = "./Processing/report1.zip")
      unzip("./Processing/report1.zip",exdir = "./Processing",)
      file.remove( "./Processing/report1.zip")
      file.rename(paste("./Processing/",i,"_ContributionLoanExtract.csv",sep=""),"./Processing/report1.csv")
      CompileAndPort("Oklahoma",i)
    }
  }
  Oregon <- function(Year){
    nav <- function(){
      remDr$navigate("https://secure.sos.state.or.us/orestar/gotoPublicTransactionSearch.do")
      Sys.sleep(1)
      while(!element_exists(remDr,"css"," #cneSearchTranStartDate")){
        Sys.sleep(0.25)
      }
      remDr$findElement("css"," #cneSearchTranStartDate")$clearElement()
      remDr$findElement("css","#cneSearchTranEndDate")$clearElement()
      remDr$findElement("css","#cneSearchTranType")$clickElement()
      remDr$findElement("css","#cneSearchTranType")$sendKeysToElement(list("\uE015","\uE007"))
    }
    dl <- function(){
        ss()
        c <- length(list.files( path = "./DockerHome/mozilla_mozillaUser0"))
        Sys.sleep(5)
        remDr$findElement("css","#content > div > form > table:nth-child(10) > tbody > tr > td:nth-child(5) > input[type=button]:nth-child(1)")$clickElement()
        while(!element_exists(remDr,"css","#content > div > form > table:nth-child(6) > tbody > tr > td:nth-child(3) > a")){
          Sys.sleep(0.25)
        }
        remDr$findElement("css","#content > div > form > table:nth-child(6) > tbody > tr > td:nth-child(3) > a")$clickElement()
        waitforLoad(c+1)
        Sys.sleep(5)
        nav()

    }
    for( i in Year){
      nav()
      for(j in 1:12){
        if(j == 1){
              for(k in 1:14){
                remDr$findElement("css"," #cneSearchTranStartDate")$sendKeysToElement(list(paste("01/",1+(2*(k-1)),"/",i, sep = "")))
                remDr$findElement("css","#cneSearchTranEndDate")$sendKeysToElement(list(paste("01/",(2*k)+1,"/",i, sep = ""),key = "enter"))
                dl()
              }
              remDr$findElement("css"," #cneSearchTranStartDate")$sendKeysToElement(list(paste("1/30/",i, sep = "")))
              remDr$findElement("css","#cneSearchTranEndDate")$sendKeysToElement(list(paste("02/01/",i, sep = ""),key = "enter"))
              dl()
        } else if(j == 12){
            for(k in 1:15){
              remDr$findElement("css"," #cneSearchTranStartDate")$sendKeysToElement(list(paste("12/",2+(2*(k-1)),"/",i, sep = "")))
              remDr$findElement("css","#cneSearchTranEndDate")$sendKeysToElement(list(paste("12/",(2*k)+1,"/",i, sep = ""),key = "enter"))
              dl()
            }
        }else if(j == 2){
          for(k in 1:13){
            remDr$findElement("css"," #cneSearchTranStartDate")$sendKeysToElement(list(paste("02/",2+(2*(k-1)),"/",i, sep = "")))
            remDr$findElement("css","#cneSearchTranEndDate")$sendKeysToElement(list(paste("02/",(2*k)+1,"/",i, sep = ""),key = "enter"))
            dl()
          }
          remDr$findElement("css"," #cneSearchTranStartDate")$sendKeysToElement(list(paste("02/28/",i, sep = "")))
          remDr$findElement("css","#cneSearchTranEndDate")$sendKeysToElement(list(paste("03/1/",i, sep = ""),key = "enter"))
          dl()
        }else{
            for(k in 1:14){
              remDr$findElement("css","#cneSearchTranStartDate")$sendKeysToElement(list(paste(j,"/",2+(2*(k-1)),"/",i, sep = "")))
              remDr$findElement("css","#cneSearchTranEndDate")$sendKeysToElement(list(paste(j,"/",(2*k)+1,"/",i, sep = ""),key = "enter"))
              dl()
            }
          remDr$findElement("css","#cneSearchTranStartDate")$sendKeysToElement(list(paste(j,"/30/",i, sep = "")))
          remDr$findElement("css","#cneSearchTranEndDate")$sendKeysToElement(list(paste(j+1,"/01/",i, sep = ""),key = "enter"))
          dl()
        }
      }
    }
  }
  
  Pennsylvania <- function(Year){
    for(i in Year){
      files <- NULL
      remDr$navigate("https://www.pa.gov/agencies/dos/resources/voting-and-elections-resources/campaign-finance-data")
      remDr$findElement("css",paste("#container-f27885e8b2 > div > div.list.aem-GridColumn.aem-GridColumn--default--12 > div > div > div:nth-child(2) > div > ul > li:nth-child(",(2+((as.integer(substr(Sys.Date(),1,4))-i))),") > a > div.cmp-list__item--content > span",sep=""))$clickElement()
      wait()
      files <- list.files("./DockerHome/mozilla_mozillaUser0/")
      file.rename(from = paste("./DockerHome/mozilla_mozillaUser0/",files, sep = ""), to = "./Processing/report1.zip")
      Sys.sleep(1)
      unzip("./Processing/report1.zip",exdir = "./Processing/",)
      Sys.sleep(1)
      if(i <2025){
        files <- list.files( # Get all the files in tmp
          path = paste("./Processing/",i,sep=""),
          include.dirs = FALSE,
          full.names = FALSE     
        )
        for( j in files){
          file.rename(from = paste("./Processing/",i,"/",j,sep=""), to = paste("./Processing/",j,sep=""))
        }
        unlink(paste("./Processing/",i,sep=""), recursive = TRUE,force = TRUE)
      }
      Sys.sleep(1)
      file.remove( c("./Processing/report1.zip",paste("./Processing/debt_",i,".txt",sep=""),paste("./Processing/expense_",i,".txt",sep=""),paste("./Processing/filer_",i,".txt",sep=""),paste("./Processing/receipt_",i,".txt",sep="")))
      file.rename(paste("./Processing/contrib_",i,".txt",sep=""),"./Processing/report1.csv")
      CompileAndPort("Pennsylvania",i)
    }
  }
  RhodeIsland <- function(Year){
    chars <- c(letters, 0:9)
    for(i in Year){
      dump <- 0
      for(j in chars){
        remDr$navigate(paste("https://ricampaignfinance.com/RIPublic/Reporting/TransactionReport.aspx?OrgID=0&BeginDate=01/01/",i,"&EndDate=12/31/",i,"&LastName=",j,"&FirstName=&ContType=0&State=&City=&ZIPCode=&EmployerName=&Amount=0&ReportType=Contrib&CFStatus=F&MPFStatus=A&Level=S&SumBy=Type&Sort1=ReceiptDate&Direct1=desc&Sort2=None&Direct2=asc&Sort3=None&Direct3=asc&Site=Public&Incomplete=A&ContSource=CF",sep=""))
        if(element_exists(remDr,"css","#dgrReport > tbody > tr.GridItem") == TRUE){
           while(!element_exists(remDr,"css","#lnkExport")){
            Sys.sleep(0.25)
           }
           remDr$findElement("css","#lnkExport")$clickElement()
           while(length(remDr$getWindowHandles()) <2){
             Sys.sleep(0.25)
           }
           remDr$switchToWindow(remDr$getWindowHandles()[[2]][1])
           remDr$findElement("css","#hypFileDownload")$clickElement()
           waitforLoad(which(chars == j)-dump)
           waitForDownload()
           remDr$closeWindow()
           remDr$switchToWindow(remDr$getWindowHandles()[[1]][1])
        }else{
            print(paste("No Results:",j))
          dump <- dump+1
          }
      }
      ClearTmp()
      CompileAndPort("Rhode Island",i)
    }
  }
  
 # SouthDakota  IS A TOTALBLACKBOX
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
      tryCatch({ remDr$findElement("css", "#content > form > div.exportlinks > a:nth-child(1)")$clickElement(); TRUE}, error = function(e){ return(FALSE)})
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

      More <- TRUE
      count <- 1
      while(More == TRUE){
        print(count)
        SavedPage <- FALSE
        while(SavedPage == FALSE){
          SavedPage <- SavePage()
          Sys.sleep(1)
        }
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
  
  Texas <- function(Year){
    for(i in Year){
      remDr$navigate("https://www.ethics.state.tx.us/search/cf/AdvancedSearch.php")
      remDr$findElement("css","#transType")$clickElement()
      remDr$findElement("css","#transType")$sendKeysToElement(list("\uE015"))
      remDr$findElement("css","#MySelect")$clickElement()
      remDr$findElement("css","#MySelect")$sendKeysToElement(list("\uE015", "\uE015"))
      remDr$findElement("css","#MyDate")$clickElement()
      remDr$findElement("css","#MyDate")$sendKeysToElement(list("\uE015","\uE015",key = "enter"))
      remDr$findElement("css","#NameFirst")$sendKeysToElement(list("a"))
      for(j in 1:12){
          if(j == 1){
            remDr$findElement("css","#beginDate")$sendKeysToElement(list(paste(i,"-01-01", sep = "")))
            remDr$findElement("css"," #endDate")$sendKeysToElement(list(paste(i,"-02-01", sep = ""),key = "enter"))
          } else if(j == 12){
            remDr$findElement("css","#beginDate")$sendKeysToElement(list(paste(i,"-12-02", sep = "")))
            remDr$findElement("css"," #endDate")$sendKeysToElement(list(paste(i,"-12-31", sep = ""),key = "enter"))
          } else{
            remDr$findElement("css","#beginDate")$sendKeysToElement(list(paste(i,"-",j,"-02", sep = "")))
            remDr$findElement("css"," #endDate")$sendKeysToElement(list(paste(i,"-",j+1,"-01", sep = ""),key = "enter"))
          }
        remDr$findElement("css","#content > div.container > div.AdvSrch > div.submitDiv > input.submit-row.submit-pdf")$clickElement()
        while(element_exists(remDr,"css","#exportReport") == FALSE){
          Sys.sleep(1)
        }
        Button <- remDr$findElement("css","#exportReport")$clickElement()
      }
    
    
      Button <- remDr$findElement("xpath","/html/body/div[1]/table/tbody/tr/td[1]/button")$clickElement()
      remDr$executeScript("arguments[0].click();", list(Button))
      }
  }
  Utah <- function(Year){
    for(i in Year){
      remDr$navigate("https://disclosures.utah.gov/Search/AdvancedSearch")
      remDr$findElement("css","#EntityType")$clickElement()
      remDr$findElement("css","#EntityType")$sendKeysToElement(list("c",key="enter"))
      remDr$findElement("css","#ReportYear")$clickElement()
      remDr$findElement("css","#ReportYear")$sendKeysToElement(list(as.character(i),key="enter"))
      remDr$findElement("css","#searchBtn")$clickElement()
      Sys.sleep(1)
      check <- element_exists(remDr,"css","#searchResults > table > tfoot > tr > th > a:nth-child(10)")
      html <- remDr$getPageSource()[[1]]
      table <- (html %>% read_html() %>% html_table())[[1]]
      export <- table[3:(length(table$X1)-1),]
      count <- 1
      for(j in 1:9){
        remDr$findElement("css",paste("#searchResults > table > tfoot > tr > th > a:nth-child(",j,")",sep=""))$clickElement()
        Sys.sleep(0.2)
        html <- remDr$getPageSource()[[1]]
        table <- (html %>% read_html() %>% html_table())[[1]]
        export <- rbind(export,table[3:(length(table$X1)-1),])
        count <- count +1
      }
      remDr$findElement("css","#searchResults > table > tfoot > tr > th > a:nth-child(10)")$clickElement()
      while(check == TRUE){
        html <- remDr$getPageSource()[[1]]
        table <- (html %>% read_html() %>% html_table())[[1]]
        export <- rbind(export,table[3:(length(table$X1)-1),])
        count <- count +1
        for(j in 2:10){
          remDr$findElement("css",paste("#searchResults > table > tfoot > tr > th > a:nth-child(",j,")",sep=""))$clickElement()
          Sys.sleep(0.3)
          ss()
          html <- remDr$getPageSource()[[1]]
          table <- (html %>% read_html() %>% html_table())[[1]]
          export <- rbind(export,table[3:(length(table$X1)-1),])
          count <- count +1
          print(count)
        }
        check <- element_exists(remDr,"css","#searchResults > table > tfoot > tr > th > a:nth-child(11)")
        if(check == TRUE){
         remDr$findElement("css","#searchResults > table > tfoot > tr > th > a:nth-child(11)")$clickElement()
          Sys.sleep(0.3)
        }
      } 
      names(export) <- unname(unlist(table[2,]))
      write.csv(export,paste("./Utah/Utah",i,".csv",sep=""))
    }
  }
  Virginia <- function(Year){
    for(i in Year){
      for(j in 1:12){
        if(j <10){
          remDr$navigate(paste("https://apps.elections.virginia.gov/SBE_CSV/CF/",i,"_0",j,"/Report.csv",sep=""))
        }else{
          remDr$navigate(paste("https://apps.elections.virginia.gov/SBE_CSV/CF/",i,"_",j,"/Report.csv",sep=""))
        }
        Sys.sleep(0.5)
      }
      ClearTmp()
      CompileAndPort("Virginia",paste0("PACIDs",i))
      for(j in 1:12){
        if(j <10){
          remDr$navigate(paste("https://apps.elections.virginia.gov/SBE_CSV/CF/",i,"_0",j,"/ScheduleA.csv",sep=""))
        }else{
          remDr$navigate(paste("https://apps.elections.virginia.gov/SBE_CSV/CF/",i,"_",j,"/ScheduleA.csv",sep=""))
        }
        Sys.sleep(0.5)
      }
      ClearTmp()
      CompileAndPort("Virginia",i)
    }
  }
  Wyoming <- function(Year){
      for( i in Year){
        remDr$navigate("https://www.wycampaignfinance.gov/WYCFWebApplication/GSF_SystemConfiguration/SearchContributions.aspx")
        remDr$findElement("css","#imageDivLink")$clickElement()
        remDr$findElement("css","#ctl00_BodyContent_txtDateFrom")$sendKeysToElement(list(paste("1/1/",i,sep="")))
        remDr$findElement("css","#ctl00_BodyContent_txtDateTo")$sendKeysToElement(list(paste("12/31/",i,sep="")))
        remDr$findElement("css","#ctl00_BodyContent_bntSearch")$clickElement()
        while(element_exists(remDr,"css","#ctl00_BodyContent_btnExport") == FALSE){
          Sys.sleep(1)
        }
        remDr$findElement("css","#ctl00_BodyContent_btnExport")$clickElement()
        waitforLoad()
        ClearTmp()
        CompileAndPort("Wyoming",i)
      }
  }
#remDr$close()
  


