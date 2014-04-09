#### Libraries ####
library(XML)

#### Function Definition ####
# Get URL's from sitemap
getUrls <- function(sitemap){
  content <- htmlTreeParse(sitemap, useInternalNodes=T)
  urls <- xpathSApply(content, "//url/loc", xmlValue)
  return(urls)
}

# Parse the content of each URL and check for analytics js
gaCheck <- function(url){
  ga <- as.character()
  tm <- as.character()
  content <- htmlTreeParse(url, useInternalNodes=T)
  script <- xpathSApply(content, "//script", xmlValue)
  universal <- length(grep('analytics.js',script))
  gtm <- length(grep('GTM-',script))
  classic <- length(grep('ga.js',script))
  doubleclick <- length(grep('dc.js',script))
  
  if(gtm > 0){
    tm <- "Yes"
  } else {
    tm <- "No"
  }
  
  if(universal == 1){
    ga <- 'Universal Analytics'
	}  else if(classic == 1){
    ga <- 'Classic Analytics'
	}  else if(doubleclick == 1){
    ga <- 'Doubleclick Analytics'
    } else {
      ga <- 'None'
    }
  df <- data.frame(ga=ga,tm=tm)
  return(df)
}

# Boolean operation
ga <- function(x){
  if(x!='None'){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#### Load the sitemap ####
sitemap <-"http://www.agilej.com/sitemap.xml"

#### Get the URLs from sitemap ####
urls <- getUrls(sitemap)

#### Report Generation ####

# Get the Analytics type and GTM existence
result <- lapply(urls,gaCheck)
report <- do.call(rbind,result)
colnames(report) <- c("AnalyticsType","GoogleTagManager")

# Check the existence of Google Analytics
gaType <- data.frame(
  GoogleAnalytics=unlist(
    (lapply(report[,"AnalyticsType"],ga)
     )
    )
  )

# Final Report
report <- cbind(urls,gaType,report)
