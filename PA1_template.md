# Reproducible Research: Peer Assessment 1
### Environment info

```r
setwd("C:/Data/Coursera/repdata-010/RepData_PeerAssessment1")
```

```r
rversion <- R.Version()$version.string
sysname <- Sys.info()["sysname"]
sys.release <- Sys.info()["release"]
```
R version 3.1.2 (2014-10-31)  
OS Version: Windows, 7 x64

## Loading and preprocessing the data

```r
#check have we loaded data:
if (!file.exists("data")) {     
  dir.create("data")  
}

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata/data/activity.zip"
zip_filename <- "./data/activity.zip"
csv_filename <- "./data/activity.csv"

if (!file.exists(csv_filename)) {
  # if we have not CSV file and have not ZIP file - try to get  them from web    
  if (!file.exists(zip_filename))
    {
    
    download.file(fileUrl, destfile = zip_filename, method = "curl")
    }
  
  zip.file.ctime <- as.character(file.info(zip_filename)$ctime)
  
  # unzip and save to CSV file
  con <- unz(zip_filename, "activity.csv")
  f <- readLines(con)
  writeLines(f, file(csv_filename))
  close(con)
  
  
  dataSource.info <- paste0(csv_filename,"', unzipped from zip '", zip_filename, "', downloaded on ", zip.file.ctime, "")    
} else   
{
  dataSource.info <- csv_filename
}

activity <- read.csv(csv_filename)
```
Data source: ./data/activity.csv', unzipped from zip './data/activity.zip', downloaded on 2015-01-17 23:36:23


# What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
