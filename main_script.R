#importing necessary libraries
library(shiny) 
library(shinydashboard)
library(DT)
library(fs)
library(wbstats)
library(leaflet)
library(plotly)
library(tidyverse)

#utils script
source('src_utils.R')

#downloading and refreshing data
downloadData <- function(){
  download.file(url = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip",destfile = "data/CVD19_data")
  
  total_path <- "COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"
  unzip(zipfile = 'data/CVD19_data.zip',files = paste0(total_path,c("confirmed_global.csv", "deaths_global.csv", "recovered_global.csv")),exdir = "data",junkpaths = TRUE)
}

refreshData <- function(){
  refresh_time = 24 #hours
  if(!dir.exists('data')){
    dir.create('data')
    downloadData()
  }
  else if((!file.exists('data/CVD19_data.zip')) || (as.double(Sys.time() - file.info('data/CVD19_data.zip')$ctime,units='hours')>refresh_time)){
    downloadData()  
  }
}

#update on start
refreshData()

#reading the world
confirmedData <- read.csv("data/time_series_covid19_confirmed_global.csv")
deceasedData <- read.csv("data/time_series_covid19_deaths_global.csv")
recoveredData <- read.csv("data/time_series_covid19_recovered_global.csv")

#date correction 
n_confirmed <- names(confirmedData)[5:ncol(confirmedData)]
date_correct <- gsub("[a-zA-Z]","",n_confirmed)
names(confirmedData)[5:ncol(confirmedData)] <- date_correct

n_deceased <- names(deceasedData)[5:ncol(deceasedData)]
date_correct <- gsub("[a-zA-Z]","",n_deceased)
names(deceasedData)[5:ncol(deceasedData)] <- date_correct

n_recovered <- names(recoveredData)[5:ncol(recoveredData)]
date_correct <- gsub("[a-zA-Z]","",n_recovered)
names(recoveredData)[5:ncol(recoveredData)] <- date_correct

#latest data  
latest_date <- as.Date(names(confirmedData)[ncol(confirmedData)],format = "%m.%d.%y") 
changed_date <- file.info('data/CVD19_data')$ctime