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

