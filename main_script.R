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

#data evolution by country
confirmedSub <- confirmedData %>% 
                pivot_longer(names_to = "date",cols = 5:ncol(confirmedData)) %>%
                group_by(`Province.State`,`Country.Region`,date,Lat,Long) %>%
                summarise("confirmed" = sum(value,na.rm = TRUE))

deceasedSub <- deceasedData %>% 
                pivot_longer(names_to = "date",cols = 5:ncol(deceasedData)) %>%
                group_by(`Province.State`,`Country.Region`,date,Lat,Long) %>%
                summarise("deceased" = sum(value,na.rm = TRUE))

evolutionData <- confirmedSub %>%
  full_join(deceasedSub) %>%
  ungroup() %>%
  mutate(date=as.Date(date,"%m.%d.%y")) %>%
  arrange(date) %>%
  group_by(`Province.State`,`Country.Region`,Lat,Long) %>%
  mutate(
    recovered = lag(confirmed,14,default = 0) - deceased,
    recovered = ifelse(recovered>0,recovered,0),
    active = confirmed - recovered - deceased) %>%
  pivot_longer(names_to = "var",cols=c(confirmed,recovered,deceased,active)) %>%
  ungroup()

evolutionData <- evolutionData %>%
  group_by(`Province.State`,`Country.Region`) %>%
  mutate(value_new = value - lag(value, 4, default = 0)) %>%
  ungroup()

#clear environment
remove(confirmedData,confirmedSub,recoveredData,deceasedData,deceasedSub)
remove(n_recovered,n_deceased,n_confirmed,date_correct)

#downloading population data
population <- wb_data(country = "countries_only", indicator = "SP.POP.TOTL", start_date = 2018, end_date = 2020) %>%
  select(country, SP.POP.TOTL) %>%
  rename(population = SP.POP.TOTL)

countryNamesPop <- c("Brunei Darussalam", "Congo, Dem. Rep.", "Congo, Rep.", "Czech Republic","Egypt, Arab Rep.", "Iran, Islamic Rep.", "Korea, Rep.", "St. Lucia", "West Bank and Gaza", "Russian Federation","Slovak Republic", "United States", "St. Vincent and the Grenadines", "Venezuela, RB")

countryNamesDat<- c("Brunei", "Congo (Kinshasa)", "Congo (Brazzaville)", "Czechia", "Egypt", "Iran", "Korea, South",
                    "Saint Lucia", "occupied Palestinian territory", "Russia", "Slovakia", "US", "Saint Vincent and the Grenadines", "Venezuela")

population <- population[complete.cases(population),]
need <- seq(2,432,2)
population <- population[need,]
population[which(population$country %in% countryNamesPop), "country"] <- countryNamesDat
noDataCountries <- data.frame(
  country    = c("Cruise Ship", "Guadeloupe", "Guernsey", "Holy See", "Jersey", "Martinique", "Reunion", "Taiwan*"),
  population = c(3700, 395700, 63026, 800, 106800, 376480, 859959, 23780452)
)
population <- bind_rows(population, noDataCountries)
evolutionData <- evolutionData %>%left_join(population, by = c("Country.Region" = "country"))

#latest data
data_to_date <- function(inputDate){
  evolutionData[which(evolutionData$date == inputDate),] %>%
  distinct() %>%
  pivot_wider(id_cols = c("Province.State", "Country.Region", "date", "Lat", "Long", "population"), names_from = var, values_from = value) %>%
  filter(confirmed > 0|recovered > 0|deceased > 0|active > 0)  
}

latest_data <- data_to_date(max(evolutionData$date))

#subsetting the top5 countries

top5_countries <- evolutionData %>%
  filter(var=="active", date==latest_date) %>%
  group_by(`Country.Region`) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  arrange(desc(value)) %>%
  top_n(5) %>%
  select(`Country.Region`) %>%
  pull()