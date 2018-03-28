##########################
# SaMi Analysis          #
# Author: Jesse Honkanen #
# Savonia amk 2018       #
# MIT Licence            #
##########################

### Read help_text.html for more info ###

### Uses checkpoint-package to install required packages ###
library(checkpoint)
checkpoint("2018-03-21", checkpointLocation = tempdir())


### Used R libraries ###
library(shiny) # web engine
library(shinyjs) # js additions
library(shinydashboard) # dashboard layout
library(jsonlite) # for parsing json
library(tidyverse) # loads ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats
library(dygraphs) # interactive time series plot
library(tictoc) # counting excecution times
library(lubridate) # datetime operations
library(zoo) # time series functions
library(xts) # time series objects
library(forecast) # univariate time series forecasting
library(pryr) # advanced rtools
library(DT) # Javascript DataTables interface
library(readODS) # for ods writing
library(openxlsx) # for excel writing

### Global variables ###
dataf <- "sami_data.rds"
dataf_csv <- "sami_data.csv"
max_meas <- 20000 # max measurements to download from SaMi
sami_url <- "https://sami.savonia.fi/Service/1.0/MeasurementsService.svc/json/measurements/"
samplequery <- paste0("https://sami.savonia.fi/Service/3.0/MeasurementsService.svc/json/measurements/",
                      "your-key-here?obj=your-meas-object&tag=your-meas-tag",
                      "&data-tags=comma-separated-list-of-data-tags&from=from&to=to",
                      "&take=20&inclusiveFrom=true&inclusiveTo=true&binaryValueFormat=ByteArray",collapse="")

# load UI and logic
source("myUI.R", local = TRUE)
source("myServer.R", local = TRUE)


# Run the application 
app <- shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE, port = 5470))
