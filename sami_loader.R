library(jsonlite)
library(tidyverse)
library(tictoc)

sami_url <- "https://sami.savonia.fi/Service/1.0/MeasurementsService.svc/json/measurements/"

#loads data from sami server
#query = list of query params
#returns converted data frame
sami_load_data <- function(query) {
  tic("Total processing time")
  # pitäisi tehdä queryn parsinta
  tic("Loading data from SaMi")
  #make df from JSON-data
  cat("Loading data from SaMi service\n")
  cat("Load data using query: ",query,"\n")
  jsondata <- fromJSON(query)
  toc()
  req_cols <- c("Data", "Object", "Tag", "TimestampISO8601")
  if (is_empty(jsondata) || !(req_cols %in% colnames(jsondata)) || nrow(jsondata) < 1) {
    stop("Loading data from SaMi failed for some reason")
  } else if (any(is.na(jsondata$Data))){
    stop("Data in SaMi is invalid")
  } else if (nrow(jsondata) > 20000){
    stop("There are over 20 000 measurements. Load smaller sample.")
  }
  rows = nrow(jsondata)
  #print messages to output
  cat("Measurement count:",rows,"\n")
  cat("Starting data conversion. This may take several minutes. Please wait.\n")
  #check data validity
  tic("Data conversion total")
  tdf <- samijson_to_df(jsondata)
  toc()
  toc()
  cat("Data loaded succesfully!\n")
  return(tdf)
}

#takes JSON object as input and returns a data frame (tibble)
samijson_to_df <- function(jdf) {
  #timing spread operation
  tic("Spreading SaMi data")
  #takes each measurement data points and combines them to df
  #may require many minutes with lots of measurements. returns data frame 
  jdfdata <- lapply(jdf[["Data"]], function(x) spread(x[c("Tag","Value")], key=Tag, value=Value))
  toc() #print time to console
  #timing combining operation
  tic("Combining data")
  #combine data values. missing values get NA
  tdf <- rbind.fill(jdfdata)
  #format timestamp
  time <- as_datetime(jdf$TimestampISO8601, tz="Europe/Helsinki")
  #combine selected columns, result is tibble
  tdf <- as_tibble(bind_cols(Timestamp=time, select(jdf, Object, Tag, Note, Location), tdf))
  toc()
  return(tdf)
}

jsondf <- sami_load_data("SIMO-mittaus_2017-11-15_json.js")
