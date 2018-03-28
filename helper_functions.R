### Helper functions ###



# Helper function to find wanted ts
get_selected_ts <- function(tsname, onlyauto = FALSE) {
  fromlist <- switch(substr(tsname,1,3), "-a-"=values$tslist,"-u-"=values$tscreated,"-c-"=values$tscombined)
  name <- substring(tsname,4)
  if (name %in% names(fromlist)) {
    return(fromlist[[name]])
  } else {
    return(NULL)
  }
}




# Helper function for updateSelectInput()
create_ts_namelist <- function(includecombined=FALSE) {
  if (!is.null(values$tslist) && length(values$tslist) > 0) {
    tsautonames = as.list(paste0("-a-",names(values$tslist)))
    names(tsautonames) = names(values$tslist)
  } else {
    tsautonames = NULL
  }
  if (!is.null(values$tscreated) && length(values$tscreated) > 0) {
    tsusernames = as.list(paste0("-u-",names(values$tscreated)))
    names(tsusernames) = names(values$tscreated)
  } else {
    tsusernames = NULL
  }
  if (includecombined && !is.null(values$tscombined) && length(values$tscombined) > 0) {
    tscombinednames = as.list(paste0("-c-",names(values$tscombined)))
    names(tscombinednames) = names(values$tscombined)
  } else {
    tscombinednames = NULL
  }
  list("User created single variable" = rev(tsusernames), "Combined multivariable" = rev(tscombinednames), 
       "Automatically created" = tsautonames)
}





# Helper function for deleting ts
delete_ts <- function(tsname) {
  fromlist <- switch(substr(tsname,1,3), "-a-"="auto","-u-"="user","-c-"="combined")
  name <- substring(tsname,4)
  if (fromlist == "auto") {
    values$tslist[[name]] <- NULL
  } else if (fromlist == "user") {
    values$tscreated[[name]] <- NULL
  } else if (fromlist == "combined") {
    values$tscombined[[name]] <- NULL
  }
}




# Create ts summary as plain text and return it
create_ts_summary <- function(ts, name="[Unkown]") {
  if (is.null(ts) || length(ts) < 1 || ncol(ts) < 1) 
    return("Selected time series is empty!")
  if (ncol(ts) == 1) {
    summary <- paste0("Variable: ", names(ts)[1],"\n")
    per <- periodicity(ts)
    summary <- paste0(summary, "First measurement time: ", per$start,"\n")
    summary <- paste0(summary, "Last measurement time: ", per$end,"\n")
    summary <- paste0(summary, "Measurements: ", nrow(ts), "\n")
    summary <- paste0(summary, "Periodicity: ", per$scale, "\n")
    summary <- paste0(summary, "Frequency: ", round(per$frequency,3), "\n")
    summary <- paste0(summary, "Empty values (NA): ", sum(is.na(ts[[1]])), "\n")
    summary <- paste0(summary, "Are there duplicate timestamps: ", ifelse(is.index.unique(ts), "no", "yes"), "\n")
    summary <- paste0(summary, "First measurement value: ", xts::first(ts),"\n")
    summary <- paste0(summary, "Last measurement value: ", xts::last(ts), "\n")
    summary <- paste0(summary, "Minimum value: ", min(ts, na.rm=TRUE), "\n")
    summary <- paste0(summary, "Maximum value: ", max(ts, na.rm=TRUE), "\n")
    summary <- paste0(summary, "Mean: ", round(mean(ts, na.rm=TRUE),digits=4), "\n")
    summary <- paste0(summary, "Standard deviation: ", round(sd(ts, na.rm=TRUE), digits=4), "\n")
  } else if (ncol(ts) > 1) {
    summary <- paste0(lapply(ts, create_ts_summary, name), collapse="\n")
    return(summary)
  } 
  return(paste0("Time series name: ",name,"\n",summary))
}





# Creates automatic list of time series objects (xts)
create_timeseries <- function(){
  if (is.null(values$data))
    return(NULL)
  if (values$fromsami) {
    values$objects <- levels(values$data$Object)
    values$tags <- levels(values$data$Tag)
    values$sensors <- sort(colnames(values$data)[-(1:6)])
  } else {
    if (all(c("Object", "Tag") %in% names(values$data))) {
      values$objects <- levels(values$data$Object)
      values$tags <- levels(values$data$Tag)
    } else {
      values$objects <- NULL
      values$tags <- NULL
    }
    values$sensors <- sort(colnames(values$data)[sapply(values$data, is.numeric)])
  }
  tslist <- list()
  print_to_output("sconsole", "Starting time series conversion", leading=TRUE, newline=TRUE, append=TRUE)
  if(values$fromsami || all(c("Object","Tag") %in% colnames(values$data))) {
    #make object-tag combinations
    objtag <- dplyr::summarise(values$data)
    if (is.null(objtag) || NROW(objtag) < 1 || !all(c("Object", "Tag") %in% names(objtag))) {
      print_to_output("sconsole", "Error in conversion. Conversion cancelled", leading=TRUE, newline=TRUE, append=TRUE)
      return()
    } else {
      objtag$Object <- as.character(objtag$Object)
      objtag$Tag <- as.character(objtag$Tag)
      objtag <- as.matrix(objtag, ncol=2)
    }
    #find empty sensor columns
    emptysens <- sapply(values$data[,values$sensors], function(x) all(is.na(x)))
    #remove empty sensors
    if (!is.null(emptysens))
      sensors <- values$sensors[!emptysens]
    sencount <- 1
    for (i in seq_len(NROW(objtag))) {
      for (j in seq_len(length(sensors))) {
        sens <- dplyr::filter(values$data, Object==objtag[i,1], Tag==objtag[i,2])[,c("Timestamp", sensors[j])]
        if (!all(is.na(sens[[2]]))){
          nobj <- which(values$objects == objtag[i,1])
          ntag <- which(values$tags == objtag[i,2])
          newname <- paste0("O",nobj,"T",ntag," ",sensors[j])
          tssens <- xts(sens[[2]], order.by=sens[[1]])
          names(tssens) <- sensors[j]
          tslist[[sencount]] <- tssens
          names(tslist)[sencount] <- newname
          sencount <- sencount + 1
        }
      }
    }
  } else {
    #assume that first column is timestamp and other columns numeric data
    sensors <- values$sensors
    for (i in 1:length(values$sensors)) {
      tssens <- xts(values$data[[sensors[i]]], order.by=values$data[[1]])
      names(tssens) <- sensors[i]
      tslist[[i]] <- tssens
      names(tslist)[i] <- sensors[i]
    }
  }
  # #create combined time series object
  # combinedts <- xts()
  # for (i in 1:length(tslist)) {
  #   combinedts <- merge(combinedts, tslist[[i]], all=TRUE)
  # }
  # values$tscombined <- list("All combined"=combinedts)
  
  print_to_output("sconsole", paste("Created", length(tslist), "time series objects"), leading=TRUE, newline=TRUE, append=TRUE)
  #save to global list
  values$tslist <- tslist
}





# handles time series aggregation
aggregate_timeseries <- function(ts, aggr, period, method, createtimes) {
  #print("Starting timeseries aggregation")
  aggper <- paste(period, aggr)
  # change times in time series
  ts <- xts(coredata(ts), floor_date(index(ts), aggper))
  ohlc <- FALSE
  # if (method != "n")
  #   ts <- na.omit(ts)
  if (method == "n") {
    f <- function(x) { length(x)}
  } else if (method == "ohlc") {
    ohlc <- TRUE
  } else {
    f <- function(x) { eval(parse(text=paste0(method,"(x, na.rm=TRUE)"))) }
  }
  if (!ohlc) {
    ends <- endpoints(ts, aggr, period)
    tsnew <- period.apply(ts, ends, f)
  } else {
    #creates ohlc. only first column is used
    if (aggr=="mins") {
      per <- "minutes"
    } else {
      per <- aggr
    }
    tsnew <- to.period(ts[,1], period=per, k=period, name=colnames(ts)[1])
  }
  # creates new times
  if (createtimes) {
    firsttime <- index(ts)[1]
    lasttime <- index(ts)[length(ts)]
    newtimes <- seq(firsttime, lasttime, by=aggper)
    newxts <- xts(NULL,newtimes,tzone=Sys.timezone())
    tsnew <- merge(newxts,tsnew)
  }
  return(tsnew)
}





# Prints message to console and in html element
# htmlid = html element or shiny tag to print to
# msg = HTML markup or plain text
# leading = string to add before every line
# file = file to print to (if not given, prints to R console)
print_to_output <- function(htmlid, msg, leading=FALSE, newline=TRUE, append=TRUE, file=NULL) {
  if (leading)
    msg <- paste0("> ",msg)
  if (newline)
    msg <- paste0(msg, "\n")
  cat(msg)
  shinyjs::html(id=htmlid,html=msg, add=append)
}




# Create summary for summary page
summary_printer <- function() {
  if (!is.null(values$data)) {
    output$html_summary <- renderUI({
      withTags(
        tagList(
          h3("Dataset summary"),
          p(
            strong("Measurements: "),
            paste0(length(values$data[[1]]))
          ),
          p(
            strong("First measurement: "),
            paste0(min(values$data[[1]], na.rm=TRUE))
          ),
          p(
            strong("Last measurement: "),
            paste0(max(values$data[[1]], na.rm=TRUE))
          ),
          p(
            strong("Objects: "),
            paste0(values$objects, collapse=", ")
          ),
          p(
            strong("Tags: "),
            paste0(values$tags, collapse=", ")
          ),
          p(
            strong("Numeric variables: "),
            paste0(values$sensors, collapse=", ")
          ),
          p(
            strong("First lines of dataset")
          ),
          pre(
            paste0(capture.output(glimpse(values$data)), collapse="\n")
          ),
          h3("Time series summary"),
          h4("User created timeseries"),
          pre(
            paste0(lapply(names(values$tscreated), function(x) create_ts_summary(values$tscreated[[x]], x) ), collapse="\n")
          ),
          h4("Combined timeseries"),
          pre(
            paste0(lapply(names(values$tscombined), function(x) create_ts_summary(values$tscombined[[x]], x) ), collapse="\n")
          ),
          h4("Automatically created timeseries"),
          p("To avoid overlapping names time series were automatically renamed to the following format:"
            ,em("O<x>T<y> <variable>")
          ),
          p(
            strong("Objects: "),
            paste0(sapply(values$objects, function(x) paste0("O",which(values$objects==x)," = ",x)),collapse=", ")
          ),
          p(
            strong("Tags: "),
            paste0(sapply(values$tags, function(x) paste0("T",which(values$tags==x)," = ",x)),collapse=", ")
          ),
          pre(
            paste0(lapply(names(values$tslist), function(x) create_ts_summary(values$tslist[[x]], x) ), collapse="\n")
          )
        )
      )
    })
  }
}




# Makes interactive time series plot
plot_timeseries <- function() {
  ts <- get_selected_ts(input$sel_graph_ts)
  if (is.null(ts)) return()
  graphname <- substring(input$sel_graph_ts, 4)
  
  if (input$sel_plotlibrary == 'dygraphs') {
    graphtype <- input$sel_dygraphs_type
    output$plot_dygraphs <- renderDygraph({
      if (graphtype == "line") {
        dygraph(ts, main = graphname)
      } else if (graphtype == "candle") {
        if (is.OHLC(ts)) {
          dygraph(ts, main = graphname) %>% dyCandlestick()
        } else {
          shinyjs::alert("Selected time series is not OHLC!")
        }
      }
    })
  } else if (input$sel_plotlibrary == 'highcharter') {
    graphtype <- input$sel_highcharter_type
    output$plot_highcharter <- renderHighchart({
      if (graphtype == "line") {
        hchart(ts)
      } else if (graphtype == "candle") {
        if (is.OHLC(ts)) {
          shinyjs::alert("Not implemented yet!")
        } else {
          shinyjs::alert("Selected time series is not OHLC!")
        }
      } else if (graphtype == "bar") {
        shinyjs::alert("Not implemented yet!")
      } else if (graphtype == "histogram") {
        shinyjs::alert("Not implemented yet!")
      } else if (graphtype == "scatter") {
        shinyjs::alert("Not implemented yet!")
      } else if (graphtype == "heat") {
        shinyjs::alert("Not implemented yet!")
      } else if (graphtype == "boxplot") {
        shinyjs::alert("Not implemented yet!")
      }
    })
  }
}
