# Define server logic
server <- function(input, output, session) {
  
  # reactive values to save server status
  values <- reactiveValues()
  # downloaded data in data frame
  values$data <- NULL
  # automatically created time series list
  values$tslist <- NULL
  # user created time series list
  values$tscreated <- NULL
  # combined time series list
  values$tscombined <- NULL
  # is data from sami db
  values$fromsami <- FALSE
  
  #### Things to do when session loads ####
  shinyjs::html(id = "sconsole", html = paste0("> Messages during load will be shown here\n"))
  shinyjs::disable("div_actions")
  shinyjs::disable("div_filter")
  shinyjs::disable("btn_download")
  shinyjs::hide("graphsettings")
  shinyjs::hide("hiding4")
  cat("> New session! Welcome to SaMi Analysis!\n")
  options(xts_check_TZ=FALSE) # possible warnings without this
  
  #### Load helper functions ####
  source("helper_functions.R", local = TRUE)
  
  ### Quit the app ###
  observeEvent(input$btn_quit, {
    shinyjs::runjs("setTimeout(function(){window.close();},500);")
    stopApp()
  })
  
  
  
  #### Sidebar selection actions ####
  observeEvent(input$sidebar, {
    selected <- input$sidebar
    if (selected == "loader") {
      if (file.exists(dataf)) {
        updateRadioButtons(session, "query_select", selected=4)
      }
    } else if (selected == "filter") {
      create_ts_page()
    } else if(selected == "graphs") {
      create_graphs_page()
    } else if(selected == "tables") {
      create_tables_page()
    } else if(selected == "summary") {
      create_summary_page()
    } else if(selected == "analysis") {
      create_analysis_page()
    }
  })
  
  
  
  
  
  
  #### Data loader page actions ####
  
  #create infobox that shows data load status
  output$box_status <- renderInfoBox({
    if (is.data.frame(values$data)) {
      msg <- "Data loaded"
      boxsubtitle <- paste0("Measurements: ",nrow(values$data))
      boxcolor <- "green"
      boxicon <- "check"
    }
    else {
      msg <- "Data not loaded"
      boxsubtitle <- "Load data with \"Data loader\""
      boxcolor <- "red"
      boxicon <- "ban"
    }
    infoBox("Data status", value=msg, subtitle=boxsubtitle, icon=icon(boxicon), color=boxcolor, fill=TRUE)
  })
  
  # Enables or disables buttons
  observeEvent(values$data, {
    if (is.data.frame(values$data)) {
      shinyjs::enable("div_actions")
      shinyjs::enable("div_filter")
      shinyjs::enable("btn_download")
    } else {
      shinyjs::disable("div_actions")
      shinyjs::disable("div_filter")
      shinyjs::disable("btn_download")
      return()
    }
  })
  
  
  # Loads data from SaMi or other source and saves it to values$data
  observeEvent(input$btn_load,{
    tryCatch({
      msg <- paste0("Starting data load")
      print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=FALSE)
      selected = input$query_select
      query <- ""
      if (selected == 1) {
        gquery=sami_url
        if (nchar(input$txt_key)[1] < 1) {
          stop("You have to enter a key!")
        } else {
          gquery = paste0(gquery,input$txt_key,"?",collapse='')
        }
        if (nchar(input$txt_object)[1] > 0) {
          gquery = paste0(gquery,"&obj=",input$txt_object,collapse='')
        }
        if (nchar(input$txt_tag)[1] > 0) {
          gquery = paste0(gquery,"&tag=",input$txt_tag,collapse='')
        }
        if (nchar(input$txt_sensors)[1] > 0) {
          gquery = paste0(gquery,"&data-tags=",input$txt_sensors,collapse='')
        }
        if (input$sel_filter == 2) {
          gquery = paste0(gquery,"&take=",input$num_latest,collapse='')
        } else {
          gquery = paste0(gquery,"&take=",max_meas,collapse='')
        }
        
        if (input$cb_datesload) {
          gquery = paste0(gquery,"&from=",input$in_date_range[1],collapse='')
          gquery = paste0(gquery,"&to=",input$in_date_range[2],collapse='')
        } else {
          gquery = paste0(gquery,"&from=1900-01-01",collapse='')
          gquery = paste0(gquery,"&to=2100-01-01",collapse='')
        }
        gquery = paste0(gquery,"&inclusiveFrom=true",collapse='')
        gquery = paste0(gquery,"&inclusiveTo=true",collapse='')
        #cat(gquery)
        query <- gquery
      } else if (selected == 2) {
        query <- input$txt_query
      } else if (selected == 3) {
        file <- paste0("examples/",input$sel_exdata,".rds")
        if (file.exists(file)) {
          values$data <- readRDS(file)
          msg <- paste0("Loaded file: ",file)
          print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
          values$fromsami <- FALSE
          create_timeseries()
        } else {
          stop(paste0("Can't find file: ",file))
        }
        return()
      } else if (selected == 4) {
        if (file.exists(dataf)){
          values$data <- readRDS(dataf)
          msg <- paste0("Loaded file: ",dataf)
          print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
          values$fromsami <- TRUE
          create_timeseries()
        } else {
          stop(paste0("Can't find file: ",dataf))
        }
        return()
      }
      # load data using query
      withProgress(message="Processing data", value=0, {
        tic("Total processing time")
        tic("Loading data from SaMi")
        msg <- "Loading data from SaMi service"
        print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
        query <- URLencode(query)
        msg <- paste0("Load data using query:\n",query)
        print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
        incProgress(0.1, detail="Downloading data")
        #download data and make df from JSON-data
        jsondata <- fromJSON(query)
        msg <- paste0(capture.output(toc()))
        print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
        req_cols <- c("Data", "Object", "Tag", "TimestampISO8601")
        if (is_empty(jsondata) || !(req_cols %in% colnames(jsondata)) || nrow(jsondata) < 1) {
          stop("Loading data from SaMi failed for some reason")
        } else if (any(is.na(jsondata$Data))){
          stop("Data in SaMi is invalid")
        } else if (nrow(jsondata) > max_meas){
          stop(paste0("There are over ", max_meas, " measurements. Load smaller sample."))
        }
        rows = nrow(jsondata)
        #print messages to output
        msg <- paste0("Measurement count:",rows)
        print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
        msg <- paste0("Starting data conversion")
        print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
        msg <- paste0("This may take several minutes. Please wait...")
        print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
        tic("Data conversion total")
        tic("Spreading SaMi data")
        incProgress(0.2, detail="Converting data")
        #takes each measurement data points and combines them to df
        #may require many minutes with lots of measurements. returns data frame
        jdfdata <- jsondata$Data
        jsondata$Data <- NULL # delete extra data
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Converting", value = 0)
        for (x in 1:rows) {
          if(x %% 20 == 0) progress$set(value=x/rows, detail=paste("Measurement",x,"/",rows))
          jdfdata[[x]] <- spread(jdfdata[[x]][c("Tag","Value")], key=Tag, value=Value)
        }
        progress$set(value=1, detail=paste("Measurement",rows,"/",rows))
        msg <- paste0(capture.output(toc()))
        print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
        incProgress(0.5, detail="Combining data")
        #timing combining operation
        tic("Combining data")
        #combine data values. missing values get NA
        tdf <- bind_rows(jdfdata)
        incProgress(0.1, detail="Finishing")
        #format timestamp
        time <- as_datetime(jsondata$TimestampISO8601, tz="Europe/Helsinki")
        #take locations
        if (is.list(jsondata$Location)) {
          latitude <- jsondata$Location$Latitude
          longitude <- jsondata$Location$Longitude
        } else {
          latitude <- rep(NA,rows)
          longitude <- rep(NA,rows)
        }
        #make factors from Object and Tag. include NA if present
        objects <- fct_explicit_na(factor(jsondata$Object), na_level="[Empty]")
        tags <- fct_explicit_na(factor(jsondata$Tag), na_level="[Empty]")
        notes <- as.character(jsondata$Note)
        #combine selected columns, result is tibble
        tdf <- as_tibble(bind_cols(Timestamp=time, Object=objects, Tag=tags, Note=notes, 
                                   Latitude=latitude, Longitude=longitude, tdf))
        tdf <- group_by(tdf, Object, Tag)
        msg <- paste0(capture.output(toc()))
        print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
        msg <- paste0(capture.output(toc()))
        print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
        msg <- paste0(capture.output(toc()))
        print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
        msg <- "Data loaded succesfully!"
        print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
        incProgress(0.1, detail="Done")
        Sys.sleep(1)
      })
      values$data <- tdf
      values$fromsami <- TRUE
      create_timeseries()
      saveRDS(tdf, dataf)
      msg <- paste0("Wrote converted data to file: ",dataf)
      print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
      write_csv(tdf, dataf_csv)
      msg <- paste0("Wrote CSV to file: ",dataf_csv)
      print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
    }, 
    warning = function(err) {
      msg <- paste0("Warning: ",conditionMessage(err))
      print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
      print(err)
      msg <- paste0("Load terminated")
      print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)

    },
    error = function(err) {
      msg <- paste0("Error: ",conditionMessage(err))
      print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
      print(err)
      msg <- paste0("Load terminated")
      print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
    },
    finally = {
      msg <- paste0("Loading data ended")
      print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
      msg <- capture.output(print(mem_used()))
      msg <- paste0("Memory in use: ", msg)
      print_to_output("sconsole", msg, leading=TRUE, newline=TRUE, append=TRUE)
    }
    )
  })
  
  
  # Creates download file and saves it
  output$btn_download <- downloadHandler(
    filename = function() {
      if (input$sel_downloadwhat == 1) {
        filu <- paste0("SaMi_data_",Sys.Date(),".",input$sel_download)
      } else {
        filu <- paste0(substring(as.character(input$sel_tstodownload), 4),"_",Sys.Date(),".",input$sel_download)
      }
      filu
    },
    content = function(file) {
      Sys.sleep(1)
      if (input$sel_downloadwhat == 1) {
        ddata <- values$data
      } else {
        ts <- get_selected_ts(input$sel_tstodownload)
        ddata <- bind_cols(Timestamp=index(ts), as_tibble(coredata(ts)))
      }
      if (input$cb_exlimitdigits) {
        digits <- as.integer(input$num_exportdecimals)
        nums <- vapply(ddata, is.numeric, FUN.VALUE = logical(1))
        ddata[,nums] <- round(ddata[,nums], digits=digits)
      }
      selected <- input$sel_download
      if (selected == "csv") {
        write_excel_csv(ddata, file)
      } else if (selected == "xlsx") {
        write.xlsx(ddata, file, colNames = TRUE)
      } else if (selected == "rds") {
        write_rds(ddata, file)
      } else if (selected == "json") {
        write(toJSON(ddata, pretty=TRUE, na="null", POSIXt="ISO8601", Date="ISO8601", factor="string", digits=4), file)
      }  
    },
    contentType=NULL
  )
  
  
  # Filters values$data and overwrites it
  # Creates new time series objects too
  observeEvent(input$btn_filter, {
    if (is.null(values$data))
      return()
    selections <- as.numeric(input$cbg_filter)
    dropvariables <- if (1 %in% selections) TRUE else FALSE
    dropmeas <- if (2 %in% selections) TRUE else FALSE
    ordervars <- if (3 %in% selections) TRUE else FALSE
    overwrite <- if (4 %in% selections) TRUE else FALSE
    from <- input$dates_filter[1]
    to <- input$dates_filter[2]
    objs <- input$cbg_objects
    tags <- input$cbg_tags
    sensors <- input$cbg_sensors
    newdata <- values$data
    if (ordervars) {
      sensors <- sort(sensors)
    } else {
      sensors <- names(newdata)[(names(newdata) %in% sensors)]
    }
    #drop unselected objects and tags
    if (all(c("Object", "Tag") %in% names(newdata))) {
      newdata <- newdata %>% filter(Object %in% objs) %>% filter(Tag %in% tags) 
    }
    if (dropmeas) {
      sensdf <- newdata[,(names(newdata) %in% values$sensors)]
      emptyrows <- apply(sensdf, 1, function(x) all(is.na(x)))
      newdata <- newdata[!emptyrows,]
    }
    if (dropvariables) {
      #find remaining empty variables
      emptysens <- sapply(newdata[,(names(newdata) %in% sensors)], function(x) all(is.na(x)))
      emptysens <- names(emptysens)[emptysens]
      sensors <- sensors[!(sensors %in% emptysens)]
    }
    #columns to keep
    keep <- c(names(newdata)[!(names(newdata) %in% values$sensors)], sensors)
    #drop unselected variables
    newdata <- newdata[,(names(newdata) %in% keep)]
    #limit by date range
    newdata <- newdata %>% filter_at(1, all_vars(between(as_date(.), as_date(from), as_date(to))))
    #remake Object and Tag factors and grouping
    if (all(c("Object", "Tag") %in% names(newdata))) {
      newdata$Object <- fct_explicit_na(factor(as.character(newdata$Object)), na_level="[Empty]")
      newdata$Tag <- fct_explicit_na(factor(as.character(newdata$Tag)), na_level="[Empty]")
      newdata <- group_by(newdata, Object, Tag)
    }
    if (is.null(newdata) || nrow(newdata) < 1) {
      values$data <- NULL
      values$tslist <- NULL
      print_to_output("sconsole", "Filtered dataset is empty!",leading=TRUE, newline=TRUE, append=TRUE)
    } else {
      if (overwrite) {
        saveRDS(newdata, dataf)
        write_csv(newdata, dataf_csv)
      }
      msg <- paste("Filtering dataset complete.", nrow(newdata), "measurements saved.")
      print_to_output("sconsole", msg,leading=TRUE, newline=TRUE, append=TRUE)
      values$data <- newdata
      create_timeseries()
    }
  })
  
  # Updates filter tab
  update_filter_tab <- function() {
    objs <- values$objects
    if (is.null(objs)) objs <- character(0)
    tags <- values$tags
    if (is.null(tags)) tags <- character(0)
    sensors <- values$sensors
    if (is.null(sensors)) sensors <- character(0)
    
    updateCheckboxGroupInput(
      session, "cbg_objects",
      label = paste0("Objects [",length(objs),"]"),
      choices = objs,
      selected = objs,
      inline=TRUE
    )
    updateCheckboxGroupInput(
      session, "cbg_tags",
      label = paste0("Tags [",length(tags),"]"),
      choices = tags,
      selected = tags,
      inline=TRUE
    )
    updateCheckboxGroupInput(
      session, "cbg_sensors",
      label = paste0("Variables [",length(sensors),"]"),
      choices = sensors,
      selected = sensors,
      inline=TRUE
    )
    updateDateRangeInput(
      session, "dates_filter",
      start = min(values$data[[1]], na.rm=TRUE),
      end = max(values$data[[1]], na.rm=TRUE)
    )
  }
  
  # Updates filter tab if values$sensors change
  observeEvent(values$sensors, {
    update_filter_tab()
  })
  
  # Updates download list
  observeEvent(input$sel_downloadwhat, {
    updateSelectInput(session, "sel_tstodownload", choices = create_ts_namelist(TRUE))
  })
  
  # Opens summary page
  observeEvent(input$btn_acsummary, {
    updateTabItems(session, "sidebar", "summary")
  })
  
  # Opens table page
  observeEvent(input$btn_actable, {
    updateTabItems(session, "sidebar", "tables")
  })
  
  # Opens plot page
  observeEvent(input$btn_acplot, {
    updateTabItems(session, "sidebar", "graphs")
  })
  
  # Opens analysis page
  observeEvent(input$btn_acanalyse, {
    updateTabItems(session, "sidebar", "analysis")
  })
  
  
  
  
  
  
  
  #### Time series page actions ####
  create_ts_page <- function() {
    if(is.null(values$tslist))
      return()
    # Reset page to default
    shinyjs::reset("div_tsoptions")
    updateSelectInput(session, "sel_filterts", "Select time series to process", choices = create_ts_namelist(TRUE))
    updateCheckboxGroupInput(session, "cbg_combine_created", "User created time series to combine", 
                             choices = as.list(names(values$tscreated)), inline=TRUE)
    updateCheckboxGroupInput(session, "cbg_combine_auto", "Automatically created time series to combine", 
                             choices = as.list(names(values$tslist)), inline=TRUE)
    updateSelectInput(session, "sel_deletablets", "Select time series to delete", choices = create_ts_namelist(TRUE))
    
    if (is.null(values$objects) && is.null(values$tags)) {
      output$html_tshelp <- renderUI({
        withTags(
          tagList(
            p("Some time series were automatically created from loaded dataset."),
            p("Your dataset doesn't include Object and Tag fields. Variable names are used as default time series names.")
          )
        )
      })
    } else {
      output$html_tshelp <- renderUI({
        withTags(
          tagList(
            p("Some time series were automatically created from loaded dataset."),
            p("To avoid overlapping names, automatically created time series were renamed to the following format:"
              ,i("OxTy variable")),
            p(
              strong("Objects: "),
              paste0(sapply(values$objects, function(x) paste0("O",which(values$objects==x)," = ",x)),collapse=", ")
            ),
            p(
              strong("Tags: "),
              paste0(sapply(values$tags, function(x) paste0("T",which(values$tags==x)," = ",x)),collapse=", ")
            )
          )
        )
      })
    }
  }
  
  # Creates and saves new time series to values$tscreated
  observeEvent(input$btn_savets,{
    if (is.null(values$tslist)) return()
    
    #save all input fields
    ts <- get_selected_ts(input$sel_filterts)
    editchoice <- as.integer(input$radio_tseditchoice)
    # aggregation choices
    aggr <- input$sel_aggregation
    createtimes <- input$cb_createtimes
    aggrperiod <- as.integer(input$num_everynperiod)
    aggrmethod <- input$sel_aggregationmethod
    # calculation choices
    calculation <- input$sel_calculation
    usercalc <- as.character(input$txtin_usertscalculation)
    rollfunction <- input$sel_rollfunction
    rollingwindow <- as.integer(input$num_rollingwindow)
    rolldirection <- input$radio_rightleftroll
    # smoothing choices
    smoothmethod <- input$sel_tssmoothmethod
    # decompose choices
    decomposemethod <- input$sel_decomposemethod
    # lag choices
    lagk <- as.integer(input$num_lag)
    # general choices
    emptyvalues <- as.integer(input$sel_emptyvalues)
    emptyvalue <- as.integer(input$num_emptyvalue)
    dropduplicatetime <- input$cb_tsdropsametimestamp
    rounding <- input$cb_round
    decimals <- input$num_decimals
    variablename <- input$txtin_newvariablename
    name <- as.character(input$intxt_ts_name)
    
    # 1 = aggregate, 2 = calculate, 3 = smooth, 4 = decompose, 5 = lag
    if (editchoice==1) {
      tsagg <- lapply(ts, aggregate_timeseries, aggr, aggrperiod, aggrmethod, createtimes)
      if (length(tsagg) == 1) {
        ts <- tsagg[[1]]
      } else if (length(tsagg) > 1) {
        ts <- do.call("merge",tsagg)
        names(ts) <- names(tsagg)
      }
    } else if (editchoice == 2) {
      calc <- as.character(input$sel_calculation)
      if (calc == "user") {
        error_occured <- FALSE
        tryCatch ({
          f <- function(x) { eval(parse(text=usercalc)) }
          tsnew <- f(ts)
          if (!is.xts(tsnew)) {
            stop("Function didn't return valid time series object!")
          } else {
            ts <- tsnew
          }
        }, error = function(err) {
          msg <- paste0("Error: ",conditionMessage(err))
          print_to_output("txt_ts_filtered", msg, leading=FALSE, newline=TRUE, append=TRUE)
          error_occured <- TRUE
        })
        if (error_occured) {
          msg <- paste0("Stopping time series creation!")
          print_to_output("txt_ts_filtered", msg, leading=FALSE, newline=TRUE, append=TRUE)
          return()
        } else {
          msg <- paste0("Calculation valid. Continuing time series creation.")
          print_to_output("txt_ts_filtered", msg, leading=FALSE, newline=TRUE, append=TRUE)
        }
      } else if (calc == "roll") {
        if (rollfunction == "mean") {
          ts <- rollmean(ts, k=rollingwindow, align=rolldirection)
        } else if (rollfunction == "median") {
          if (rollingwindow %% 2 == 0) {
            rollingwindow <- rollingwindow + 1
          }
          ts <- rollmedian(ts, k=rollingwindow, align=rolldirection)
        } else if (rollfunction == "max") {
          ts <- rollmax(ts, k=rollingwindow, align=rolldirection)
        } else if (rollfunction == "sum") {
          ts <- rollsum(ts, k=rollingwindow, align=rolldirection)
        }
      } else if (calc == "log") {
        ts <- log(ts)
      } else if (calc == "sqrt") {
        ts <- sqrt(ts)
      } else if (calc == "diff") {
        ts <- diff(ts)
      }
    } else if (editchoice == 3) {
      
    } else if (editchoice == 4) {
      
    } else if (editchoice == 5) {
      ts <- lag(ts, k=lagk, na.pad=TRUE)
    }
    if (dropduplicatetime) {
      ts <- make.index.unique(ts, drop=TRUE)
    }
    if (rounding) {
      ts <- round(ts, decimals)
    }
    if (emptyvalues == 2) {
      #drop empty values
      ts <- na.omit(ts)
    } else if (emptyvalues == 3) {
      #use given value
      ts <- na.fill(ts, emptyvalue)
    } else if (emptyvalues == 4) {
      #use previous
      ts <- na.locf(ts, na.rm=TRUE)
    } else if (emptyvalues == 5) {
      #use next
      ts <- na.locf(ts, na.rm=TRUE, fromLast=TRUE)
    } else if (emptyvalues == 6) {
      #interpolate
      ts <- na.approx(ts, na.rm=FALSE)
    }
    # rename single variable time series
    if (ncol(ts) == 1) {
      names(ts) <- variablename
    }
    # Save new time series and update page
    if (is.null(ts) || ncol(ts) == 0 || nrow(ts) < 1) {
      print_to_output("txt_ts_filtered", "Failed to create time series!", append=TRUE)
      return()
    } else if (ncol(ts) == 1) {
      print_to_output("txt_ts_filtered", "Created single variable time series", append=TRUE)
      values$tscreated[[name]] <- ts
    } else {
      print_to_output("txt_ts_filtered", "Created multiple variable time series", append=TRUE)
      values$tscombined[[name]] <- ts
    }
    create_ts_page()
  })
  
  # Creates and saves new random time series to values$tscreated
  observeEvent(input$btn_saverandom, {
    if(is.null(values$tscreated))
      values$tscreated <- list()
    from <- force_tz(as_datetime(input$dates_randomts[1]), tzone="Europe/Helsinki")
    to <- force_tz(as_datetime(input$dates_randomts[2]), tzone="Europe/Helsinki")
    interval <- as.character(input$sel_randinterval)
    rmethod <- input$sel_randommethod
    name <- input$intxt_randomts_name
    ts <- seq(from, to, interval)
    nr <- length(ts)
    if (rmethod == 1) {
      vals <- cumsum(sample(c(-1, 1), nr, TRUE))
    } else if (rmethod == 2) {
      p <- as.integer(input$num_rarimap)
      if(p>0){ ars <- runif(p,-1,1) } else { ars <- NULL }
      d <- as.integer(input$num_rarimad)
      q <- as.integer(input$num_rarimaq)
      if(q>0){ mas <- runif(q,-1,1) } else { mas <- NULL }
      vals <- as.vector(arima.sim(model=list(order = c(p,d,q), ar = ars, ma = mas), n = nr-d))
    }
    tsobj <- xts(vals, ts, tzone="Europe/Helsinki")
    names(tsobj) <- name
    values$tscreated[[name]] <- tsobj
    print_to_output("txt_ts_filtered", "Created random time series", append=FALSE)
    print_to_output("txt_ts_filtered", head(values$tscreated[[name]]))
    create_ts_page()
  })
  
  # Combines time series and saves to values$tscombined
  observeEvent(input$btn_combinets, {
    creatednames <- input$cbg_combine_created
    autonames <- input$cbg_combine_auto
    name <- as.character(input$txtin_combinedtsname)
    if (nchar(name) < 1) return()
    method <- switch(as.integer(input$sel_combine_type), "outer", "inner")
    combinedts <- NULL
    for (i in creatednames) {
      if (is.null(combinedts)) {
        combinedts <- values$tscreated[[i]]
      } else {
        combinedts <- merge(combinedts, values$tscreated[[i]], join=method)
      }
    }
    for (i in autonames) {
      if (is.null(combinedts)) {
        combinedts <- values$tslist[[i]]
      } else {
        combinedts <- merge(combinedts, values$tslist[[i]], join=method)
      }
    }
    if (!is.null(combinedts)) {
      varnametype <- input$sel_combinedvarnames
      if (varnametype == "old") {
        createdvars <- NULL
        for (i in creatednames) {
          createdvars <- c(createdvars, names(values$tscreated[[i]])[1])
        }
        autovars <- NULL
        for (i in autonames) {
          autovars <- c(autovars, names(values$tslist[[i]])[1])
        }
        names(combinedts) <- c(createdvars, autovars)
      } else if (varnametype == "ts") {
        names(combinedts) <- c(creatednames, autonames)
      }
      
      values$tscombined[[name]] <- combinedts
      print_to_output("txt_ts_filtered", "Created combined time series", append=TRUE)
      create_ts_page()
    }
  })
  
  # Deletes time series object
  observeEvent(input$btn_deletets, {
    delete_ts(input$sel_deletablets)
    msg <- paste0("Deleted time series: ", substring(input$sel_deletablets, 4))
    print_to_output("txt_ts_filtered", msg, append=TRUE)
    create_ts_page()
  })
  
  # Makes summary of time series and fills values
  observeEvent(input$sel_filterts, {
    if (is.null(values$tslist)) return()
    ts <- get_selected_ts(input$sel_filterts)
    if (!is.null(ts) && ncol(ts) > 0 && nrow(ts) > 0) {
      tsname <- substring(input$sel_filterts,4)
      tssummary <- create_ts_summary(ts, tsname)
      output$txt_ts_summary <- renderText({
        tssummary
      })
      shinyjs::hide("div_newtsvarname")
      if (ncol(ts) == 1) {
        shinyjs::show("div_newtsvarname")
        updateTextInput(session,"txtin_newvariablename", value=colnames(ts)[1])
      }
      updateTextInput(session, "intxt_ts_name", value=paste("[Edit]", tsname))
      output$plot_tsboxplot <- renderPlot({
        boxplot(as.matrix(ts), use.cols=TRUE, main="Time series distribution", xlab=names(ts))
      })
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_clearfilterconsole, {
    print_to_output("txt_ts_filtered", "", leading=FALSE, newline=FALSE, append=FALSE)
  })
  
  shinyjs::onclick("hider1", shinyjs::toggle(id = "hiding1", anim = TRUE))
  shinyjs::onclick("hider2", shinyjs::toggle(id = "hiding2", anim = TRUE)) 
  shinyjs::onclick("hider3", shinyjs::toggle(id = "hiding3", anim = TRUE)) 
  shinyjs::onclick("hider4", shinyjs::toggle(id = "hiding4", anim = TRUE)) 
  
  
  
  
  
  
  ##### Data summary page actions ####
  create_summary_page <- function() {
    isolate(summary_printer())
  }
  
  
  
  
  
  
  
  #### Graphs page actions ####
  create_graphs_page <- function () {
    nameslist <- create_ts_namelist(TRUE)
    updateSelectInput(session, "sel_graph_ts", label="Select time series to plot", choices = nameslist)
  }
  
  shinyjs::onclick("toggle_graphsettings", shinyjs::toggle(id = "graphsettings", anim = TRUE))    
  
  observeEvent(input$sel_graph_ts, {
    plot_timeseries()
  })
  
  observeEvent(input$btn_updateplot, {
    plot_timeseries()
    shinyjs::hide("graphsettings", anim=TRUE)
  })
  
  
  
  
  
  
  
  #### DataTable page actions ####
  
  # create datatables when opening tab
  create_tables_page <- function () {
    if (is.null(values$data)) 
      return()
    updateSelectInput(session, "sel_tstable", label=("Select time series to show"),choices = create_ts_namelist(TRUE))
  }
  
  observeEvent(input$sel_showtable, {
    if (input$sel_showtable == 1) {
      output$dt_table <- DT::renderDataTable({
        DT::datatable(values$data, extensions=c("Buttons"), 
                      options=list(dom="ifrtBlp", orderClasses= TRUE, pageLength=10,
                                   scrollX=TRUE, buttons=list("colvis", list(extend="collection", 
                                              buttons=c("csv","excel","pdf","print","copy"), text="Export visible page")),
                                   lengthMenu=list(c(10,20,50,-1), c(10,20,50, "All"))))
      },server= TRUE)
      return()
    } else {
      ts <- get_selected_ts(input$sel_tstable)
      if (is.null(ts)) {
        return()
      }
      output$dt_table <- DT::renderDataTable({
        DT::datatable(as.data.frame(ts), extensions=c("Buttons"), 
                      options=list(dom="ifrtBlp", orderClasses= TRUE, pageLength=10,
                                   scrollX=TRUE, buttons=list("colvis", list(extend="collection", 
                                                  buttons=c("csv","excel","pdf","print","copy"), text="Export visible page")),
                                   lengthMenu=list(c(10,20,50,-1), c(10,20,50, "All"))))
      },server= TRUE)
    }
  })
  
  observeEvent(input$sel_tstable, {
    if (input$sel_showtable == 1) {
      return()
    } else {
      ts <- get_selected_ts(input$sel_tstable)
      if (is.null(ts)) {
        return()
      }
      output$dt_table <- DT::renderDataTable({
        DT::datatable(as.data.frame(ts), extensions=c("Buttons"), 
                      options=list(dom="ifrtBlp", orderClasses= TRUE, pageLength=10,
                                   scrollX=TRUE, buttons=list("colvis", list(extend="collection", 
                                               buttons=c("csv","excel","pdf","print","copy"), text="Export visible page")),
                                   lengthMenu=list(c(10,20,50,-1), c(10,20,50, "All"))))
      },server= TRUE)
    }
  })
  
  
  
  
  
  
  
  #### Analysis page actions ####
  create_analysis_page <- function () {
    updateSelectInput(session, "sel_analysists", choices = create_ts_namelist(TRUE))
  }
  
  observeEvent(input$btn_executeown, {
    codetext <- input$txtin_useranalysis
    print_to_output("txtout_analysis", paste0("Running the following code:\n",codetext))
    result <- tryCatch({
      if (!is.null(session$userData$env)) {
        e <- session$userData$env
      } else {
        e <- new.env()
      }
      ts <- get_selected_ts(input$sel_analysists)
      plot(ts) # just to make sure recordPlot() doesn't generate errror
      assign("x", ts, e)
      parsed <- parse(text=codetext)
      out <- capture.output(source(exprs=parsed, local=e, spaced=TRUE, echo=TRUE))
      msgs <- paste0(paste0(out, collapse="\n"),"\n")
      session$userData$env <- e
      p <- NULL
      p <- recordPlot()
      if (!is.null(p)) {
        output$plot_analysis <- renderPlot({
          replayPlot(p)
        })
      }
      msgs
    }, error = function(e) {
      return(paste0("An error occured: ", conditionMessage(e),"\n",e,collapse="\n"))
    })
    print_to_output("txtout_analysis", result, newline=TRUE)
  })
  
  observeEvent(input$sel_analysisfunction, {
    func <- input$sel_analysisfunction
    updateTextAreaInput(session, "txtin_useranalysis", value=paste0(input$txtin_useranalysis,func,"\n"))
  }, ignoreInit=TRUE)
  
  observeEvent(input$btn_clearanalysis, {
    print_to_output("txtout_analysis", "", leading=FALSE, newline=FALSE, append=FALSE)
  })
  
  observeEvent(input$btn_clearscript, {
    updateTextAreaInput(session, "txtin_useranalysis", value="")
  })
}
