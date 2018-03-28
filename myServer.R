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
  cat("Welcome to SaMi analysis service!\n")
  
  #### Load helper functions ####
  source("helper_functions.R", local = TRUE)
  
  
  
  
  
  
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
      msg <- "> Starting data load\n"
      shinyjs::html(id = "sconsole", html = msg)
      cat(msg)
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
          msg <- paste0("> Loaded file: ",file,"\n")
          cat(msg)
          shinyjs::html(id="sconsole",html=msg, add=TRUE)
          values$fromsami <- FALSE
          create_timeseries()
        } else {
          stop(paste0("Can't find file: ",file))
        }
        return()
      } else if (selected == 4) {
        if (file.exists(dataf)){
          values$data <- readRDS(dataf)
          msg <- paste0("> Loaded file: ",dataf,"\n")
          cat(msg)
          shinyjs::html(id="sconsole",html=msg, add=TRUE)
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
        msg <- "> Loading data from SaMi service\n"
        cat(msg)
        shinyjs::html(id="sconsole",html=msg, add=TRUE)
        query <- URLencode(query)
        msg <- paste0("> Load data using query:\n",query,"\n")
        cat(msg)
        shinyjs::html(id="sconsole",html=msg, add=TRUE)
        incProgress(0.1, detail="Downloading data")
        #download data and make df from JSON-data
        jsondata <- fromJSON(query)
        msg <- paste0("> ",capture.output(toc()),"\n")
        cat(msg)
        shinyjs::html(id="sconsole",html=msg, add=TRUE)
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
        msg <- paste0("> Measurement count:",rows,"\n")
        cat(msg)
        shinyjs::html(id="sconsole",html=msg, add=TRUE)
        msg <- paste0("> Starting data conversion\n","> This may take several minutes. Please wait...\n")
        cat(msg)
        shinyjs::html(id="sconsole",html=msg, add=TRUE)
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
        msg <- paste0("> ",capture.output(toc()),"\n")
        cat(msg)
        shinyjs::html(id="sconsole",html=msg, add=TRUE)
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
        msg <- paste0("> ",capture.output(toc()),"\n")
        cat(msg)
        shinyjs::html(id="sconsole",html=msg, add=TRUE)
        msg <- paste0("> ",capture.output(toc()),"\n")
        cat(msg)
        shinyjs::html(id="sconsole",html=msg, add=TRUE)
        msg <- paste0("> ",capture.output(toc()),"\n")
        cat(msg)
        shinyjs::html(id="sconsole",html=msg, add=TRUE)
        msg <- "> Data loaded succesfully!\n"
        cat(msg)
        shinyjs::html(id="sconsole",html=msg, add=TRUE)
        incProgress(0.1, detail="Done")
        Sys.sleep(1)
      })
      values$data <- tdf
      values$fromsami <- TRUE
      create_timeseries()
      saveRDS(tdf, dataf)
      msg <- paste0("> Wrote converted data to file: ",dataf,"\n")
      cat(msg)
      shinyjs::html(id="sconsole",html=msg, add=TRUE)
      write_csv(tdf, dataf_csv)
      msg <- paste0("> Wrote CSV to file: ",dataf_csv,"\n")
      cat(msg)
      shinyjs::html(id="sconsole",html=msg, add=TRUE)
    }, 
    warning = function(err) {
      msg <- paste0("> Warning: ",conditionMessage(err),"\n","> Load terminated\n")
      cat(msg)
      print(err)
      shinyjs::html(id="sconsole",html=msg, add=TRUE)
    },
    error = function(err) {
      msg <- paste0("> Error: ",conditionMessage(err),"\n","> Load terminated\n")
      cat(msg)
      print(err)
      shinyjs::html(id="sconsole",html=msg, add=TRUE)
    },
    finally = {
      msg <- paste0("> Loading data ended\n")
      cat(msg)
      shinyjs::html(id="sconsole",html=msg, add=TRUE)
      msg <- capture.output(print(mem_used()))
      msg <- paste0("> Memory in use: ", msg, "\n")
      cat(msg)
      shinyjs::html(id="sconsole",html=msg, add=TRUE)
    }
    )
  })
  
  
  # Creates download file and saves it
  output$btn_download <- downloadHandler(
    filename = function() {
      if (input$sel_downloadwhat == 1) {
        filu <- paste0("SaMi_data_",Sys.Date(),".",input$sel_download)
      } else {
        filu <- paste0(as.character(input$sel_tstodownload),"_",Sys.Date(),".",input$sel_download)
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
      selected <- input$sel_download
      if (selected == "csv") {
        write_excel_csv(ddata, file)
      } else if (selected == "xlsx") {
        write.xlsx(ddata, file, colNames = TRUE)
      } else if (selected == "rds") {
        write_rds(ddata, file)
      } else if (selected == "ods") {
        
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
    newdata <- newdata %>% filter(Object %in% objs) %>% filter(Tag %in% tags)
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
    if (all(c("Object", "Tag") %in% names(newdata))) {
      newdata <- droplevels(newdata)
      newdata <- group_by(newdata, Object, Tag)
    }
    if (overwrite) {
      saveRDS(newdata, dataf)
      write_csv(newdata, dataf_csv)
    }
    values$data <- newdata
    create_timeseries()
    update_filter_tab()
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
  
  
  
  
  #### Time series page actions ####
  create_ts_page <- function() {
    if(is.null(values$tslist))
      return()
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
            p("Your dataset doesn't include Object and Tag fields.")
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
    aggr <- input$sel_aggregation
    createtimes <- input$cb_createtimes
    aggrperiod <- as.integer(input$num_everynperiod)
    aggrmethod <- input$sel_aggregationmethod
    emptyaggr <- as.integer(input$sel_emptyvalues)
    emptyvalue <- as.numeric(input$num_emptyvalue)
    empty <- as.integer(input$sel_emptyvalues)
    calculation <- input$sel_calculation
    lag <- input$cb_lag
    lagvalue <- input$num_lag
    rounding <- input$cb_round
    decimals <- input$num_decimals
    name <- as.character(input$intxt_ts_name)
    
    if (aggr != "no") {
      if (ncol(ts) > 1) {
        # we have multiple time series
      } else {
        # we have one time series
        ts <- aggregate_timeseries(ts, aggr, aggrperiod, aggrmethod, emptyaggr, emptyvalue, createtimes)
      }
    } else {
      #no resampling
    }
    # Save new time series and update page
    values$tscreated[[name]] <- ts
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
    output$txt_randomcreated <- renderPrint({
      print(paste("Created time series:", name))
      print(head(values$tscreated[[name]]))
    })
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
      values$tscombined[[name]] <- combinedts
      create_ts_page()
    }
  })
  
  # Deletes time series object
  observeEvent(input$btn_deletets, {
    delete_ts(input$sel_deletablets)
    create_ts_page()
  })
  
  # Makes summary of time series and fills values
  observeEvent(input$sel_filterts, {
    if (is.null(values$tslist)) return()
    ts <- get_selected_ts(input$sel_filterts)
    updateTextInput(session,"txtin_newvariablename", value=colnames(ts)[1])
    tssummary <- create_ts_summary(ts)
    output$txt_ts_summary <- renderText({
      tssummary
    })
    if (!is.null(ts) && length(ts) > 0) {
      output$plot_tsboxplot <- renderPlot({
        boxplot(as.matrix(ts), use.cols=TRUE, main="Time series distribution", xlab=names(ts))
      })
    }
  }, ignoreInit = TRUE)
  
  

  
  
  
  
  
  
  
  
  
  ##### Data summary page actions ####
  create_summary_page <- function() {
    isolate(summary_printer())
  }
  
  
  
  
  
  
  
  
  
  
  
  #### Graphs page actions ####
  create_graphs_page <- function () {
    nameslist <- create_ts_namelist(TRUE)
    updateSelectInput(session, "sel_linegraph_ts", label=("Select time series to plot"), choices = nameslist)
  }
  
  observeEvent(input$sel_linegraph_ts, {
    ts <- get_selected_ts(input$sel_linegraph_ts)
    output$plot_line <- renderDygraph({
      dygraph(ts, main = "Plotted time series")
    })
  }, ignoreInit = TRUE)
  
  
  
  
  
  
  
  
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
    code <- function(x) { eval(parse(text=codetext)) }
    ts <- get_selected_ts(input$sel_analysists)
    print_to_output("txtout_analysis", paste0("Running the following code:\n",codetext,"\n\n"))
    result <- tryCatch({
      paste0(capture.output(code(ts), type=c("output", "message")), "\n", collapse="\n")
    }, error = function(e) {
      return(paste0("An error occured: ", conditionMessage(e),"\n",e,"\n", collapse="\n"))
    })
    print_to_output("txtout_analysis", result)
  })
  
  observeEvent(input$btn_clearanalysis, {
    shinyjs::html(id="txtout_analysis",html="", add=FALSE)
  })
}
