# Define UI for application
ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(title="SaMi Analysis"),
  dashboardSidebar(
    sidebarMenu(
      id="sidebar",
      menuItem("Data loader", tabName = "loader", icon = icon("dashboard")),
      menuItem("Edit time series", tabName = "filter", icon = icon("check")),
      menuItem("Data summary", tabName = "summary", icon = icon("file")),
      menuItem("Data plotting", tabName = "graphs", icon = icon("cog")),
      menuItem("Data table", tabName = "tables", icon = icon("table")),
      menuItem("Analysis", tabName = "analysis", icon = icon("flask")),
      menuItem("Help", tabName = "help", icon = icon("info"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName="loader",
              fluidRow(
                column(width=5,
                       box(title="Data loader", width=NULL, status="primary", solidHeader=TRUE,collapsible = TRUE,
                           radioButtons("query_select", h3("Select query type"),
                                        choices = list("Make a new query" = 1, "Paste your own query" = 2, 
                                                       "Use sample data" = 3, "Load saved data" = 4),selected = 1),
                           conditionalPanel(
                             condition = "input.query_select == 1",
                             textInput("txt_key", label="SaMi key (required)", placeholder="Your SaMi key"),
                             textInput("txt_object", label="Object", placeholder="Object to download"),
                             helpText("Empty returns all"),
                             textInput("txt_tag", label="Tag", placeholder="Tag to download"),
                             helpText("Empty returns all"),
                             textInput("txt_sensors", label="Sensors", placeholder="sensor1,sensor2..."),
                             helpText("Seperate names with commas"),
                             checkboxInput("cb_datesload", "Filter by dates", FALSE),
                             conditionalPanel(
                               condition = "input.cb_datesload",
                               dateRangeInput("in_date_range", label="Date range:", start=Sys.Date()-30, weekstart=1)
                             ),
                             selectInput("sel_filter", label="Measurements to take", 
                                         choices=list("All measurements"=1, "Latest n measurements"=2), selected=2),
                             conditionalPanel(
                               condition = "input.sel_filter == 2",
                               numericInput("num_latest", label="Measurements to take:", value=100)
                             ),
                             helpText("Maximum measurements",max_meas)
                           ),
                           conditionalPanel(
                             condition = "input.query_select == 2",
                             textAreaInput("txt_query", label="Query to make", value=samplequery,width="100%",rows=6)
                           ),
                           conditionalPanel(
                             condition = "input.query_select == 3",
                             selectInput("sel_exdata", label="Example dataset", 
                                         choices=list("Earthquakes"="earthquakes", "Global temperatures"="global_temperatures",
                                                      "Helsinki cyclists"="helsinki_cyclists", 
                                                      "Helsinki energy consumption"="helsinki_energy_consumption",
                                                      "Life expectancy by country"="life_expectancy",
                                                      "Nokia shares"="nokia_shares", 
                                                      "OMX Helsinki 25 index"="omxh25",
                                                      "Savonia weather data"="savonia_weather"), selected="savonia_weather"),
                             helpText("Select example dataset to test service."),
                             helpText("You can find more information about datasets from the examples/Example_info.txt file.")
                           ),
                           actionButton("btn_load", "Load data")
                       )),
                column(width=7,
                       infoBoxOutput("box_status", width=NULL),
                       tabBox(
                         title="Actions", id="tb_actions", width=NULL, selected="Export data", side="right",
                         tabPanel("Other actions",
                                  div(id="div_actions",
                                      h4("Other data actions"),
                                      div(id="actbuttons",
                                          actionButton("btn_acsummary", "Show summary"),
                                          actionButton("btn_actable", "Show in table"),
                                          actionButton("btn_acplot", "Plot data"), 
                                          actionButton("btn_acanalyse", "Analyse data")
                                      )
                                  )
                         ),
                         tabPanel("Filter data",
                                  div(id="div_filter",
                                      h4("Filter data"),
                                      helpText("You can drop data from loaded dataset using this page.",
                                              "Unselecting object/tag/sensor names drops those from current dataset."),
                                      helpText("Data aggregations and other calculations can be made on \"Edit time series\" tab."),
                                      checkboxGroupInput("cbg_filter", "General options", inline=FALSE,
                                                         choices=list("Drop variables with no numeric values" = 1, 
                                                                      "Drop measurements with no numeric values"=2,
                                                                      "Order variables by name"=3, 
                                                                      "Overwrite data file on disk"=4),
                                                         selected=c(1,2,3)),
                                      dateRangeInput("dates_filter", "Change date range"),
                                      checkboxGroupInput("cbg_objects", "Objects", inline=TRUE),
                                      checkboxGroupInput("cbg_tags", "Tags", inline=TRUE),
                                      checkboxGroupInput("cbg_sensors", "Sensors", inline=TRUE),
                                      actionButton("btn_filter", "Filter data")
                                  )
                         ),
                         tabPanel("Export data",
                                  div(id="div_download",
                                      h4("Export data"),
                                      selectInput("sel_downloadwhat", "What you want to export",
                                                  choices=list("Whole dataset"=1, "Single time series object"=2), selected=1),
                                      conditionalPanel(
                                        condition="input.sel_downloadwhat == 2",
                                        selectInput("sel_tstodownload", "Select time series", choices = NULL)
                                      ),
                                      selectInput("sel_download", "File format", 
                                                  choices=list("CSV file (.csv)"="csv", "Excel (.xlsx)"="xlsx", 
                                                               "OpenDocument (.ods)"="ods", "DIF file (.dif)"="dif",
                                                               "R object (.rds)"="rds", "JSON (.json)"="json"), selected="csv"),
                                      downloadButton("btn_download", "Save file")
                                  )
                         )
                       )
                )
              ),
              fluidRow(
                box(
                  title="Message console", width=12, status="warning", solidHeader=TRUE,collapsible = TRUE,
                  verbatimTextOutput("sconsole", placeholder=TRUE)
                )
              )
      ),
      tabItem(tabName="filter",
              fluidRow(
                column(width=6,
                       
                       box(title="Create or edit time series", width=NULL, status="primary", solidHeader=TRUE,collapsible = TRUE,
                           radioButtons("sel_ts_randorno", "Select one from below", 
                                        choices=list("Create from existing time series"=1, 
                                         "Create random time series"=2, "Create combined time series"=3, 
                                         "Delete time series"=4), selected=1),
                           conditionalPanel(
                             condition="input.sel_ts_randorno == 1",
                             selectInput("sel_filterts", "No data loaded!", choices=NULL),
                             radioButtons("radio_tseditchoice", "Choose what to do with time series", 
                                          choices=list("Clean up time series"=0, "Aggregate time series"=1, 
                                                       "Recalculate values"=2,"Smooth time series"=3, 
                                                       "Decompose"=4, "Lag time series"=5), 
                                          selected=0),
                             conditionalPanel(
                               condition = "input.radio_tseditchoice == 1",
                               selectInput("sel_aggregation", "Change time series granularity", 
                                           choices=list("Minutes"="mins", "Hours"="hours",
                                                        "Days"="days", "Weeks"="weeks","Months"="months",
                                                        "Quarters"="quarters","Years"="years"), selected="mins"),
                               numericInput("num_everynperiod", "Aggregate by n:th period", min=0, value=1, max=59),
                               helpText("For example, if granularity is set to 'Minutes', changing this to 5 makes 5 min intervals"),
                               checkboxInput("cb_createtimes", "Create missing times", TRUE),
                               helpText("Above option fills gaps in time series with chosen interval"),
                               selectInput("sel_aggregationmethod", "Select aggregation function", choices=list(
                                 "Count"="n", "Mean"="mean","Median"="median","Sum"="sum",
                                 "Max"="max","Min"="min","OHLC"="ohlc"), selected="mean"),
                               helpText("Function is used if there are multiple values in chosen interval")
                             ),
                             conditionalPanel(
                               condition = "input.radio_tseditchoice == 2",
                               selectInput("sel_calculation", "Value calculation", choices=list(
                                 "No calculations"="no", "Log"="log", "Sqrt"="sqrt", "Diff"="diff",
                                 "Arithmetic"="arit", "Rolling"="roll", "User defined"="user"),selected="no"),
                               conditionalPanel(
                                 condition="input.sel_calculation=='user'",
                                 textInput("txtin_usertscalculation", "Write your R command here", placeholder="sqrt(x)"),
                                 helpText("You can use 'x' as selected time series in calculation")
                               )
                             ),
                             conditionalPanel(
                               condition = "input.radio_tseditchoice == 3",
                               selectInput("sel_tssmoothmethod", "Select smoothing method to use",
                                           choices=list("Holt-Winters"="holtw", "Simple exponential"="simple",
                                                        "Holt's linear trend"="holtl"))
                             ),
                             conditionalPanel(
                               condition = "input.radio_tseditchoice == 4",
                               h4("No implementation yet!")
                             ),
                             conditionalPanel(
                               condition = "input.radio_tseditchoice == 5",
                               numericInput("num_lag", "Periods to shift data", value=0)
                             ),
                             tags$hr(),
                             tags$h3("Cleaning data"),
                             selectInput("sel_emptyvalues", "What to do with empty (NA) values", choices=list(
                               "Leave as they are"=1, "Drop"=2, "Use given value"=3, "Use previous"=4,
                               "Use next"=5, "Interpolate"=6),selected=2),
                             conditionalPanel(
                               condition = "input.sel_emptyvalues == 3",
                               numericInput("num_emptynumeric", "Value to use", value=0)
                             ),
                             checkboxInput("cb_tsdropsametimestamp", "Drop values with same timestamp", TRUE),
                             checkboxInput("cb_round", "Round up numeric values", FALSE),
                             conditionalPanel(
                               condition = "input.cb_round",
                               numericInput("num_decimals", "Decimals to keep", value=2, min=0)
                             ),
                             checkboxInput("cb_renametsvariable", "Rename variable", TRUE),
                             conditionalPanel(
                               condition = "input.cb_renametsvariable",
                               textInput("txtin_newvariablename", "Name for variable")
                             ),
                             textInput("intxt_ts_name", "Name for new time series", value="New time series"),
                             actionButton("btn_savets", "Save time series"),
                             verbatimTextOutput("txt_ts_filtered", placeholder = FALSE)
                           ),
                           conditionalPanel(
                             condition="input.sel_ts_randorno == 2",
                             dateRangeInput("dates_randomts", "Time series period"),
                             selectInput("sel_randinterval", "Change time series granularity", choices=list( 
                               "Minutely"="mins", "Hourly"="hours","Daily"="days",
                               "Weekly"="weeks","Montly"="months","Quarterly"="quarters","Yearly"="years"), selected=1),
                             selectInput("sel_randommethod", "Select randomizing function", choices=list(
                               "Random walk cumulative"=1, "ARIMA(p,d,q) simulation"=2), selected=1),
                             conditionalPanel(
                               condition = "input.sel_randommethod == 2",
                               numericInput("num_rarimap", "Autoregressive terms (p)", value=0, min=0, max=1),
                               numericInput("num_rarimad", "Nonseasonal differences (d)", value=0, min=0, max=1),
                               numericInput("num_rarimaq", "Moving average terms (q)", value=0, min=0, max=1)
                             ),
                             textInput("intxt_randomts_name", "Name for time series", value="Random time series"),
                             actionButton("btn_saverandom", "Save random time series"),
                             verbatimTextOutput("txt_randomcreated")
                           ),
                           conditionalPanel(
                             condition="input.sel_ts_randorno == 3",
                             checkboxGroupInput("cbg_combine_created", "User created"),
                             checkboxGroupInput("cbg_combine_auto", "Automatically created"),
                             selectInput("sel_combine_type", "How to combine", 
                                         choices=list("Keep all timestamps"=1, 
                                                       "Keep only common timestamps"=2), selected=1),
                             textInput("txtin_combinedtsname", "Name for combined time series", "Combined time series"),
                             actionButton("btn_combinets", "Combine time series")
                           ),
                           conditionalPanel(
                             condition="input.sel_ts_randorno == 4",
                             selectInput("sel_deletablets", "Time series to delete", choices=NULL),
                             actionButton("btn_deletets", "Delete time series")
                           )
                       )
                ),
                column(width=6,
                       box(title="Help", width=NULL, status="info", solidHeader=TRUE,collapsible = TRUE,
                           uiOutput("html_tspagehelp"),
                           h3("Automatically created time series"),
                           uiOutput("html_tshelp"),
                           h3("Selected time series summary"),
                           verbatimTextOutput("txt_ts_summary", placeholder = TRUE),
                           h3("Time series Boxplot"),
                           plotOutput("plot_tsboxplot", height="350px")
                       )
                )
              )
      ),
      tabItem(tabName="summary",
              fluidRow(
                box(title="Data summary", width=12, status="primary", solidHeader=TRUE,collapsible = TRUE,
                    htmlOutput("html_summary")
                )
              )
      ),
      tabItem(tabName="graphs",
              fluidRow(
                box(title="Line graph", width=12, status="primary", solidHeader=TRUE,collapsible = TRUE,
                    selectInput("sel_linegraph_ts", "No time series created!", choices=NULL),
                    dygraphOutput("plot_line")
                )
              )
      ),
      tabItem(tabName="tables",
              fluidRow(
                box(title="Data table", width=12, status="primary", solidHeader=TRUE,collapsible = TRUE,
                    selectInput("sel_showtable", "Select data to show", choices=list(
                      "Whole dataset" = 1, "Time series object"=2), selected=1),
                    conditionalPanel(
                      condition = "input.sel_showtable == 2",
                      selectInput("sel_tstable", "Select time series to show", choices=NULL)
                    ),
                    h4("Interactive data table"),
                    DT::dataTableOutput("dt_table")
                )
              )
      ),
      tabItem(tabName="analysis",
              fluidRow(
                box(title="Data analysis tool", width=12, status="primary", solidHeader=TRUE,collapsible = TRUE,
                    selectInput("sel_analysists", "Time series to analyse", choices=NULL),
                    radioButtons("radio_ownorno", "Choose one", 
                                 choices=list("Select analysis method from a list"=1, "Make your own R script"=2), selected = 1),
                    conditionalPanel(
                      condition = "input.radio_ownorno == 1",
                      selectInput("sel_analysisfunction", "Select analysis script to use", 
                                  choices=list("Decomposition"="decompose", "Autocorrelation"="acf", "Partial autocorrelation"="pacf",
                                               "Lagged correlation"="ccf", "Test mean reversion"="adf.test",
                                               "ARIMA"="arima", "Forecast"="forecast"), selected="decompose"),
                      actionButton("btn_analyse", "Run analysis")
                    ),
                    conditionalPanel(
                      condition = "input.radio_ownorno == 2",
                      helpText("Here you can write your own R script to execute. Anything is ok as long as it's valid R code."),
                      helpText("You can insert 'x' as selected time series object (e.g decompose(x))"),
                      textAreaInput("txtin_useranalysis", "Your own R script", rows=6),
                      actionButton("btn_executeown", "Execute")
                    ),
                    h4("Command console output"),
                    verbatimTextOutput("txtout_analysis", placeholder=TRUE),
                    actionLink("btn_clearanalysis", "Clear console"),
                    h4("Plot output"),
                    plotOutput("plot_analysis")
                )
                    
              )
      ),
      tabItem(tabName="help",
              fluidRow(
                box(title="Help", width=12, status="info", solidHeader=TRUE,collapsible = TRUE,
                    includeHTML("help_text.html")
                )
              )
      )
    )
  )
)
