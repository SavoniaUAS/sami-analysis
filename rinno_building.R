library(RInno)
# Uncomment if you need to install inno setup
#RInno::install_inno()

packages <- c(
  shiny = "1.0.5",
  shinydashboard = "0.6.1",
  "shinyjs",
  jsonlite = "1.5",
  tidyverse = "1.2.1",
  "dygraphs",
  "highcharter",
  "tictoc",
  zoo = "1.8-1",
  "xts",
  "forecast",
  "TTR",
  "quantmod",
  "KernSmooth",
  "pryr",
  "DT",
  "openxlsx"
)

create_app(
	app_name = "SaMi Analysis",
	app_dir = getwd(),
	app_icon = "savonia_icon.ico",
	dir_out = "installer",
	publisher = "Savonia",
	pub_url   = "sami.savonia.fi",
	default_dir = "userdocs",
	privilege = "lowest",
	pkgs = packages,
	include_R = TRUE,
	R_version = "3.4.4"
)

compile_iss()
