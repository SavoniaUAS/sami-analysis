#!/usr/bin/env Rscript

# This is a helper script to install packages and start Shiny

# Install checkpoint package to install other packages
if (!require(checkpoint))
  install.packages("checkpoint")
library(checkpoint)
# The following command downloads other required packages found in project folder
# The packages are installed to .checkpoint-folder in the current directory
checkpoint("2018-03-21", checkpointLocation = getwd())

library(shiny)
shiny::runApp(port=5470, launch.browser=TRUE)
