# SaMi Analysis

This is a data exploration and transforming tool for [Savonia Measurements System (SaMi)](http://sami.savonia.fi/). Goal of this project is to help exploratory data analysis of [time series](https://en.wikipedia.org/wiki/Time_series) data (for example recorded sensor values). Tool can also be used for just loading and transforming SaMi data to different format or as basis for more general time series analysis web app.

## Features

- Written completely in R language as [Shiny](https://shiny.rstudio.com/) web app
- Application makes queries to SaMi public [HTTP API](https://sami.savonia.fi/Manage/Home/Help#json) and transforms returned JSON data to R dataset (data frame) and time series (xts) objects.
- Supports loading several Object-Tag combinations. Overlapping variable names are taken into account
- Created datasets and time series can be exported in various formats (CSV, Excel, RDS, JSON...)
- Some preprocessed datasets included for testing purposes
- Filter data and create or edit time series objects
- Some included time series operations are cleaning, various aggregations, calculations, smoothing and decomposing
- Data can be summarised, plotted and viewed in table
- Time series analysis can be done in application. Running own code in application is supported
- Implementing new features and fixing code is easy (if you know R language)

## Limitations

- Only numerical measurement data is supported. Non-numeric data is automatically dropped from dataset
- Not well suited for handling larger datasets (big data). Even 100 000 measurements may be too much for this application. Larger datasets need to be preprosessed on server side
- Currently not suitable for running on server in multiuser environment. Some changes need to be done to achieve that
- Many parts of code are quite messy, buggy and/or inefficient
- All planned features are not implemented
- There are some visible GUI elements that don't work yet (at the time of writing this)

## Getting started

You can install SaMi Analysis to your local computer (Linux, Windows or Mac) by following these instructions. Installing to server environment is not tested. Some changes may need to be done before installing on Shiny Server or similar. Current version allows running arbitrary user code which is not recommended on server.

### Installing on Windows with installer

The easiest way to get started on a Windows machine is to use ready to run installer (Download).

Download and run the installer. The installer will guide you through the installation progress. The default installation folder is user's Documents folder, so installation shouldn't require administrator rights. 

After installation run SaMi Analysis. Some packages still need to be installed on the first run. This may require many minutes.

If installer doesn't work for you, use the following instructions.

### Installing and running with RStudio

The following instructions can be used to install on different systems (Windows, Linux, Mac).

You need to install [R environment](https://cloud.r-project.org/) to run this application. [RStudio Desktop](https://www.rstudio.com/products/rstudio/download/#download) is also recommended, but not required. For Windows machine, you may need to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) or all the features may not work. Try installing it if you get errors during installation. On Linux you need to install r-base and r-base-dev.

The following instructions assume that you have [R](https://cloud.r-project.org/) and [RStudio](https://www.rstudio.com/products/rstudio/download/#download) installed on your computer. 

Start RStudio and ensure that it finds the R environment. Run the following in the RStudio "Console" window and check that R is installed.

```
R.version
```
Download SaMi Analyse project from GitHub if you haven't done so already (here's the latest ZIP).

Unzip master.zip to some folder and open "sami_analysis.Rproj" from that folder to RStudio (File -> Open Project...).

From RStudio Console run the following command. This installs [checkpoint](https://cran.r-project.org/web/packages/checkpoint/vignettes/checkpoint.html)-package which is in turn used to install other required packages to the project folder. The first run will take some time because it installs many packages, but subsequent runs will be much faster. Only checkpoint-package is installed globally to your R system and other packages are used only in this application. Installing required packages may take as long as 30 minutes on the first run. Wait patiently. A browser window will open after installation.

```
> source("run.R")
```

You can end Shiny session by pressing red "stop sign" in the upper right corner of Console or "Esc" button of your keyboard when Console is active.


### Installing and running with RStudio alternative method

Alternatively to the abowe you can install all packages to your R environment to be able to use in other projects. In that case you can run the following command. Note that this command installs latest versions of packages which may not be compatible with older applications and cause strange bugs. Using checkpoint installs package versions available on set day.

```
> install.packages(c("shiny", "shinydashboard", "shinyjs", "jsonlite","tidyverse",
                    "dygraphs", "highcharter", "zoo", "xts", "DT","openxlsx", "pryr",
                    "forecast", "TTR", "quantmod", "KernSmooth", "tictoc"))
> runApp(launch.browser=TRUE)
```
You can download and open SaMi Analysis package in RStudio as described before. You can then start application by the following command.

```
> shiny::runApp(launch.browser=TRUE)
```
You don't have to download application manually. There is a command that can do downloading for you and by default delete files after you are done. If you want to keep files, you can add the following option that saves files to "samianalysis" directory in your current working directory: `destdir = "samianalysis"`. Then you can `setwd("samianalysis")` to move to new directory and use the above command to run app.

```
> runGitHub("sami-analysis", "savoniauas")
```

You can end Shiny session by pressing red "stop sign" in the upper right corner of RStudio Console or by pressing "Quit Shiny" button in running app. 

### Installing and running with command line on Linux

You can use the following commands to install SaMi Analysis on a Linux system. Windows command line installation is quite similar, but not covered here. These instructions assume that you have basic understanding of using command line in Linux.

Install r-base and r-base-dev to your Linux system. You can find more information from [R homesite](https://cloud.r-project.org/). Below is an example on Ubuntu/Debian based system.

```
sudo apt-get install r-base r-base-dev
```
Create a directory for the project and download project from GitHub. Below is an example.

```
sudo apt-get install git
git clone https://github.com/SavoniaUAS/SaMi-analysis.git
cd SaMi-analysis

```
Created folder contains a file called `run.R` which is a script to install required R packages and start a Shiny session. There are many ways to run this script. Below is two of them. Answer 'y' to possible questions.

```
chmod +x run.R
./run.R
```
Alternatively you can run R session and start file from there. You can quit R session via q() command or if the Shiny is running, use Ctrl-Z.

```
R
> source("run.R")
```
Installing packages may require a long time on the first run. This script installs a snapshot of package versions used when making this application. Required packages are installed to project folder (in subfolder .checkpoint).

When installation is over, SaMi Analysis application should start automatically to the following address: [http://127.0.0.1:5470](http://127.0.0.1:5470)

## Built With

* [Dropwizard](http://www.dropwizard.io/1.0.2/docs/) - The web framework used
* [Maven](https://maven.apache.org/) - Dependency Management
* [ROME](https://rometools.github.io/rome/) - Used to generate RSS Feeds

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

