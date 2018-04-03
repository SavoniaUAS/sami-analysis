# SaMi Analysis

This is a data exploration and transforming tool for [Savonia Measurements System (SaMi)](http://sami.savonia.fi/). Goal of this project is to help exploratory data analysis of [time series](https://en.wikipedia.org/wiki/Time_series) data (for example recorded sensor values). Tool can also be used for just loading and transforming SaMi data to different format or as basis for more general time series analysis web app.

There are some screenshots in the "screenshots" folder.

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
- Not well suited for handling larger datasets (big data). Even 100 000 measurements may be too much for this application. Larger datasets need to be preprosessed (on server side or other program)
- Currently not suitable for running on server in multiuser environment. Some changes need to be done to achieve that
- Many parts of code are quite messy, buggy and/or inefficient
- All planned features are not implemented
- There are some visible GUI elements that don't work yet (at the time of writing this)

## Getting started

You can install SaMi Analysis to your local computer (Linux, Windows or Mac) by following these instructions. Installing to server environment is not tested. Some changes may need to be done before installing on Shiny Server or similar. Current version allows running arbitrary user code which is not recommended on server.

### Installing on Windows with installer

The easiest way to get started on a Windows machine is to use ready to run installer. Installer includes R environment (v3.4.4) and required R libraries. If installer doesn't work for you, try some other method below.

1. [Download](https://github.com/SavoniaUAS/sami-analysis/releases/download/v0.8.2/SaMi_Analysis_installer_win_x64_v0.8.2_full.exe) and run the installer.
2. Application is installed to Windows user's "Documents" folder by default. Installation shouldn't require administrator rights. Installing application requires about 500 Mt of hard disk space
3. Unpacking libraries may require many minutes. Wait till installation is over
4. You can run SaMi Analysis after installation is over or from start-menu
5. Running SaMi Analysis opens a web-browser window. You can close the application simply by closing this window
6. Program state and time series objects are not saved between sessions so make sure you download data when you are done!
7. You can uninstall SaMi Analysis and R environment from Windows control panel

### Installing and running with RStudio

The following instructions can be used to install on different systems (Windows, Linux, Mac).

1. You need to install [R environment](https://cloud.r-project.org/) to run this application. [RStudio Desktop](https://www.rstudio.com/products/rstudio/download/#download) is also recommended, but not required. For Windows machine, you may need to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) or all the features may not work (for example Excel export). Try installing it if you get errors during installation. On Linux you need to install r-base and r-base-dev.
  
    The following instructions assume that you have [R](https://cloud.r-project.org/) and [RStudio](https://www.rstudio.com/products/rstudio/download/#download) installed on your computer. 

2. Start RStudio and ensure that it finds the R environment. Run the following in the RStudio "Console" window and check that R is installed.
  
```
R.version
```
3. Download SaMi Analyse project from GitHub if you haven't done so already.

4. Unzip master.zip to some folder and open `sami_analysis.Rproj` from that folder to RStudio (File -> Open Project...). You can check the current project directory by the following command.
  
```
getwd()
```

5. Install required R packages. 
  
    There are two ways to achieve this. You can install the latest packages globally or install snapshot of packages to be used only in this project. Latest packages may not be backwards-compatible with packages used in this project and cause unexpected bugs. Pick one method first and try another method if the first one doesn't work.

  - **Install required packages globally**

      You can install all packages to your R environment to be able to use in other projects. You can achieve this by running the following command in RStudio console. 
  
```
> install.packages(c("shiny", "shinydashboard", "shinyjs", "jsonlite","tidyverse",
                    "dygraphs", "highcharter", "xts", "DT", "openxlsx", "pryr",
                    "forecast", "TTR", "quantmod", "KernSmooth", "tictoc"))
```

  - **Install snapshot of packages to the project folder**
  
    If you want to install packages used when this application was made, use the following command. This installs [checkpoint](https://cran.r-project.org/web/packages/checkpoint/vignettes/checkpoint.html)-package which is in turn used to install other required packages to the project folder. The first run will take some time because it installs many packages, but subsequent runs will be much faster. Only checkpoint-package is installed globally to your R system and other packages are used only in this application. Installing required packages may take as long as 30 minutes on the first run. Wait patiently. A browser window will open after installation.

```
> source("run.R")
```

6. Running the application

    If you used the second method above, you can run the application by using `source("run.R")`. 

    If you used the first method, you can use the following command.
  
```
> shiny::runApp(launch.browser=TRUE)

```

7. Stopping the application

    You can end Shiny session by pressing "Quit Shiny" in browser window or by pressing "stop sign" in the upper right corner of Console.

### Installing and running with command line on Linux

You can use the following commands to install SaMi Analysis on a Linux system. Windows command line installation is quite similar, but not covered here. These instructions assume that you have basic understanding of using command line in Linux.

Install at least r-base and r-base-dev to your Linux system. You can find more information from [R homesite](https://cloud.r-project.org/). Below is an example on Ubuntu/Debian based system. On my test machine I also needed to install libblas-dev and liblapack-dev, but you may not need those.

```
sudo apt-get install r-base r-base-dev
sudo apt-get install libblas-dev liblapack-dev
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
Alternatively you can run R session and start file from there. You can quit R session via `q()` command or if the Shiny is running, use Ctrl-Z.

```
R
> source("run.R")
```
Installing packages may require a long time on the first run. This script installs a snapshot of package versions used when making this application. Required packages are installed to project folder (subfolder .checkpoint).

When installation is over, SaMi Analysis application should start automatically to the following address: [http://127.0.0.1:5470](http://127.0.0.1:5470)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

