# Lake Thermal Regions

A local web version of this R shiny app is currently available using the web link provided, however, for speed and efficiency it is strongly recommended that the app is run from the version available on GitHub using the instructions provided below. 

https://shiny.maths-stats.gla.ac.uk/rhaggarty/LakeThermalRegions/

### Installing R Software

The app is run using R, free software which can be downloaded from https://cran.r-project.org/

R Studio is an Integrated Development Envrionment for R which can be downloaded (free) from https://rstudio.com/products/rstudio/

R has a vast number of well documented local extension packages available. Instructions on how to install such packages are provided at: https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/install.packages

### Running the Lake Thermal Regions App

For this R shiny app. For this app, you will need to install the following R packages and their dependencies if required. 

dplyr\
fda\
ggplot2\
gridExtra\
maps\
MASS\
RColorBrewer\
reshape2\
shiny\
shinythemes


Then run the following code to start the app.

```
library(shiny)
runGitHub(repo='LakeThermalRegions', username='ruth-odonnell') 
```
