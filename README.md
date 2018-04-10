# TRACER

TRACER is a code base for cleaning and analysing the [DfE's National Pupil Database](https://www.gov.uk/government/collections/national-pupil-database) (NPD). It is written entirely in R.

The code base will currently allow you to:
 - import KS4 and KS5 data for 2011 - 17.
 - import data on Ethnicity (Maj|Min), Pupil Premium, IDACI, Gender, Provider Descriptions, EAL flag, SEN flag, 
 - match data between years to complete missing values (e.g. fields missing in KS5 that are present in KS4).
 - produce standard tables to represent uptake of subjects based on any given criteria.
 - automatically anonymise datasets.
 - a range of graphs and maps to represent provision and outcomes based on given criteria.
 
## Installation

1. Download TRACER folder structure
2. follow instructions in __/data__ folder to download
    * __../students__ NPD student datasets (restricted DfE data)
    * __../results__ NPD results datasets (restricted DfE data)
    * __../schools__ Most recent snapshot of Edubase provider list from: http://www.education.gov.uk/edubase/home.xhtml
    * __../qualifications__ Most recent JCQ snapshot of xxxxx
    * __../maps__ 
        * updated postcode information from [ordnancesurvey](https://www.ordnancesurvey.co.uk/opendatadownload/products.html), combine all the letters into one large csv file called _postcodes.csv_. [Instructions](http://webpierat.com/2011/05/23/merging-csv-files-using-the-command-line/) on how to do this with command line. (see [also](https://www.r-bloggers.com/gb-postcode-polygons-open-data/))
        * regional map of England from [sharegeo](https://www.sharegeo.ac.uk/handle/10672/50)
        * local education authority map of England from [statistics.gov.uk](http://geoportal.statistics.gov.uk/datasets/c4a62d87de9f4b6087cf5f1515d5a0c1_0?geometry=-8.141%2C54.005%2C4.933%2C55.897&uiTab=table&orderByFields=ctyua14nm+ASC_)
        * coastal map of England from [divagis](http://www.diva-gis.org/datadown)
    * __../filters__ results.json and students.json files allow you to specify which fields you wish to pull from the DfE provided datasets. The format is: 
         ```{ Keystage :{ 
                     Year :{ 
                         "grep string to match DFE field name" : "cleaned field name" 
                     },
           },
         }```
3. create cleaned versions of the student and results by running the following commands in __/code/Main.R__
   First load all the ```source("...")``` files, this will provide the commands needed to clean the datasets
   Second run ```Main(years, keystages)```, where ```years``` stores the two digit year numbers as a vector, and ```keystages``` lists which keystages you want to clean. For example: ```Main(years = c(12:17), keystages = c("KS4","KS5"))``` 
4. create any reports in __/reports__ folder (see _How to write a report_ below)
5. any updates to the code, consider comitting back to github

## Preparing your environment
We would recommend that you download and use [RStudio](https://www.rstudio.com/products/rstudio/download/) for your analysis. It's crossplatform, free and wonderful.

Depending on how you are storing you NPD dataset you might not have a networked machine. If this is the case you need to get the correct packages installed in RStudio before you start your analysis. (Requests from RStudio to talk to the CRAN server might well meet with a blank response!). The packages you need are as listed in the code below. If you run this code on a networked machine it will download _win.binary_ files to your __c:/tmp/packages/__ folder. Mac users need to set __type__ = _"mac.binary"_ and Linux set __type__ = _"source"_

```r
# with thanks to: https://www.mango-solutions.com/blog/installing-packages-without-the-internet
getPackages <- function(packs){
  packages <- unlist(
    tools::package_dependencies(packs, available.packages(),
                                which=c("Depends", "Imports"), recursive=TRUE)
  )
  packages <- union(packs, packages)
  packages
}

 packages <- getPackages(c("abind","assertthat","bitops","Cairo","caTools","colorspace",
                           "DBI","digest","dplyr","evaluate","formatR","gdtools","ggplot2",
                           "gridExtra","gtable","highr","htmltools","httr","knitr",
                           "labeling","lazyeval","magrittr","maptools","markdown","munsell",
                           "pander","plyr","purrr","R.methodsS3","R.oo","R.utils","R6",
                           "RColorBrewer","Rcpp","rgdal","rgeos","rmarkdown","scales",
                           "sp","stringi","stringr","svglite","tibble","tidyr","xtable","yaml"))
 
 download.packages(packages, destdir="C:/tmp/packages/", 
                   type="win.binary")
 ```
 Once all the files are downloaded transfer them to your NPD analysis machine and load them from there.

## How to write a report


