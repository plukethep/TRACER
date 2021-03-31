# TRACER

TRACER is a code base for cleaning and analysing the [DfE's National Pupil Database](https://www.gov.uk/government/collections/national-pupil-database) (NPD). It is written entirely in R.

The code base will currently allow you to:
 - import KS4 and KS5 data for 2011-17,
 - import data on Ethnicity (Maj|Min), Pupil Premium, IDACI, Gender, Provider Descriptions, EAL flag, SEN flag, 
 - match data between years to complete missing values (e.g. fields missing in KS5 that are present in KS4),
 - produce standard tables to represent uptake of subjects based on any given criteria,
 - automatically anonymise output tables,
 - ouput a range of graphs and maps to represent provision and outcomes based on given criteria.
 
## Installation

1. Pull TRACER from github
2. follow instructions in __/data__ folder to download
    * __../students__ NPD student datasets (restricted DfE data). Rename files as follows:
        - KS4: KS4Pupil_YEAR_Census e.g. _KS4Pupil_2012_Census.txt_
        - KS5: KS5CandInd_YEAR_KS4_KS3_Census e.g. _KS5CandInd_2012_KS4_KS3_Census.txt_
    * __../results__ NPD results datasets (restricted DfE data). Rename files as follows:
        - KS4: KS4Results_YEAR e.g. _KS4Results_2012.txt_
        - KS5: KS5Results_YEAR e.g. _KS5Results_2015.txt_
    * __../schools__ Most recent snapshot of Edubase provider list from: https://get-information-schools.service.gov.uk/Downloads allowing you to pull information on school demographics
        * rename the file provided by Edubase to _schools.csv_
    * __../qualifications__ discount codes to map to results information. Document available from www.gov.uk/government/publications/2018-performance-tables-discount-codes. Download and store as: _SUBLEVNO.csv_
    * __../postcodes__ get up to date postcode information "Code-Point® Open" from [ordnancesurvey](https://www.ordnancesurvey.co.uk/opendatadownload/products.html), combine all the letters into one large csv file called _postcodes.csv_. [Instructions](http://webpierat.com/2011/05/23/merging-csv-files-using-the-command-line/) on how to do this with command line. (see [also](https://www.r-bloggers.com/gb-postcode-polygons-open-data/))
    * __../maps__ 
        * regional map of England from [ed.ac.uk](https://datashare.is.ed.ac.uk/handle/10283/2404)
        * local education authority map of England from [ed.ac.uk](https://datashare.is.ed.ac.uk/handle/10283/2532)
        * coastal map of England from [divagis](http://www.diva-gis.org/datadown)
    * __../filters__ _results.json_ and _students.json_ files allow you to specify which fields you wish to pull from the DfE provided datasets. The format is: 
```
     { Keystage :{ 
              Year :{ 
                   "grep string to match DFE field name" : "cleaned field name" e.g.
                   "KS4_URN$" : "URN",
              },
     },
    }
```

3. create cleaned versions of the student and results by running the following commands in __/code/Main.R__
   * First load all the ```source("...")``` files, this will provide the commands needed to clean the datasets
   * Second run ```Main(years, keystages)```, where ```years``` stores the two digit year numbers as a vector, and ```keystages``` lists which keystages you want to clean. For example: ```Main(years = c(12:17), keystages = c("KS4","KS5"))``` 
   * This process will produce three new datasets:
       - Spreads - one row per student, columns give demographic, schools and results data. Inefficient, but easy to use. only covers GCSE and Alevel data
       - Students - clean students only listing information specified by the _/filters_
       - Results - clean students only listing information specified by the _/filters_
   * If you have already cleaned the data, you can load the cleaned datasets into RAM by running: ```initialiseDataFrames(keystage, level,  two_digit_year)``` e.g. ```initialiseDataFrames("KS4", "GCSE", 17)```, this would create:
       - Spread_GCSE_17
       - Students_GCSE_17
       - Results_GCSE_17
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

## Codes used throughout:
### Level
Used to define the qualification level
310 == GCSE (A*-U), 391 == GCSE (9-1), 311 == Double GCSE, 320 == Alevel?

### MAPPING
Used to define the subject
2210 == Maths, 2610 == CS, 2650 == ICT

## How to write a report

# License
Creative Commons Attribution Share Alike 4.0
