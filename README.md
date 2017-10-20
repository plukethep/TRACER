# TRACER

TRACER is a code base for cleaning and analysing the [DfE's National Pupil Database](https://www.gov.uk/government/collections/national-pupil-database) (NPD). It is written entirely in R.

The code base will currently allow you to:
 - import KS4 and KS5 data for 2011 - 16.
 - import data on Ethnicity (Maj|Min), Pupil Premium, IDACI, Gender, Provider Descriptions, EAL flag, SEN flag, 
 - match data between years to complete missing values (e.g. fields missing in KS5 that are present in KS4).
 - produce standard tables to represent uptake of subjects based on any given criteria.
 - automatically anonymise datasets.
 - a range of graphs and maps to represent provision and outcomes based on given criteria.
 
## Installation

1. Download TRACER folder structure
2. follow instructions in __/data__ folder to download
    * __../students__  NPD student datasets for
    * __../results__ NPD results datasets for 
    * __../schools__ Most recent snapshot of Edubase provider list from: http://www.education.gov.uk/edubase/home.xhtml
    * __../qualifications__ Most recent JCQ snapshot of xxxxx
    * __../maps__ 
        * updated postcode information from [ordnancesurvey](https://www.ordnancesurvey.co.uk/opendatadownload/products.html), combine all the letters into one large csv file called postcodes.csv. [Instructions](http://webpierat.com/2011/05/23/merging-csv-files-using-the-command-line/) on how to do this with command line. 
        * regional map of England from [sharegeo](https://www.sharegeo.ac.uk/handle/10672/50)
        * local education authority map of England from [statistics.gov.uk](http://geoportal.statistics.gov.uk/datasets/c4a62d87de9f4b6087cf5f1515d5a0c1_0?geometry=-8.141%2C54.005%2C4.933%2C55.897&uiTab=table&orderByFields=ctyua14nm+ASC_)
        * coastal map of England from [divagis](http://www.diva-gis.org/datadown)
3. create cleaned versions of the student and results by running the following commands in __Main.R__
4. create any reports in __/reports__ folder (see _How to write a report_ below)
5. any updates to the code, consider comitting back to github

## How to write a report
