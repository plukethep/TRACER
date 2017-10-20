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
2. follow instructions in __/data__ folder to download: 
.. + __../students__  NPD student datasets for
.. + __../results__ NPD results datasets for 
.. + __../schools__ Most recent snapshot of Edubase provider list
.. + __../qualifications__ Most recent JCQ snapshot of xxxxx
.. + __../maps__ 
... + updated postcode information from gov.uk
... + regional map of England from xxxxx
... + local education authority map of England from xxxxx
... + coastal map of England from xxxxx
3. create cleaned versions of the student and results by running the following commands in __Main.R__
4. create any reports in __/reports__ folder (see _How to write a report_ below)
5. any updates to the code, consider comitting back to github

## How to write a report
