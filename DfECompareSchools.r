require(readxl)
library(dplyr)
library(tidyr)
library(magrittr)

# download 2016 school summary data from: https://www.compare-school-performance.service.gov.uk/
# https://www.compare-school-performance.service.gov.uk/download-data?currentstep=datatypes&regiontype=all&la=0&downloadYear=2015-2016&datatypes=ks4underlying
# https://www.compare-school-performance.service.gov.uk/download-data?download=true&regions=0&filters=KS4UNDERLYING&fileformat=xls&year=2015-2016&meta=false

loadEntries <- function(filename){

  allEntries <- NULL
  for (x in c(1:50)){
    n <- 99 #set to show when sheet is blank
    print(x)

    #x<-4

    #find how many rows to skip (changes on qual type)
    for(y in c(3:0)){

      temp <- try(read_excel(filename, x, na = "",col_types = NULL, skip=y))

      if("Entries" %in% names(temp) || "Number of entries" %in% names(temp)){
        print(paste("skipping", y, "rows"))
        n <- y

        #check if vocational or standard qual for merging fields
        if(names(temp)[7] == "Entries" || names(temp)[7] == "Number of entries"){
          print("non Vocational")
          incfields <- c(1:7)
        }else{
          incfields <- c(1:6,8)
          print("Vocational")
        }
      }
    }

    if(class(temp) == "try-error"){
      break
    }

    if(n != 99){
      if (is.null(allEntries))
      {
        allEntries <- read_excel(filename,x,na = "",col_types = NULL, skip=n)[,incfields]
        names(allEntries)[7] <- "Entries" # so we can bind it
        names(allEntries)[4] <- "Provider Type" # so we can bind it
      }else{
        temp <- read_excel(filename,x,na = "",col_types = NULL, skip=n)[,incfields]
        names(temp)[7] <- "Entries" # so we can bind it
        names(temp)[4] <- "Provider Type" # so we can bind it
        allEntries <- rbind(allEntries, temp)
      }
    }else{
      print(paste("no results found on sheet", x))
    }
  }

  return(allEntries)
}

loadSchools <- function(filename){
  allSchools <- NULL

  #find how many rows to skip
  for(y in c(3:0)){
    
    temp <- try(read_excel(filename, 2, na = "",col_types = NULL, skip=y))
    
    if("LAESTAB" %in% names(temp)){
      print(paste("skipping", y, "rows"))
      n <- y
      allSchools <- try(read_excel(filename, 2, na = "",col_types = NULL, skip=y))
    }
  }
  return(allSchools)
}

calculateProviderStats <- function(students, schools, sub, qual){

  #students <- KS4Entries
  #schools <- KS4Schools
  #sub <- "Computer Studies/Computing"
  #qual <- "GCSE Full Course"

  compDF <- students %>%
    group_by(`Provider Type`) %>%
    mutate(compProviders =
             ifelse(Subject == sub & Qualification == qual, TRUE, FALSE),
           compStudents =  ifelse(Subject == sub & Qualification == qual, Entries, NA)) %>%
    summarise(`Total Providers` = length(unique(URN)),
              `Computing Providers` = sum(compProviders),
              `Computing Students` = sum(compStudents, na.rm=TRUE),
              `% of Providers` = 100 * ( `Computing Providers` / `Total Providers`),
              `Avg Cohort` = mean(compStudents, na.rm=TRUE),
              `Median Cohort` = median(compStudents, na.rm=TRUE))

  AllSchoolDF <- schools %>%
    group_by(`Institution Type`) %>%
    summarise(`Total Students` = sum(`Number of students at the end of KS4`))

  df <- left_join(compDF, AllSchoolDF, by = c("Provider Type" = "Institution Type"))
  df <- df %>% mutate(`% of Students` = 100 * (`Computing Students`/`Total Students`)) %>%
    filter(!is.na(`Provider Type`))
  df <- df %>% select(`Provider Type`, `Total Providers`, `Total Students`, `Computing Providers`,
                      `% of Providers`, `Computing Students`, `% of Students`,`Avg Cohort`, `Median Cohort`) %>%
    arrange(desc(`Total Students`))

  #add total row
  Totals <- df %>% ungroup() %>%
    summarise(`Provider Type` = "Total",
              `Total Providers` = sum(`Total Providers`),
              `Total Students` = sum(`Total Students`),
              `Computing Providers` = sum(`Computing Providers`),
              `% of Providers` = 100*(`Computing Providers` / `Total Providers`),
              `Computing Students` = sum(`Computing Students`),
              `% of Students` = 100*(`Computing Students` / `Total Students`),
              `Avg Cohort` = `Computing Students`/ `Computing Providers`,
              `Median Cohort` = NA
    )

  df <- rbind(df, Totals)

  df <- df %>% mutate(`% of Providers` = round(`% of Providers`,1),
                `% of Students` = round(`% of Students`,1),
                `Avg Cohort` = round(`Avg Cohort`,1),
                `Median Cohort` = round(`Median Cohort`,1))
  return(df)
}


##############
##############
####Ouptuts###
##############
##############

#file link to downloaded DfE data (data available here: https://www.compare-school-performance.service.gov.uk/)
KS4filename <- "C:/Users/Peter/Downloads/2015-2016-england_ks4underlying.xlsx"

#load national data into usable format
KS4Entries <- loadEntries(KS4filename)
KS4Schools <- loadSchools(KS4filename)

#num of schools offering GCSE in given subjects:
GCSEComp <- calculateProviderStats(KS4Entries, KS4Schools, "Computer Studies/Computing", "GCSE Full Course")
GCSEICT <- calculateProviderStats(KS4Entries, KS4Schools, "Information & Communications Technology", "GCSE Full Course")
GCSEPhysics <- calculateProviderStats(KS4Entries, KS4Schools, "Physics", "GCSE Full Course")

write.csv(GCSEComp, file = "c:/temp/2016compprovisionCOMP.csv")
write.csv(GCSEICT, file = "c:/temp/2016compprovisionICT.csv")
write.csv(GCSEPhysics, file = "c:/temp/2016compprovisionPHYSICS.csv")
