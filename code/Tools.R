library(ggplot2)
library(svglite)
library(rgeos) #for map simplification
library(rgdal) #for reading/writing geo files
library(Cairo)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(maptools)
# for reports
library(knitr)
library(pander)
library(xtable)
library(readr) # <- super fast csv reader


#folder <- "./Data/" #doesn't work for R markdown :(
folder <- getwd()


#year <- 14
#keystage <- "KS4"
#level <- c(310,311,320)

#timing#
# ptm <- proc.time()
# proc.time() - ptm

#read in the full results file and save out a clean one for quick analysis
outputCleanResults <- function(year, keystage){

  # load big CSV file and get relevant columns
  results <- loadStudentResults(year, keystage, NULL, n=0)

  write_csv(results,
            path = paste0(getwd(), "/data/cleaned/CleanResults_20", year, keystage, ".csv"),
            col_names=TRUE,
            append = FALSE)
  print(paste("written to", paste0(getwd(), "/data/cleaned/CleanResults_20", year, keystage, ".csv")))
  
  return(results)
}

#read in the full students file and save out a clean one for quick analysis
outputCleanStudents <- function(year, keystage, match=TRUE, from = 12, to = 16, matches=NULL){
  
  # load big CSV file and get relevant columns
  students <- loadStudents(year, keystage, n=0)
  
  # this allows missing data to be added to KS5 results from KS4 data
  if(match){
    students <- matchFieldsBetweenYears(data = students, from = from, to = to, matches)
  }
  
  write_csv(students,
            path = paste0(getwd(), "/data/cleaned/CleanStudents_20", year, keystage, ".csv"),
            col_names=TRUE,
            append = FALSE)
  
  print(paste("written to", paste0(getwd(), "/data/cleaned/CleanStudents_20", year, keystage, ".csv")))
  
  return(students)
}

outputCleanSpread <- function(students, results, schools, postcodes, level, year, keystage){
  spread <- getSpreadResultsPerStudent(students, results, schools, postcodes, level)
  
  write_csv(spread,
            path = paste0(getwd(), "/data/cleaned/CleanSpread_20", year, keystage, ".csv"),
            col_names=TRUE,
            append = FALSE)
  
  print(paste("written to", paste0(getwd(), "/data/cleaned/CleanSpread_20", year, keystage, ".csv")))
  
  return(spread)
}

#return a dataframe with the IDs, Names and n of the top 10 largest subjects
#include `subject` if it isn't present
buildSubjectList <-function(spreadResults, subject="X2610", subsize){

  #spreadResults <- Aresults15
  #get columns starting with X followed by at least one number (it denotes a subject ID)
  IDs <- sapply(spreadResults[ , grep("X[0-9]", colnames(spreadResults))],
                function(x) length(which(!is.na(x))) )


  IDs <- cbind(read.table(text = names(IDs)), IDs, row.names = NULL)
  colnames(IDs) <- c("ID", "n")
  #order by largest subjects
  IDs <- IDs %>% arrange(desc(n))

  #fetch subject names
  filename <- paste0(folder,"MappingCodes.txt")
  Mappings <- read.csv(filename, head=TRUE, sep="\t")
  Mappings$MAPPING <- paste0("X",Mappings$MAPPING)

  #TODO: really really really want to put this in a single lapply
  IDs$SubjectName <- unlist(
    lapply(IDs$ID,
           function(x)
             droplevels(Mappings[which(Mappings$MAPPING == x), ]$MAPPING_DESCRIPTION)))

  if(subject %in% IDs[1:subsize,]$ID){
    return(IDs[1:subsize,])
  }else{
    return(rbind(IDs[1:subsize-1,], IDs[c(IDs$ID == subject),]))
  }
}

#create a filter to be applied to loading CSVs, noting data types and those to skip
buildCSVfilter <- function(fields, filename, s="\t") {
  #find the number of columns in a file
  temp <- read_delim(filename, delim=s, col_names = TRUE, n_max=1000)
  cols <- ncol(temp)
  
  #build a string to record the columns needed
  ff <- ""
  
  for (x in c(1:cols))
  {
    if (x %in% fields)
    {
      #add datatype of field to keep
      ff <- paste0(ff, substr(typeof(temp[[x]]),1,1))
    }else{
      #add NULL/_ to final field list
      ff <- paste0(ff, "_")
    }
  }
  temp <- NULL
  
  
  return(ff)
  
  # #find the number of columns in a file
  # temp <- read.csv(filename, head=TRUE, sep=s, nrows = 1)
  # cols <- ncol(temp)
  # #print(names(temp))
  # 
  # #build a vector to load the file
  # ff <- c()
  # for (x in c(1:cols))
  # {
  #   if (x %in% fields)
  #   {
  #     #add NA to final field list
  #     ff <- append(ff, NA)
  #   }else{
  #     #add NULL to final field list
  #     ff <- append(ff, "NULL")
  #   }
  # }
  # temp <- NULL
  # return(ff)
}

#get packages on the current system so that we can quickly redploy an R instance
buildPackageList <- function(){
  pkgs <- installed.packages()[,1]

  packages <- unlist(
    tools::package_dependencies(pkgs, available.packages(),
                                which=c("Depends", "Imports"), recursive=TRUE))
  packages <- union(pkgs, packages)
  return(packages)

  #download.packages(packages, destdir="C:/tmp/packages",
  #                type="both", repos="http://cran.rstudio.com")
}

# get the columns Ids of a given vector of fields
getColumnIds <- function(fields, filename, s="\t") {
  fields$MatchString <- trimws(fields$MatchString)#trim the white spaces
  
  temp  <- read_delim(filename, delim=s, col_names = TRUE, n_max=1)
  
  #build a vector ff to load the field Ids
  ff <- c()
  for (x in c(1:length(fields$MatchString)))
  {
    id <- grep(fields$MatchString[x], colnames(temp))
    if (length(id) > 1){
      print(paste("DUPLICATE MATCH ERROR on ", fields$MatchString[x], ":", id, colnames(temp)[id]))
    }else{
        if(length(id) == 0){
          print(paste("Missing",fields$MatchString[x]))
        }else{
          print(paste("found",colnames(temp)[id]))
        }
    }
    ff <- append(ff, id)
  }
  
  return(ff)
  
  # fields$MatchString <- trimws(fields$MatchString)#trim the white spaces
  # 
  # temp  <- read.csv(filename, head=TRUE, sep=s , nrows=1)
  # 
  # #build a vector ff to load the field Ids
  # ff <- c()
  # for (x in c(1:length(fields$MatchString)))
  # {
  #   id <- grep(fields$MatchString[x], colnames(temp))
  #   if (length(id) > 1){
  #     print(paste("DUPLICATE MATCH ERROR on ", fields$MatchString[x], ":", id, colnames(temp)[id]))
  #   }else{
  #     if(length(id) == 0){
  #       print(paste("Missing",fields$MatchString[x]))
  #     }else{
  #       print(paste("found",colnames(temp)[id]))
  #     }
  #   }
  #   ff <- append(ff, id)
  # }
  # return(ff)
}

# replace the Ids of given fields using the fields datastructure: $MatchString | $NewName
replaceIds <- function(data, fields) {
  fields$MatchString <- trimws(fields$MatchString) #trim the white spaces
  fields$NewName <- trimws(fields$NewName) #trim the white spaces

  # update each field name
  for (x in c(1:length(fields$MatchString)))
  {
    colnames(data)[grep(fields$MatchString[x], colnames(data))] <- fields$NewName[x]
  }
  return(data)
}

# convert points into grades
convertGrades <- function(data, level="GCSE"){
  if(level == "GCSE"){
    data <- data %>%
      mutate(grade = case_when(.$grade == "0" ~ "U",
                               .$grade == "1" ~ "G",
                               .$grade == "2" ~ "F",
                               .$grade == "3" ~ "E",
                               .$grade == "4" ~ "D",
                               .$grade == "5" ~ "C",
                               .$grade == "6" ~ "B",
                               .$grade == "7" ~ "A",
                               .$grade == "8" ~ "*",
                               .$grade == "58" ~ "*",
                               .$grade == "52" ~ "A",
                               .$grade == "46" ~ "B",
                               .$grade == "40" ~ "C",
                               .$grade == "34" ~ "D",
                               .$grade == "28" ~ "E",
                               .$grade == "22" ~ "F",
                               .$grade == "16" ~ "G",
                               .$grade == "0"  ~ "U"))


  }else if(level == "A-Level"){
    data <- data %>%
      mutate(grade = case_when(.$grade == "300" ~ "*",
                               .$grade == "270" ~ "A",
                               .$grade == "240" ~ "B",
                               .$grade == "210" ~ "C",
                               .$grade == "180" ~ "D",
                               .$grade == "150" ~ "E",
                               .$grade == "0" ~ "X"))
  }

  return(data)

}

#return a total row to a basic results table
addTotalRow <- function(data, extrafields, grades= FALSE){
  # data <- output
  # extrafields <- c("Type")
  # grades
  
  
  #data <- LEAData
  # names(output)
  #extrafields <- c("LEA Name", "LEA Code")

  newRow <- data[c(1),]

  Name <- "Totals"
  TotalSchools <- sum(data$`SchTypeTotalSchools`)
  TotalStudents <- sum(data$`SchTotalStudents`)
  SubjectProviders <- sum(data$`SubTotalSchools`)
  ProvidersPer <- round(100 * sum(data$`SubTotalSchools`) / sum(data$`SchTypeTotalSchools`), digits=1)
  SubjectStudents <- sum(data$`SubTotalStudents`)
  StudentPer <- round(100 * sum(data$`SubTotalStudents`) / sum(data$`SchTotalStudents`), digits=1)
  AvgCohort <- round(sum(data$`SubTotalStudents`) / sum(data$`SubTotalSchools`), digits=1)
  
  size <- length(extrafields)

  newRow[,c(1)] <- Name

  newRow[,c(size + 1)] <- TotalSchools
  newRow[,c(size + 2)] <- TotalStudents
  newRow[,c(size + 3)] <- SubjectProviders
  newRow[,c(size + 4)] <- ProvidersPer
  newRow[,c(size + 5)] <- SubjectStudents
  newRow[,c(size + 6)] <- StudentPer
  newRow[,c(size + 7)] <- AvgCohort
  
  if(grades == TRUE){
    AvgGradeFocus <- round(sum(data$SchTotalGrade)/sum(data$SchTotalStudents), digits=1)
    AvgGradeSubStudents <- round(sum(data$SubStuAllGrades)/sum(data$SubTotalStudents), digits=1)
    AvgGradeSubStudents <- round(sum(data$SubTotalGrade)/sum(data$SubTotalStudents), digits=1)
    
    newRow[,c(size + 8)] <- AvgGradeFocus
    newRow[,c(size + 9)] <- AvgGradeSubStudents
    newRow[,c(size + 10)] <- AvgGradeSubStudents
  }
  
  #deal with any extra non-computable columns
  if(size == 2){
    if(is.integer(newRow[,c(size)][[1]])){
      newRow[,c(size)] <- 0
    }
    if(is.character(newRow[,c(size)][[1]])){
      newRow[,c(size)] <- ""
    }
    #TODO, deal with extra field types
    #typeof( newRow[,c(size)][[1]])
  }

  # data <- bind_rows(data, newRow)

  return(newRow)
}


anonymiseGraph <- function(data, limit = 6){

  #add anon column to print X where data is suppressed
  data$anon <- ifelse(data$total < limit, "X", "")
  #blank field that contains identifiable data
  data$total <- ifelse(data$total < limit, 0, data$total)

  if("percentage" %in% colnames(data)) {
    data$percentage <- ifelse(data$total < limit, 0, data$percentage)
  }

  return(data)
}
# get the codes of the 9 regions from the names
getRegionCodes <- function(data=NULL){

  string <- "Code,\"Region\"
  \"42353\",\"North East\"
  \"121915\",\"North West\"
  \"117537\",\"London\"
  \"59646\",\"West Midlands\"
  \"60802\",\"Yorkshire and The Humber\"
  \"122188\",\"South West\"
  \"52650\",\"East Midlands\"
  \"51776\",\"South East\"
  \"40935\",\"East of England\""

  RegionsIDs <- read.csv(text=string)

  if(!is.null(data)){
    RegionsIDs <- left_join(data, RegionsIDs)
  }

  return(RegionsIDs)
}


# Get the codes fo LEAs and map to data
getLocalEducationAuthorities <- function(data=NULL, dir=getwd()){
 
  if(!exists("LEAs")){
    # data <- region_data
    LEAs <- loadLocalEducationAuthorities(NULL,dir) %>%  
                select(`LA (name)`, `GSSLACode (name)`) %>% 
                distinct(`LA (name)`, `GSSLACode (name)`)
  }
  
  if(is.null(data)){
    return(LEAs)
  }
  
  data <- left_join(data, LEAs, by=c("Region" = "LA (name)")) %>% rename(Code = `GSSLACode (name)`)
  return(data)
}

# for a given table X out identifiable data
anonymiseTable<- function(table){

  #TODO: get this finished! : http://stackoverflow.com/questions/27027347/mutate-each-summarise-each-in-dplyr-how-do-i-select-certain-columns-and-give

  table %>% group_by_(name) %>%
    mutate_each(funs(.=as.character(ifelse(.<=5,"X",.))))

  return(table)
  #mutate_each_(funs(ifelse(.<=5,"X",.),-`Pupil premium`))

}

#mutate to add Major ethnicities
mutateEthnicity <- function(EthTotals){

  EthTotals <- EthTotals %>%
    mutate(EthName = case_when(.$EthMaj == ""       ~ "Missing",
                               .$EthMaj == "AOEG"   ~ "Other",
                               .$EthMaj == "ASIA"   ~ "Asian",
                               .$EthMaj == "CHIN"   ~ "Chinese",
                               .$EthMaj == "MIXD"   ~ "Mixed",
                               .$EthMaj == "UNCL"   ~ "Undeclared",
                               .$EthMaj == "WHIT"   ~ "White",
                               .$EthMaj == "BLAC"   ~ "Black",
                               .$EthMaj == "WHIBRI" ~ "White British",
                               .$EthMaj == "WHIOTH" ~ "White Other",
                               TRUE                 ~ "ERROR"))

  return(EthTotals)

}

#get missing stats on a giving field
missingStats <- function(dataSet, Column, level){

  #dataSet <- Aresults15
  #dataSet <- Gresults15
  #Column <- "EthMaj"
  #NewName <- "EthMaj"

  tally <- dataSet %>% group_by_(Column) %>% summarise(n = n())
  missingData <- tally %>% filter_(paste0(Column, " == \"\" | is.na(", Column,")")) %>% ungroup() %>% summarise(n=sum(n)) %$% n
  print(paste(level, "|", Column, "missing:", missingData, "or", round(100 * (missingData / sum(tally$n)), 2), "%"))

}

#get the health of each field in a table
healthOfDataTable <- function(df){
  df <- Students_Alevel_16
  fields <- names(df)
  sapply(fields, function(x) healthOfField(df,x) )
}

healthOfField <- function(df, field="X2610"){

  temp <- df %>% select(!!field)
  names(temp) <- c("target")

  temp <- temp %>% mutate(Blank = ifelse(target == "" | is.na(target), TRUE, FALSE)) %>%
    summarise(n = n(),
              missing = sum(Blank),
              present = sum(!Blank),
              missingper = missing / n)
  return(temp)
}

# get packages and dependencies for the system you are working on
getPackages <- function(packs){
  packages <- unlist(
    tools::package_dependencies(packs, available.packages(),
                                which=c("Depends", "Imports"), recursive=TRUE)
  )
  packages <- union(packs, packages)
  packages
}

# packages <- getPackages(c("abind","assertthat","bitops","Cairo","caTools","colorspace",
#                           "DBI","digest","dplyr","evaluate","formatR","gdtools","ggplot2",
#                           "gridExtra","gtable","highr","htmltools","httr","knitr",
#                           "labeling","lazyeval","magrittr","maptools","markdown","munsell",
#                           "pander","plyr","purrr","R.methodsS3","R.oo","R.utils","R6",
#                           "RColorBrewer","Rcpp","rgdal","rgeos","rmarkdown","scales",
#                           "sp","stringi","stringr","svglite","tibble","tidyr","xtable","yaml"))
# 
# 
# download.packages(packages, destdir="C:/Users/peter.kemp/Downloads/packages/", 
#                   type="win.binary")
# packages <- c("abind","assertthat","bitops","Cairo","caTools","colorspace",
#               "DBI","digest","dplyr","evaluate","formatR","gdtools","ggplot2",
#               "gridExtra","gtable","highr","htmltools","httr","knitr",
#               "labeling","lazyeval","magrittr","maptools","markdown","munsell",
#               "pander","plyr","purrr","R.methodsS3","R.oo","R.utils","R6",
#               "RColorBrewer","Rcpp","rgdal","rgeos","rmarkdown","scales",
#               "sp","stringi","stringr","svglite","tibble","tidyr","xtable","yaml")
# 
# install.packages(packages, repos=NULL)


#number suppression
#from: page 36: https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/472700/NPD_user_guide.pdf

