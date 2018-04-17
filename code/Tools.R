library(ggplot2)
library(svglite)
library(rgeos) #for map simplification
library(rgdal) #for reading/writing geo files
library(Cairo)
library(plyr)
library(magrittr)
library(rlang)
library(tidyr)
library(dplyr)
library(maptools)
# for reports
library(knitr)
library(pander)
library(xtable)
library(readr) # <- super fast csv reader
library(ggrepel) # <- auto position labels
library(jsonlite)
library(reshape2)
library(purrr)
# library(readxl) # can't currently load this!?


#folder <- "./Data/" #doesn't work for R markdown :(
folder <- getwd()


#year <- 14
#keystage <- "KS4"
#level <- c(310,311,320)

#timing#
# ptm <- proc.time()
# proc.time() - ptm

#read in the full results file and save out a clean one for quick analysis
outputCleanResults <- function(year, keystage, n=0){

  # load big CSV file and get relevant columns
  results <- loadStudentResults(year, keystage, NULL, n)

  write_csv(results,
            path = paste0(getwd(), "/data/cleaned/CleanResults_20", year, keystage, ".csv"),
            col_names=TRUE,
            append = FALSE)
  print(paste("written to", paste0(getwd(), "/data/cleaned/CleanResults_20", year, keystage, ".csv")))

  return(results)
}

#read in the full students file and save out a clean one for quick analysis
outputCleanStudents <- function(year = 16,
                                keystage = "KS5",
                                what_to_match_person=NULL,
                                what_to_match_results=NULL,
                                match=TRUE,
                                from = 12,
                                to = 16,
                                matches_students = NULL,
                                matches_results = NULL,
                                n=0){

  # load big CSV file and get relevant columns
  students <- loadStudents(year, keystage, n)

  # this allows missing data to be added to KS5 results from KS4 data
  if(match){
    # match on student info
    if(!is.null(what_to_match_person)){
      students <- matchFieldsBetweenYears(data = students,
                                        from = from,
                                        to = year,
                                        year_dest = year,
                                        what_to_match_person = what_to_match_person,
                                        what_to_match_results = NULL,
                                        matches = matches_students)
    }
    #match on results info
    if(!is.null(what_to_match_results)){
      students <- matchFieldsBetweenYears(data = students,
                                          from = from,
                                          to = year,
                                          year_dest = year,
                                          what_to_match_person = NULL,
                                          what_to_match_results = what_to_match_results,
                                          matches = matches_results)
    }

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

  # subsize <- 30
  #spreadResults <- Aresults15
  #get columns starting with X followed by at least one number (it denotes a subject ID)
  IDs <- sapply(spreadResults[ , grep("X[0-9]", colnames(spreadResults))],
                function(x) length(which(!is.na(x))) )


  IDs <- cbind(read.table(text = names(IDs)), IDs, row.names = NULL)
  colnames(IDs) <- c("ID", "n")
  #order by largest subjects
  IDs <- IDs %>% arrange(desc(n))

  #fetch subject names
  filename <- paste0(basefolder,"/data/qualifications/MappingCodes.txt")
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

# returns fields for filtering on a given keystage, year and either students or results file
get_data_filter_fields <- function(ks, focus_year, type){
  focus_year <- paste0(20,focus_year)

  # read url and convert to data.frame
  url <- paste0(datafolder, "filters/",type,'.json')
  fltr <- fromJSON(txt=url, flatten = TRUE)

  # flatten the json into a single dataframe
  temp <- reshape2::melt(fltr) %>% select(L1, L2, L3, value)
  names(temp) <- c("Keystage", "Year", "MatchString", "NewName")

  # select the filter that is requested
  temp <- temp %>% filter(Keystage == ks, Year == focus_year) %>% select(MatchString, NewName)
  return(temp)
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
          message(paste("MISSING -",fields$MatchString[x]))
        }else{
          message(paste0(" +",colnames(temp)[id]))
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

# convert a spread to a one line per result observation
gather_spread <- function(data){
  cols <- names(data)[grepl("X",names(data))]
  cols <- c(cols, "PupilMatchingRefAnonymous")

  data <- data %>% select(cols) %>% gather(Subject, Grade, -PupilMatchingRefAnonymous, na.rm=TRUE)
  return(data)
}

# convert points into grades
convertGrades <- function(data, level="GCSE"){
  # data <- df
  
  if(level == "GCSE"){
    data <- data %>%
      mutate(grade = case_when(.$grade == 0 ~ "U",
                               .$grade == 1 ~ "G",
                               .$grade == 2 ~ "F",
                               .$grade == 3 ~ "E",
                               .$grade == 4 ~ "D",
                               .$grade == 5 ~ "C",
                               .$grade == 6 ~ "B",
                               .$grade == 7 ~ "A",
                               .$grade == 8 ~ "*",
                               .$grade == 58 ~ "*",
                               .$grade == 52 ~ "A",
                               .$grade == 46 ~ "B",
                               .$grade == 40 ~ "C",
                               .$grade == 34 ~ "D",
                               .$grade == 28 ~ "E",
                               .$grade == 22 ~ "F",
                               .$grade == 16 ~ "G",
                               .$grade == 0  ~ "U"))


  }else if(level == "Alevel"){
    data <- data %>%
      mutate(grade = case_when(.$grade == 300 ~ "*",
                               .$grade == 270 ~ "A",
                               .$grade == 240 ~ "B",
                               .$grade == 210 ~ "C",
                               .$grade == 180 ~ "D",
                               .$grade == 150 ~ "E",
                               .$grade == 0 ~ "U"))
  }

  return(data)

}

# and the other way around
convertGrades_letter_to_number <- function(data, level="GCSE"){
  if(level == "GCSE"){
    data <- data %>%
      mutate(grade = case_when(.$grade == "U" ~ 0,
                               .$grade == "G" ~ 1,
                               .$grade == "F" ~ 2,
                               .$grade == "E" ~ 3,
                               .$grade == "D" ~ 4,
                               .$grade == "C" ~ 5,
                               .$grade == "B" ~ 6,
                               .$grade == "A" ~ 7,
                               .$grade == "*" ~ 8))
    
    
  }else if(level == "Alevel"){
    data <- data %>%
      mutate(grade = case_when(.$grade == "*" ~ 300,
                               .$grade == "A" ~ 270,
                               .$grade == "B" ~ 240,
                               .$grade == "C" ~ 210,
                               .$grade == "D" ~ 180,
                               .$grade == "E" ~ 150,
                               .$grade == "U" ~ 0  ))
  }
  
  return(data)
  
}


# convert to selection criteria
convertSchType <- function(df){
  df <-  df %>% mutate(selective = ifelse(is.na(selective), "", as.character(selective))) %>%
                mutate(schType = ifelse(regexpr("special", schType) != -1, "Special",
                                 ifelse(regexpr("inde", schType) != -1, "Independent",
                                        ifelse(regexpr("Selective", selective) != -1, "Grammar",
                                               "Comprehensive"))))
  return(df)
}
  



#return a total row to a basic results table
addTotalRow <- function(data, extrafields, grades= TRUE){
  # data <- df
  # extrafields <- c(names(df)[1])
  # grades

  # sum(df$SubTotalStudents)

  #data <- LEAData
  # names(output)
  #extrafields <- c("LEA Name", "LEA Code")
  # TotalSchools <- sum(data$`SchTypeTotalSchools`)
  # TotalStudents <- sum(data$`SchTotalStudents`)
  # SubjectProviders <- sum(data$`SubTotalSchools`)
  # ProvidersPer <- round(100 * sum(data$`SubTotalSchools`) / sum(data$`SchTypeTotalSchools`), digits=1)
  # SubjectStudents <- sum(data$`SubTotalStudents`)
  # StudentPer <- round(100 * sum(data$`SubTotalStudents`) / sum(data$`SchTotalStudents`), digits=1)
  # AvgCohort <- round(sum(data$`SubTotalStudents`) / sum(data$`SubTotalSchools`), digits=1)
  # newRow[,c(size + 1)] <- TotalSchools
  # newRow[,c(size + 2)] <- TotalStudents
  # newRow[,c(size + 3)] <- SubjectProviders
  # newRow[,c(size + 4)] <- SubjectStudents
  # newRow[,c(size + 5)] <- ProvidersPer
  # newRow[,c(size + 6)] <- StudentPer
  # newRow[,c(size + 7)] <- AvgCohort

  newRow <- data[c(1),]

  newRow[,c(1)] <- "Totals"
  newRow$`SchTypeTotalSchools` <- sum(data$`SchTypeTotalSchools`)
  newRow$`SchTotalStudents` <- sum(data$`SchTotalStudents`)
  newRow$`SubTotalSchools` <- sum(data$`SubTotalSchools`)
  newRow$`% Schools` <- printper0(sum(data$`SubTotalSchools`) / sum(data$`SchTypeTotalSchools`), d=1)
  newRow$`SubTotalStudents` <- sum(data$`SubTotalStudents`)
  newRow$`% Students` <- printper0(sum(data$`SubTotalStudents`) / sum(data$`SchTotalStudents`), d=1)
  newRow$AverageCohort <- printper(sum(data$`SubTotalStudents`) / sum(data$`SubTotalSchools`), d=1)

  size <- length(extrafields)

  if(grades == TRUE){
    newRow$`Grade Avg Sch` <- printper(sum(data$SchTotalGrade)/sum(data$SchTotalStudents), d=1)
    newRow$`Grade Avg Sub Stu` <- printper(sum(data$SubStuAllGrades)/sum(data$SubTotalStudents), d=1)
    newRow$`Grade Avg Sub` <- printper(sum(data$SubTotalGrade)/sum(data$SubTotalStudents), d=1)

    # newRow[,c(size + 8)] <- AvgGradeFocus
    # newRow[,c(size + 9)] <- AvgGradeSubStudents
    # newRow[,c(size + 10)] <- AvgGradeSubStudents
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

  # data<-region_data

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
    mutate_all(funs(.=as.character(ifelse(.<=5,"X",.))))

  return(table)
  #mutate_each_(funs(ifelse(.<=5,"X",.),-`Pupil premium`))

}

# automatically size the columns in xtable for a given table
# first fields manually specifies the first few fields
autosizeColumns <- function(table, firstfields= c(1.7), page_width=7){
  #TODO: deal with crazy length decimals before they get cut by xtable

  # table <- table_Alevel_student_ethnicity_sub_comparison
  # firstfields= c(0.8)
  # page_width=3.8

  #take the field
  page_width <- page_width - (sum(firstfields))

  #convert table to UTF to avoid those odd french letters
  table <- as.data.frame(sapply(table, function(x) iconv(x, to="UTF-8")))
  widths <- c()

  #get the character widths for each of the columns
  for (x in c(1:length(names(table)))){
    # checks to see if there are any decimal points in each data column
    decimal <- ifelse(any(grepl("\\.", table[[x]])), 0.5, 0)
    widths[x] <- max(nchar(as.character(table[[x]])), na.rm = TRUE) - decimal
    message(x, " - ", max(nchar(as.character(table[[x]])), na.rm = TRUE), "-", decimal)
  }
  # find the long length names
  # table %>% group_by(Trust) %>% summarise(char = nchar(as.character(Trust))) %>% arrange(desc(char))

  # work out total characters excluding the field lengths provided
  totalchars <- sum(widths[c((length(firstfields)+1):length(widths))],
                    na.rm=TRUE)
  # work out equal weighting for each field based on width
  weighting <- sapply(widths[c((length(firstfields)+1):length(widths))], function(x) round((page_width / totalchars) * x, 2))

  # combine given and calculated widths into one vector
  # weighting <- c(firstfields,weighting)

  # create width string
  weighting <- c('l',
                 sapply(firstfields, function(x) paste0("p{", x, "in}")),
                 "|",
                 sapply(weighting, function(x) paste0("R{", x, "in}")))

  return(weighting)
}

# automatically select whether a decimal point is needed in xtable for a given table
# first fields manually specifies the first few fields
autodecimalColumns <- function(table, firstfields= c(0)){
  decimals <- c(0,firstfields) # add the first blank column to given columns
  if (length(firstfields) == length(names(table))){
    return(decimals)
  }else{
    #get the character widths for each of the columns
    for (x in c((length(firstfields) + 1):length(names(table)))){
      # checks to see if there are any decimal points in each data column
        decimals <- c(decimals, ifelse(any(grepl("\\.", table[[x]])), 1, 0))
    }
  return(decimals)
  }
}




RSorderNA <- function(table){
  zeros <- table %>% filter(as.numeric(.[[length(names(table))]]) == 0)
  nonzeroes <- table %>% filter(as.numeric(.[[length(names(table))]]) != 0 | is.na(.[[length(names(table))]]))
  table <- rbind(nonzeroes, zeros)
  return(table)
}

# build part of a Total row
RSTotalsPer <- function(table, x,y,z){

  #table <- left_join(LAPPSchStats, LAPPTable, by=c("Name"="Local Authority"))

  #copy n row to start off a new dataframe
  temp <- table[1,]

  temp[[x]] <- sum(table[[x]], na.rm=TRUE)
  temp[[y]] <- sum(table[[y]], na.rm=TRUE)
  temp[[z]] <- ifelse((temp[[x]] == 0 | temp[[y]] == 0), 0, round(100*(temp[[y]] / temp[[x]]), digits = 1))

  return(temp[,c(x,y,z)])
}

#return a Total Row for a given dataframe
RSaddTotalsToDF <- function(table, what){

  if(what=="five"){
    temp <- table[c(1),c(1,2)]
    temp[[1]] <- "TOTAL"
    temp[[2]] <- ""
    totals <- cbind(temp, RSTotalsPer(table, 3,4,5))
  }

  if(what=="six"){
    temp <- table[c(1),c(1,2,3)]
    temp[[1]] <- "TOTAL"
    temp[[2]] <- ""
    temp[[3]] <- ""
    totals <- cbind(temp, RSTotalsPer(table, 4,5,6))
  }

  if(what=="seven"){
    temp <- table[c(1),c(1)]
    temp[[1]] <- "TOTAL"
    totals <- cbind(temp, RSTotalsPer(table, 2,3,4), RSTotalsPer(table, 5,6,7))
  }

  if(what=="eight"){
    temp <- table[c(1),c(1,2)]
    temp[[1]] <- "TOTAL"
    temp[[2]] <- ""
    totals <- cbind(temp, RSTotalsPer(table, 3,4,5), RSTotalsPer(table, 6,7,8))
  }

  if(what=="nine"){
    temp <- table[c(1),c(1,2,3)]
    temp[[1]] <- "TOTAL"
    temp[[2]] <- sum(table[[2]])
    temp[[2]] <- sum(table[[3]])
    totals <- cbind(temp, RSTotalsPer(table, 4,5,6), RSTotalsPer(table, 7,8,9))
  }

  if(what=="ten"){
    temp <- table[c(1),c(1,2,3,4)]
    temp[[1]] <- "TOTAL"
    temp[[2]] <- sum(table[[2]])
    temp[[3]] <- sum(table[[3]])
    temp[[4]] <- sum(table[[4]])
    totals <- cbind(temp, RSTotalsPer(table, 5,6,7), RSTotalsPer(table, 8,9,10))
  }

  if(what=="YoY"){
    temp <- table[c(1),c(1:5)]
    temp[[1]] <- "TOTAL"
    temp[[2]] <- ""
    temp[[3]] <- ""
    temp[[4]] <- ""
    temp[[5]] <- ""
    #totals <- cbind(temp, RSTotalsPer(table, 6,7,8))
  }

  return(totals)
}

#anonymise any three columns where x = total students, y = subset of students, and z = %y of x
RSanonymiseColumns <- function(table, cutoff, x,y,z){

  #x <- 2
  #y <- 3
  #z <- 4
  table[[x]] <- ifelse(table[[x]] < cutoff & table[[x]] > 0,
                       -1,
                       table[[x]])

  table[[y]] <- ifelse(table[[y]] < cutoff & table[[y]] > 0,
                       -1,
                       ifelse((table[[x]] - table[[y]]) < cutoff &
                              (table[[x]] - table[[y]]) > 0,
                              -2,
                              table[[y]]))

  table[[z]] <- ifelse(table[[y]] == -1 | table[[y]] == -2, table[[y]], round(as.numeric(table[[z]]), digits=1))

  #format(num, nsmall = 2) to fix 1 DP

  # order, including X and Y
  table <- table %>% arrange(desc(.[[z]]))

  return(table)
}

# anonymise tables where student number given and number of schools
RSanonymiseColumnsSchools <- function(table, cutoff, schoolnum, x,y,z){

  #x <- 2
  #y <- 3
  #z <- 4
  #schoolnum <-
  table[[x]] <- ifelse(table[[x]] < (table[[schoolnum]] + cutoff - 1) &
                         table[[x]] > 0,
                       -1,
                       table[[x]])

  table[[y]] <- ifelse(table[[y]] < (table[[schoolnum]] + cutoff - 1) &
                         table[[y]] > 0,
                       -1,
                       ifelse((table[[x]] - table[[y]]) < (table[[schoolnum]] + cutoff - 1) &
                                (table[[x]] - table[[y]]) > 0,
                              -2,
                              table[[y]]))

  table[[z]] <- ifelse(table[[y]] == -1 | table[[y]] == -2, table[[y]], round(as.numeric(table[[z]]), digits=1))

  #format(num, nsmall = 2) to fix 1 DP

  # order, including X and Y
  table <- table %>% arrange(desc(.[[z]]))

  return(table)
}

# make sure that case of 100 students in school where 97 take computing is anonymised
RSanonymiseCrossCheck <- function(table,  cutoff,  a,b,c,  x,y,z){

  table[[x]] <- ifelse(table[[a]] - table[[x]] > 0 & table[[a]] - table[[x]] < cutoff,
                       -2,
                       table[[x]])

   table[[y]] <- ifelse(table[[b]] - table[[y]] > 0 & table[[b]] - table[[y]] < cutoff,
                        -2,
                        table[[y]])

  # table[[z]] <- ifelse(table[[x]] == -2 | table[[y]] == -2,
  #                      -2,
  #                      round(as.numeric(table[[z]], digits=1)))

  table[[z]] <- ifelse(table[[y]] == -2 | table[[y]] == -2,
                       -2,
                       round(as.numeric(table[[z]]), digits=1))

  return(table)
}

# for a given table X out identifiable data
RSanonymiseTable<- function(table, what, type = "school", size=30, number=30){

  # anonymisation looks for small numbers and
  # numbers large enough to allow opposite
  # (e.g. male and not female) to be discerned

  #what about the second columns that refer to the first columns? Do they? No.

  cutoff <- 6

  ####test variables####
  # table <- TopComputingSchools
  # table <- TopMixedSchools
  # table <- TopGirlsSchools
  # what <- "ten"
  # table <- temp
  # what <- "six"
  # what <- "five"
  # what <- "seven"
  # what <- "eight"
  # what <- "ten"
  # str(table)
  #ALL schools, 4,5,6%
  #Mixed_school_table, 4,5,6%
  #BAME_LA_Table 2,3,4%, 5,6,7%
  ########

  # filter out NA from column 1 i.e. missing data
  table <- table %>% ungroup() %>% filter(!is.na(.[[1]]))

  if(what=="five"){
    temp <- RSroundtoFive(table, 3,4,5)
    totals <- RSaddTotalsToDF(temp, what)

    table <- RSroundtoFive(table, 3,4,5)
    table <- RSanonymiseColumns(table,cutoff,3,4,5)


    if(type == "school"){

      table <- RSPrepareSchoolsOutput(table, 3,4, size, number)
      table <- table %>% arrange(desc(.[[5]]))
    }

  }

  if(what=="six"){
    #table <- TopComputingSchools, what = "six", type="school", size=20, number=30
    #table <- TopComputingSchools %>% filter(URN == "100094"| URN == "101394")
    temp <- RSroundtoFive(table, 4,5,6)
    totals <- RSaddTotalsToDF(temp, what)

    table <- RSroundtoFive(table, 4,5,6)
    table <- RSanonymiseColumns(table,cutoff,4,5,6)


    if(type == "school"){
      table <- RSPrepareSchoolsOutput(table, 4,5, size, number)
      table <- table %>% arrange(desc(.[[6]]))
    }

  }

  if(what=="seven"){
    # table <-   LAGenderTable
    temp <- RSroundtoFive(table, 2,3,4)
    temp <- RSroundtoFive(temp, 5,6,7)
    totals <- RSaddTotalsToDF(temp, what)

    table <- RSroundtoFive(table, 2,3,4)
    table <- RSroundtoFive(table, 5,6,7)
    table <- RSanonymiseColumns(table,cutoff,2,3,4)
    table <- RSanonymiseColumns(table,cutoff,5,6,7)
    table <- RSanonymiseCrossCheck(table, cutoff,  2,3,4,  5,6,7)

    if(type == "school"){
      table <- RSPrepareSchoolsOutput(table, 2,6, size, number)
      table <- table %>% arrange(desc(.[[7]]))
    }

  }

  if(what=="eight"){

   #get totals
    temp <- RSroundtoFive(table, 3,4,5)
    temp <- RSroundtoFive(temp, 6,7,8)
   totals <- RSaddTotalsToDF(temp, what)

   #randomise the columns
   table <- RSroundtoFive(table, 3,4,5)
   table <- RSroundtoFive(table, 6,7,8)
   table <- RSanonymiseColumns(table,cutoff,3,4,5)
   table <- RSanonymiseColumns(table,cutoff,6,7,8)
   table <- RSanonymiseCrossCheck(table, cutoff,  3,4,5,  6,7,8)

   ## add check between 3 and 6/7/8
   if(type == "school"){
     table <- RSPrepareSchoolsOutput(table, 3,7, size, number)
     table <- table %>% arrange(desc(.[[8]]))
   }


  }

  if(what=="nine"){

    #table <- LAGenderTable
    # table <- temp
    #what<- "nine"
    temp <- RSroundtoFive(table, 4,5,6)
    temp <- RSroundtoFive(temp, 7,8,9)
    #get totals
    totals <- RSaddTotalsToDF(temp, what)

    #randomise the columns
    table <- RSanonymiseColumns(table,cutoff,4,5,6)
    table <- RSanonymiseColumns(table,cutoff,7,8,9)
    table <- RSanonymiseCrossCheck(table, cutoff,  4,5,6,  7,8,9)
    if(type == "localauthority"){
      table <- RSanonymiseColumnsSchools(table, cutoff, 3, 4,5,6)
      table <- RSanonymiseColumnsSchools(table, cutoff, 3, 7,8,9)
    }

    table <- RSroundtoFive(table, 4,5,6)
    table <- RSroundtoFive(table, 7,8,9)

  }

  if(what=="ten"){

    #table <- LAGenderTable
    # table <- left_join(LAPPSchStats, LAPPTable, by=c("Name"="Local Authority"))
    #what<- "ten"

    temp <- RSroundtoFive(table, 5,6,7)
    temp <- RSroundtoFive(temp, 8,9,10)
    #get totals
    totals <- RSaddTotalsToDF(temp, what)

    #randomise the columns
    table <- RSanonymiseColumns(table,cutoff,5,6,7)
    table <- RSanonymiseColumns(table,cutoff,8,9,10)
    table <- RSanonymiseCrossCheck(table, cutoff, 5,6,7, 8,9,10)
    if(type == "localauthority"){
      table <- RSanonymiseColumnsSchools(table, cutoff, 4, 5,6,7)
      table <- RSanonymiseColumnsSchools(table, cutoff, 4, 8,9,10)
    }
    table <- RSroundtoFive(table, 5,6,7)
    table <- RSroundtoFive(table, 8,9,10)

  }

  # replacement values
  if(what=="YoY"){

    #randomise the columns
    table <- RSroundtoFive(table, 3,4,5)
    table <- RSanonymiseColumns(table,cutoff,6,7,8)

    # replace -1 and -2 with X
    table <- table %>%
      mutate_all(funs(ifelse(. == -1,"X",
                              ifelse(. == -2, "Y", as.character(.)))))
  }else{
    # replace -1 and -2 with X
    table <- table %>%
              mutate_all(funs(ifelse(. == -1,"X",
                                    ifelse(. == -2, "Y", as.character(.)))))
    # filter(`Total students` == "X")

    FinalColumn <- names(table)[length(names(table))]
    OtherColumn <- names(table)[length(names(table)) - 2]

    sortCols = c(paste0("desc(as.numeric(`",FinalColumn,"`))"), paste0("desc(as.numeric(`",OtherColumn,"`))"))

    #order on the new value of final column
    table <- table %>% arrange_(.dots=sortCols)
    table <- rbind(table, totals)
    #print(table[,c(1,4:10)])
  }

  return(table)
}

#prepare tables for best school provision
RSPrepareSchoolsOutput <- function(table, pop, focus, size=30, number=30){
  table


  table <- head(table %>% filter(as.numeric(.[[focus]]) >= size | (.[[focus]] == "Y" & .[[pop]] > 35)),number)

  return(table)
}

RSroundtoFive <- function(table, x,y,z){
  # x=8
  # y=9
  # z=10
  #table <- left_join(LAPPSchStats, LAPPTable, by=c("Name"="Local Authority"))

  # if round to five, increase to 6 as this is the smallest identifiable value
  table[[x]] <- ifelse(is.na(as.numeric(table[[x]])), table[[x]],
                       ifelse(table[[x]] %in% c(6,7), 6,
                              ifelse(table[[x]] %in% c(-1,-2,1,2,3,4,5), table[[x]], round_any(table[[x]], 5))))
  table[[y]] <- ifelse(is.na(as.numeric(table[[y]])), table[[y]],
                       ifelse(table[[y]] %in% c(6,7), 6,
                              ifelse(table[[y]] %in% c(-1,-2,1,2,3,4,5), table[[y]], round_any(table[[y]], 5))))
  table[[z]] <- ifelse(table[[x]] == 0, 0,
                       ifelse(table[[y]] %in% c(-1, -2, 0), table[[y]],
                              round(100 * (table[[y]] / table[[x]]), digits = 1)))

  return(table)
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

returnEthnicity <- function(eth){
  eth <- switch(eth,
                "AOEG"   = "Other",
                "ASIA"   = "Asian",
                "CHIN"   = "Chinese",
                "MIXD"   = "Mixed",
                "UNCL"   = "Undeclared",
                "WHIT"   = "White",
                "BLAC"   = "Black",
                "WHIBRI" = "White British",
                "WHIOTH" = "White Other",
                "Missing")
  return(eth)
}

mutateEthnicityMinor <- function(EthTotals){

  EthTotals <- EthTotals %>%
    mutate(EthNameMin = case_when(.$EthMin == ""       ~ "Missing",
                                  .$EthMin == "ABAN"   ~ "Bangladeshi",
                                  .$EthMin == "AIND"   ~ "Indian",
                                  .$EthMin == "APKN"   ~ "Pakistani",
                                  .$EthMin == "AOTH"   ~ "Asian Other",
                                  .$EthMin == "BAFR"   ~ "Black African",
                                  .$EthMin == "BCRB"   ~ "Black Caribbean",
                                  .$EthMin == "BOTH"   ~ "Black Other",
                                  .$EthMin == "CHNE"   ~ "Chinese",
                                  .$EthMin == "MOTH"   ~ "Mixed Other",
                                  .$EthMin == "MWAS"   ~ "Mixed White Asian",
                                  .$EthMin == "MWBA"   ~ "Mixed White Black African",
                                  .$EthMin == "MWBC"   ~ "Mixed White Black Caribbean",
                                  .$EthMin == "WBRI"  ~ "White British",
                                  .$EthMin == "WIRI"  ~ "White Irish",
                                  .$EthMin == "WIRT"  ~ "White Irish Traveller",
                                  .$EthMin == "WOTH"  ~ "White Other",
                                  .$EthMin == "WROM"  ~ "White Romani",
                                  .$EthMin == "REFU"   ~ "Refused",
                                  .$EthMin == "OOTH"   ~ "Other",
                                  .$EthMin == "NOBT"   ~ "Not obtained",
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
#                           "sp","stringi","stringr","svglite","tibble","tidyr","xtable","yaml","readr","ggrepel","hms"))
#
#
# download.packages(packages, destdir="C:/tmp/",
#                   type="win.binary")
# packages <- c("abind","assertthat","bitops","Cairo","caTools","colorspace",
#               "DBI","digest","dplyr","evaluate","formatR","gdtools","ggplot2",
#               "gridExtra","gtable","hms","highr","htmltools","httr","knitr",
#               "labeling","lazyeval","magrittr","maptools","markdown","munsell",
#               "pander","plyr","purrr","R.methodsS3","R.oo","R.utils","R6",
#               "RColorBrewer","Rcpp","rgdal","rgeos","rmarkdown","scales",
#               "sp","stringi","stringr","svglite","tibble","tidyr","xtable","yaml")
#
# install.packages(packages)


#number suppression
#from: page 36: https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/472700/NPD_user_guide.pdf
