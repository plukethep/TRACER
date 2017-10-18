#################
#####COLLATION COMMANDS
#################

#output spreads of students with schools and results for all subjects
outputSpreads <- function(year, keystage, level, n=0){

  # KS4 GCSE $SUBLEVNO = c(310,311,320)
  # KS5 ALevel $SUBLEVNO = c(110, 111, 120, 121)

  #see loadSUBLEVNO() for full list of qualifications

  #111	AA	GCE A level

  #310	G	GCSE Full Course
  #311 iGCSE - 0 results!

  #TODO: WORK ON THE EFFICIENCY OF THIS!
  if(n==0){ #get all students
    
    students <- loadStudents(year, keystage) # <- currently deletes duplicated students (~0.07% of records)
    results_stu <- loadStudentResults(year, keystage, level)
  }else{ #get n students
    students <- loadStudents(year, keystage,n) # <- currently deletes duplicated students (~0.07% of records)
    results_stu <- loadStudentResults(year, keystage, level,n)
  }
  comResults <- bindResultstoStudents(results_stu, students)
  spreadResults <- getSpreadResultsPerStudent(comResults, year, keystage)


  #if iGCSE; currently 0 results
  if(level == 311){
    write.csv(spreadResults,
              file = paste(getwd(), "/data/spreads/iGCSE", keystage, year, "spreadResults.csv",sep=""),
              row.names=FALSE)
  }else{
    write_csv(spreadResults,
                append = FALSE,
                col_names = TRUE,
                path = paste(getwd(), "/data/spreads/", keystage, year, "spreadResults.csv",sep=""))
    
  }

}

#bind results to student details to Mappings of each subject outputs Comresults
bindResultstoStudents <- function(results_stu, students){
  mappings <- loadDiscMappings()

  # this breaks with 2016 data where URN is not complete for student table
  # comResults <- inner_join(students, results_stu, by=c("PupilMatchingRefAnonymous", "URN"))
  # comResults <- left_join(comResults, mappings, by="MAPPING")

  # get rid of the URN attached to a student when joining tables
  comResults <- inner_join(students %>% select(-URN), results_stu, by=c("PupilMatchingRefAnonymous"))

  comResults <- left_join(comResults, mappings, by="MAPPING")

  return(comResults)
}

#get column showing results for each subject for each student (from Results and Students)
getSpreadResultsPerStudent <- function(students, results, schools, postcodes, level){
  # level <- c(310)
  # schools <- loadSchools()
  # postcodes <- loadPostcodes()
  # students <- Students_GCSE_16
  # results <- Results_GCSE_16

  #filter
  mergeResults <- results %>%
                    filter(SUBLEVNO %in% level,
                           PupilMatchingRefAnonymous %in% students$PupilMatchingRefAnonymous) %>%
                    select(PupilMatchingRefAnonymous, URN, MAPPING, SUBLEVNO, POINTS) %>%
                    distinct(PupilMatchingRefAnonymous, URN, MAPPING, SUBLEVNO, POINTS) %>%
                    mutate(MAPPING = paste0("X", MAPPING)) %>%
                    spread(MAPPING, POINTS)

  if(level=="310"){   # replaces 310 which is code for GCSE
    mergeStudents <- students %>%
                      select(PupilMatchingRefAnonymous, GENDER, KS2Eng, KS2Mat, EAL, EthMin,
                             EthMaj, EVERFSM_3, EVERFSM_6, SEN, SENMaj, FSMeligible, IDACIScore)
  }else{ # need a check here for cods for A-level BTEC etc?
    mergeStudents <- students %>%
      select(PupilMatchingRefAnonymous, GENDER, EthMin, EAL, KS2Mat, KS2Eng,
             EthMaj, EVERFSM_3, EVERFSM_6, SEN, SENMaj, FSMeligible, IDACIScore)
  }
  mergeSchools <- schools %>% select(URN, trust, schType, schPhase, schGender, selective, schUrban, Easting, Northing, Postcode)

  # how clean is the data? Look for repeats:
  # df <- mergeResults %>% group_by(PupilMatchingRefAnonymous) %>% summarise(n=n()) %>% filter(n > 1)
  # df <- mergeResults %>% group_by(PupilMatchingRefAnonymous, URN) %>% summarise(n=n()) %>% filter(n > 1)

  temp <- left_join(
            left_join(mergeResults,mergeStudents),
              left_join(mergeSchools,
                        postcodes[, c("Postcode", "area1", "area3", "area5")],
                        by="Postcode"))

  return(temp)
}

#given previous year GCSE data, fill in missing ethnicity data (assuming ethnicity doesn't change)
completeMissingData <- function(Destination, Source, Column, NewName, year){

  print("======================================")
  print(paste0("matching A-level to ", yr," GCSE results for ", NewName))
  print("======================================")

  # Destination <- Students_GCSE_16
  # Source <- Students_GCSE_12
  # #
  # Column <- "EthMaj"
  # NewName <- "EthMaj"

  # to merge datasets Column and NewName must match,
  # else you add Column from Source to Destination and give it name NewName


  #if desired field is not in the Destination dataframe, then add it:
  if(!NewName %in% names(Destination)){
    print(paste("Adding", NewName, "to destination dataframe"))
    Destination[[NewName]] <- NA
    #Destination <- Destination %>% mutate(NewName = NA)
    #Destination <- Destination %>% rename_(.dots=setNames("NewName", list(NewName)))
  }else{
    #give an update on the completeness of the field
    missingStats(Destination, NewName, "A-Level")
  }

  #if desired field is not in the Source dataframe abort function:
  if(Column %in% names(Source)){
    missingStats(Source, Column, "GCSE")
      #note scale of intersection
    matches <- intersect(Destination$PupilMatchingRefAnonymous, Source$PupilMatchingRefAnonymous)
    print(paste("Unique References matched:", length(matches)))
  
    #### Ready GCSE data for merge
    Source <- Source %>%
      select(PupilMatchingRefAnonymous, !!Column) %>%
      filter(PupilMatchingRefAnonymous %in% matches) %>%
      rename(Src1 = !!Column)
  
    #temporarily rename the Destination field so we can perform an easy (non-NSE) merge
    Destination <- Destination %>%
      rename(Dst1 = !!NewName)
  
  
    ####combine GCSE chosen column with A-level
    temp <- left_join(Destination, Source)
  
    #match the levels and work out size of replacement
    if(is.factor(temp$Dst1) & is.factor(temp$Src1)){
      temp$Dst1 <- factor(temp$Dst1, levels=c(union(levels(temp$Dst1), levels(temp$Src1))))
      temp$Src1 <- factor(temp$Src1, levels=c(union(levels(temp$Dst1), levels(temp$Src1))))
    }
  
    #TODO - get this to handle cases of Dst1 == NA and Src1 == 1/0
    replacements <- temp %>% select(Dst1, Src1) %>%
      filter(Dst1 =="" |  is.na(Dst1)) %>%
      filter(Src1 !="" | !is.na(Src1)) %>% summarise(n=n()) %$% n
    print(paste(replacements,"replacements found"))
  
    #merge data
    temp <- temp %>% mutate(Dst1 = ifelse(Dst1 != "" & !is.na(Dst1), as.character(Dst1),
                                          ifelse(Src1 != "" & !is.na(Src1), as.character(Src1), NA)),
                            Dst1 = ifelse(Dst1 == "", NA, Dst1),
                            Dst1 = as.factor(Dst1))
  
    #clear up unneeded columns and set name to that given
    temp <- temp %>% select(-Src1) %>% rename_(.dots=setNames("Dst1", list(NewName)))
  
    #### New missing stats
    missingStats(temp, NewName, "New Data")
    
    
    
  }else{
    temp <- Destination
    warning(paste(Column, "not found in source dataframe, terminating completeMissingData() command"))
  }

  return(temp)
}

# uses the above function to build a more complete dataframe
# used in particular to complete missing data in KS5 return
matchFieldsBetweenYears <- function(destination = "Alevel",
                                    source = "GCSE",
                                    data,
                                    what = c("EthMaj", "EthMin", "EVERFSM_6", "IDACIScore", "EAL", "KS2Mat", "KS2Eng"),
                                    from = 12, to = 16, 
                                    matches=NULL){

  # check that the source data exists, if not try and find it.
  if(is.null(matches)){
    print("no matching data available, attempting to load it")
    for (yr in c(from:to)){
      if(!exists(paste0("Students_", source, "_", yr))){
        print(paste("MISSING", paste0("Students_", source, "_", yr)))
      }else{
        print(paste("FOUND", paste0("Students_", source, "_", yr)))
        matches[[yr]] <- get(paste0("Students_GCSE_",yr))
      }
    }
  }

  # seed the comparison
  temp <- data # get(paste0("Students_", destination, "_", target_year))


  for (focus in what){
    for(yr in c(to:from)){
      temp <- completeMissingData( temp,
                                   matches[[yr]],
                                   focus, focus, yr)
    }
  }
  return(temp)
}
