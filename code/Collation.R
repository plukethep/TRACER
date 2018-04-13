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

  # year <- 16
  # keystage <- "KS4"
  # level <- c(310)
  # keystage <- "KS5"
  # level <- c(111)
  # head(comResults)
  # nrow(spreadResults)
  # length(unique(spreadResults$PupilMatchingRefAnonymous))

  #WORK ON THE EFFICIENCY OF THIS!
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
    # write.csv(spreadResults,
    #           file = paste(getwd(), "/data/spreads/", keystage, year, "spreadResults.csv",sep=""),
    #           row.names=FALSE)

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
  #check that there aren't multiple URNs for the same student:
  # comResults %>% group_by(PupilMatchingRefAnonymous) %>%
  #   mutate(numURN = length(unique(URN))) %>%
  #   ungroup() %>%
  #   filter(numURN > 1)

  comResults <- left_join(comResults, mappings, by="MAPPING")

  return(comResults)
}

#get column showing results for each subject for each student
OLDgetSpreadResultsPerStudent <- function(comResults, year, keystage){

  # TODO: deal with students who sit courses in multiple schools

  # filter to results per student only
  spreadResults <- comResults %>% select(PupilMatchingRefAnonymous, URN, MAPPING, POINTS) %>%
    distinct(PupilMatchingRefAnonymous, URN, MAPPING, POINTS) %>%
    spread(MAPPING, POINTS) %>%
    ungroup()

  # select base data for each student
  StudentBaseData <- comResults %>% select(-MAPPING, -POINTS, -MAPPING_DESCRIPTION) %>%
        distinct()

  #if (keystage == "KS4") {
  #  StudentBaseData <- comResults %>% select(PupilMatchingRefAnonymous, URN, GENDER, KS2Eng, KS2Mat, EVERFSM_6, IDACIScore, EthMaj, EthMin, EAL) %>%
  #    distinct()
  #} else if (keystage == "KS5") {
  #  StudentBaseData <- comResults %>% select(PupilMatchingRefAnonymous, URN, GENDER, KS3Eng, KS3Mat, EVERFSM_6, IDACIScore, EthMaj, EthMin, EAL) %>%
  #    distinct()
  #}

  # check that duplicate data hasn't snuck through
  # repeats <- StudentBaseData[duplicated(StudentBaseData$PupilMatchingRefAnonymous),] %>% arrange(PupilMatchingRefAnonymous)
  # tempStudentBase <- StudentBaseData[StudentBaseData$PupilMatchingRefAnonymous %in% repeats$PupilMatchingRefAnonymous,]

  schools <- loadSchools()

  schools <- schools %>% select(URN, schType, schPhase, schGender, selective, schUrban, Easting, Northing, Postcode)

  #link student to school data
  StudentBaseData <- left_join(StudentBaseData, schools)

  #link to easting, northing and regional mapping info.
  postcodes <- loadPostcodes()
  StudentBaseData <- left_join(StudentBaseData, postcodes[, c("Postcode", "area1", "area3", "area5")], by="Postcode")

  spreadResults <- left_join(StudentBaseData, spreadResults)

  return(spreadResults)
}

#get column showing results for each subject for each student (from Results and Students)
getSpreadResultsPerStudent <- function(students, results, schools, postcodes, level){
  # level <- c(111)
  # schools <- loadSchools()
  # postcodes <- loadPostcodes()
  # students <- temp_students
  # results <-  comResults


  if(level %in% c(310, 391)){   # replaces 310 which is code for GCSE
    # results <- Results_GCSE_Current
    # students <- Students_GCSE_17
    # edit for 2017 data, filter out 310 for MAPPING 2210, 5030 and 5110, these are now under SUBLEVNO 391
    if(391 %in% level){
      new_subs <- results %>% filter(SUBLEVNO == 391) %>% distinct(MAPPING) %$% MAPPING
      message("removing 310 results for ", new_subs)
      # get MAPPING of subjects to only get  9-1 grade system for
      # results %>% group_by(SUBLEVNO, MAPPING) %>% summarise(n=n()) %>% arrange(desc(n))
      results <- results %>% mutate(fil = paste(SUBLEVNO, MAPPING)) %>%
        filter(!fil %in% paste("310", new_subs)) %>% select(-fil) # %>% group_by(SUBLEVNO, MAPPING) %>% summarise(n=n()) %>% arrange(desc(n))
    }

    # results %>% filter(PupilMatchingRefAnonymous == "CCF950CE36D9BCF1C8")
    # temp[c(1831866, 2292535),]

    #filter
    mergeResults <- results %>%
      filter(SUBLEVNO %in% level,
             PupilMatchingRefAnonymous %in% students$PupilMatchingRefAnonymous) %>%
      select(PupilMatchingRefAnonymous, URN, MAPPING, SUBLEVNO, POINTS) %>%
      distinct(PupilMatchingRefAnonymous, URN, MAPPING, POINTS) %>%
      mutate(MAPPING = paste0("X", MAPPING)) %>%
      spread(MAPPING, POINTS)

    # results <- Results_GCSE_17

    # results %>% group_by(SUBLEVNO, MAPPING) %>% summarise(n=n()) %>% arrange(desc(n))

    mergeStudents <- students %>%
                      select(PupilMatchingRefAnonymous, GENDER, KS2Eng, KS2Mat, EAL, EthMin,
                             EthMaj, EVERFSM_3, EVERFSM_6, SEN, SENMaj, FSMeligible, IDACIScore)
  }else if(level=="111"){ # need a check here for codes for A-level BTEC etc?
    #filter
    mergeResults <- results %>%
      filter(SUBLEVNO %in% level,
             PupilMatchingRefAnonymous %in% students$PupilMatchingRefAnonymous) %>%
      select(PupilMatchingRefAnonymous, URN, MAPPING, SUBLEVNO, POINTS) %>%
      distinct(PupilMatchingRefAnonymous, URN, MAPPING, SUBLEVNO, POINTS) %>%
      mutate(MAPPING = paste0("X", MAPPING)) %>%
      spread(MAPPING, POINTS)


    mergeStudents <- students %>%
      select(PupilMatchingRefAnonymous, GENDER, EthMin, contains("EAL"), contains("KS2Mat"), contains("KS2Eng"),
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
completeMissingData <- function(Destination, Source, Column, NewName, year_dest, year_source){

  print("======================================")
  print(paste0("matching ", year_dest, " A-level to ", year_source," GCSE results for ", NewName))
  print("======================================")

  # Destination = temp
  # Source = matches[[14]]
  # Column = names(what)
  # NewName = what[[Column]]
  # year = 14

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

    #temporarily rename the Destination field so we can perform an easy (non-NSE) merge
    Destination <- Destination %>%
      rename(Dst1 = !!NewName)
    print("Destination dataframe readied")

    #### Ready GCSE data for merge
    Source <- Source %>%
      select(PupilMatchingRefAnonymous, !!Column) %>%
      # filter(PupilMatchingRefAnonymous %in% matches) %>%
      rename(Src1 = !!Column)
    print("Source columns renamed")


    ####combine GCSE chosen column with A-level
    temp <- left_join(Destination, Source)
    print("Datasets joined")

    #match the levels and work out size of replacement
    if(is.factor(temp$Dst1) & is.factor(temp$Src1)){
      temp$Dst1 <- factor(temp$Dst1, levels=c(union(levels(temp$Dst1), levels(temp$Src1))))
      temp$Src1 <- factor(temp$Src1, levels=c(union(levels(temp$Dst1), levels(temp$Src1))))
    }

    # Make sure that the below is ungrouped to speed up the matching process.
    # temp %>% ungroup() %>% filter(!is.na(Src1))

    #TODO - get this to handle cases of Dst1 == NA and Src1 == 1/0
    replacements <- temp %>% ungroup() %>%
      select(Dst1, Src1) %>%
      filter(Dst1 =="" |  is.na(Dst1)) %>%
      filter(Src1 !="" | !is.na(Src1)) %>% summarise(n=n()) %$% n
    print(paste(replacements ,"replacements found"))


    #merge data
    temp <- temp %>% mutate(Dst1 = ifelse(Dst1 != "" & !is.na(Dst1), as.character(Dst1),
                                          ifelse(Src1 != "" & !is.na(Src1), as.character(Src1), NA)),
                            Dst1 = ifelse(Dst1 == "", NA, Dst1),
                            Dst1 = as.factor(Dst1))

    print("Merging data and replacing missing fields")

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

# matches fields to a KS5 table from GCSE results, returning the highest grade for each student it finds
matchFieldsBetweenYears_RAM <- function(data_to, field="X2210", new_name="GCSE_maths", year_range=c(14:17)){
  # data_to <- Spread_Alevel_17
  
  # get URN from 2017
  IDs <- data_to$PupilMatchingRefAnonymous
  
  # cycle through each year
  missing_data <- data.frame()
  for (yr in year_range){
    # yr <- 14
    df <- get(paste0("Spread_GCSE_",yr))

    # get all GSCE_Maths data for a given student
    match_results <- df %>% select(PupilMatchingRefAnonymous,!!field) 
    names(match_results) <- c("PupilMatchingRefAnonymous", "grade")
    
    match_results <- match_results %>%
      filter(PupilMatchingRefAnonymous %in% IDs,
             !is.na(grade)) %>%
      mutate(Year = yr)
    message("=== 20", yr, " - ", field, " matches: ", nrow(match_results))
    
    # print(unique(match_results$grade))
    # match_results %>% group_by(grade) %>% summarise(n = n())
    
    # convert grades into 1-8 format
    match_results <- convertGrades(match_results, "GCSE")
    match_results <- convertGrades_letter_to_number(match_results, "GCSE")
    # print(unique(match_results$grade))
    
    missing_data <- rbind(missing_data, match_results)
  }
  
  missing_data <- missing_data %>% group_by(PupilMatchingRefAnonymous) %>% summarise(GCSE_maths = max(grade))
  
  names(missing_data) <- c("PupilMatchingRefAnonymous", new_name)
  
  # data_to <- left_join(data_to, missing_data)
  return(missing_data)
}

# uses the above function to build a more complete dataframe
# used in particular to complete missing data in KS5 return
matchFieldsBetweenYears <- function(destination = "Alevel",
                                    source = "GCSE",
                                    data,
                                    what_to_match_person = NULL,
                                    what_to_match_results = NULL,
                                    from = 12, to = 17,
                                    year_dest,
                                    matches=NULL){
  # if matching to GCSE Maths, make sure that matches contains Spread_GCSE_YEAR rather than Student_GSCE_YEAR
  # Make sure that what lists different names for the new fields from subject codes, e.g.
  # "X2210" = "GCSEMaths"


  # check that comparison and matching dataframes exist, if not load them
  # if(!exists(paste0("Students_", destination, "_", target_year))){
  #   assign(paste0("Students_", destination, "_", target_year), data, envir = .GlobalEnv)
  #   print("creating A-level data")
  # }


  # check that the source data exists, if not try and find it.
  if(is.null(matches)){
    print("no matching data available, aborting match")
    return(data)

    # for (yr in c(from:to)){
    #   if(!exists(paste0("Students_", source, "_", yr))){
    #     print(paste("MISSING", paste0("Students_", source, "_", yr)))
    #   }else{
    #     print(paste("FOUND", paste0("Students_", source, "_", yr)))
    #     matches[[yr]] <- get(paste0("Students_GCSE_",yr))
    #   }
    # }
  }

  # seed the comparison
  temp <- data # get(paste0("Students_", destination, "_", target_year))
  message("Matching on:")
  # message(what_to_match_person)

  for (focus in names(what_to_match_person)){
    message("matching personal details")
    message(focus)
    for(yr in c(to:from)){
      # yr <- 12
      # matching on student demographic data
      if(!is.null(what_to_match_person)){
        source_data <- matches[[yr]]
      }

      temp <- completeMissingData( Destination = temp,
                                   Source = source_data,
                                   Column = focus,
                                   NewName = what_to_match_person[[focus]],
                                   year_source = yr,
                                   year_dest = year_dest)
    }
  }

  for (focus in names(what_to_match_results)){
    message("matching results")
    message(what_to_match_results)
    for(yr in c(to:from)){

      # yr <- 15
      # focus <- "X2210"

      # if matching on results data = ONLY MATCHES TO GCSEs
      if(!is.null(what_to_match_results)){
        names <- substring(names(what_to_match_results),2)
        source_data <- matches[[yr]] %>%
          filter(MAPPING %in% names, SUBLEVNO == 310) %>%
          group_by(PupilMatchingRefAnonymous, MAPPING) %>%
          filter(POINTS == max(POINTS)) %>%
          distinct(PupilMatchingRefAnonymous, MAPPING, POINTS) %>%
          spread(MAPPING, POINTS)

        names(source_data) <- c(names(source_data)[1], names(what_to_match_results))

        message("finished spreading the results table...")
      }

      temp <- completeMissingData( Destination = temp,
                                   Source = source_data,
                                   Column = focus,
                                   NewName = what_to_match_results[[focus]],
                                   year_source = yr,
                                   year_dest = year_dest)
    }
  }
  # temp <- completeMissingData(temp, Gresults16, "X1210", "GCSEPhys")
  #
  # temp <- completeMissingData(temp, Gresults16, "X1110", "GCSEChem")
  #
  # temp <- completeMissingData(temp, Gresults16, "X1010", "GCSEBio")
  #
  # temp <- completeMissingData(temp, Gresults16, "X2210", "GCSEMaths")


  return(temp)
}

##WIP##
#match Local Governmental Authorities to Local Educational Authorities to mapping purposes
matchLGAtoLEA <- function(spreadResults){

  #get postcodes and LGA mapping
  LGA <- spreadResults %>% distinct(URN, Postcode, area5)
  #load local governmental authority data
  localauthorities <- loadLocalAuthorities()
  LGA <- left_join(LGA, localauthorities, by=c("area5" = "Code"))

  # get URNs and their LEAs
  LEAs <- loadLocalEducationAuthorities()

  #merge data to map LEA to LGA
  LEAs <- left_join(LGA, LEAs, by="URN")


  LEAs %>% distinct(LA..code., LA..name.)

  #check that LEA list doesn't have repreated LGA. If so, we are good to map using LEA LGA mapping
  nrow(LEAs %>% group_by(LA..code.) %>% distinct(area5) %>% ungroup())
  nrow(LEAs %>% distinct(area5))

  #where are the LGAs split across two LEAs?
  LEA_duplicate_IDs <- LEAs %>% group_by(LA..code., LA..name.) %>%
    distinct(area5) %>% ungroup() %>%
    group_by(area5) %>% filter(n()>1) %>% arrange(area5) %>%
    select(area5) %>% distinct() #ignore this line to see where conflicts are

  #find out schools that are in 'cross areas'
  t <- left_join(LEA_duplicate_IDs, LGA[,c(1:3)], by="area5") %>% arrange(area5)

  #save this to file
  write.csv(t, file = paste(folder, "junk/conflictingLGAs.csv",sep=""))
  write.csv(LEAs, file = paste(folder, "junk/AllSchoolMappingData.csv",sep=""))

  temp <- left_join(temp, distinct(LGA[c(3,4)]))

  names(temp) <- c("LGA_Code", "LEA_Code", "LEA_Name", "LGA_Name")
  temp <- temp[,c("LGA_Code", "LGA_Name", "LEA_Code", "LEA_Name")]

  names(LEAs)

}
