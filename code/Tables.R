#################
## Output Tables
#################

#return a dataframe with numbers of exam students for each school URN and GENDER
getSchoolSize <- function(spreadResults, type="combine"){
  #get School size with gender for each school
  output <- spreadResults %>% select(URN, GENDER, PupilMatchingRefAnonymous) %>%
    distinct(URN, GENDER, PupilMatchingRefAnonymous) %>%
    group_by(URN, GENDER) %>%
    summarise(ExamStu = n()) %>%
    ungroup()

  #if they want to combine male and female results
  if(type =="combine"){
    output <- output %>% group_by(URN) %>% summarise(ExamStu = sum(ExamStu))
  }

  return(output)
}

#return a dataframe with numbers of qualifications for each school URN for a given sub ID
getSchoolQualNumber <- function(spreadResults, sub = "X2610", level = "GCSE"){

  #get all subs and the refno & URN
  subnames <- c("URN", names(spreadResults[,grep("X[0-9]", names(spreadResults))]))
  output <- spreadResults %>% select_(.dots=subnames)

  #get School size with gender for each school
  output <- output %>%
    gather(subject, grade, -URN, na.rm = TRUE) %>%
    select(URN, subject) %>%
    distinct(URN, subject) %>%
    group_by(URN) %>%
    summarise(totalQuals = n(),
              computing = ifelse(sub %in% subject, TRUE, FALSE))

  return(output)
}


#Get participation by schooltype for a given subject
OutputSubjectStudentSchoolsAll <- function(df, subject = quo(X2610)){

  # df <- Spread_GCSE_Current

  ###############
  #Output total number of schools and total students per school type
  ###############

  tempOverall <- df %>% mutate(StuAvgGrade = rowMeans(.[,grep("X", names(df))], na.rm = TRUE)) %>%
    select(URN, trust, schType, schGender, selective, schUrban, PupilMatchingRefAnonymous, StuAvgGrade) %>%
    group_by(URN,trust, schType, schGender, selective, schUrban) %>%
    mutate(SchTotalStudents = n(), SchTotalGrade = sum(StuAvgGrade)) %>%
    ungroup() %>%
    distinct(trust, schType, schGender, selective, schUrban, URN, SchTotalStudents, SchTotalGrade) %>%
    group_by(trust, schType, schGender, selective, schUrban, URN) %>%
    mutate(SchTypeTotalSchools = n(), SchAvgGrade = SchTotalGrade/SchTotalStudents) %>%
    ungroup() %>%
    distinct(URN, trust, schType, schGender, selective, schUrban, SchTypeTotalSchools, SchTotalStudents, SchTotalGrade, SchAvgGrade)

  sum(tempOverall$SchTypeTotalSchools)
  sum(tempOverall$SchTotalStudents)

  #get the stats for a subject
  #get(subject) returns the column that is passed by the variable subject
  tempSubject <- df %>% mutate(StuAvgGrade = rowMeans(.[,grep("X", names(df))], na.rm = TRUE)) %>%
    select(URN, trust, schType, schGender, selective, schUrban, PupilMatchingRefAnonymous, StuAvgGrade, !!subject) %>%
    filter(!is.na(!!subject)) %>%
    group_by(URN, trust, schType, schGender, selective, schUrban) %>%
    mutate(SubTotalStudents = n(), SubTotalGrade = sum(!!subject), SubStuAllGrades = sum(StuAvgGrade)) %>%
    ungroup() %>%
    distinct(URN, trust, schType, schGender, selective, schUrban, SubTotalStudents, SubTotalGrade, SubStuAllGrades) %>%
    group_by(URN, trust, schType, schGender, selective, schUrban) %>%
    mutate(SubTotalSchools = n(),  SubAvgGrade = SubTotalGrade/SubTotalStudents, SubStuAllAvgGrade = SubStuAllGrades/SubTotalStudents) %>%
    ungroup() %>%
    distinct(URN, trust, schType, schGender, selective, schUrban, SubTotalSchools, SubTotalStudents, SubTotalGrade, SubAvgGrade, SubStuAllGrades, SubStuAllAvgGrade)

  sum(tempSubject$SubTotalSchools)
  sum(tempSubject$SubTotalStudents)

  #join results together and add summary stats
  output <- tempOverall %>%
    left_join(tempSubject, by = c("URN" = "URN", "trust" = "trust", "schType" = "schType", "schGender" = "schGender",
                                  "selective" = "selective", "schUrban" = "schUrban")) %>%
    arrange(trust, schType, schGender, selective, schUrban)

  return(output)
}

#Get grade distributions per school for all schools
getSchoolGradeDistribution <- function(spreadResults, subject= "X2610"){

  #get results in column format
  output <- spreadResults %>% select(PupilMatchingRefAnonymous, URN, schType,
                                     schGender, schUrban, selective, GENDER, get(subject)) %>%
    gather(subject, grade, -PupilMatchingRefAnonymous, -URN, -schType, -schGender,
           -schUrban, -selective, -GENDER, na.rm = TRUE)

  #spread results for each subject
  output %>% select(URN, GENDER, schType, schGender, schUrban, subject, grade) %>%
    group_by(schUrban, schType, schGender, URN, GENDER, subject, grade) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    unite(identifier, subject, grade) %>%
    arrange(URN, identifier) %>%
    spread(identifier, n)

  #NOTE: column headers are X2610_0 X2610_16 etc

  return(output)
}

# return a table of grouping thing, with % compared to general population
get_thing_comp_to_pop <- function(thing, sub, yr, data){

  graph_data <- data %>%
    group_by(!!thing) %>% mutate(total = n()) %>%
    ungroup() %>%
    filter(!is.na(!!sub)) %>%
    select(!!thing, total) %>%
    group_by(!!thing) %>%
    mutate(sub_total = n(), percentage = 100 * (sub_total / total)) %>%
    distinct(!!thing, total, sub_total, percentage) %>%
    ungroup() %>%
    filter(!is.na(!!thing)) %>%
    arrange(desc(percentage))

  graph_data[[quo_name(thing)]] <- as.vector(graph_data[[quo_name(thing)]])
  graph_data[[quo_name(thing)]] <- factor(graph_data[[quo_name(thing)]],graph_data[[quo_name(thing)]])
  graph_data$year <- yr

  return(graph_data)
}


#UNFINISHED
getGrammarByRegion <- function(spreadResults, subject= "X2610"){
  #get summary of number of schools in each category
  # temp <- spreadResults %>% select(selective, schType) %>%
  #   group_by(selective, schType) %>% summarise(n = n())

  postcodes <- loadPostcodes()

  #combine Region to spreadResults
  #add the Region data to each student record
  temp <- left_join(spreadResults %>% select(URN, selective, schType, get(subject), Postcode),
                    postcodes[, c("Postcode", "Region")])

  temp <- temp %>% select(Region, selective, schType, URN) %>%
     distinct(Region, selective, schType, URN) %>%
     group_by(Region, selective, schType) %>% summarise(n = n())


  temp %>% ungroup() %>%
    filter(selective == "Selective",
           grep("independ", schType)) %>%
    summarise(Grammar = ifelse())

  temp$n[grep("Indepen",temp$schType) & temp$selective == "Selective"]
}

###  OLD  ####
#for each subject get grades by overall, gender, FSM, non FSM
OutputGradeDistributions <- function(spreadResults, discID, level, year, scope){
  #spreadResults <- results12
  #discID <- "X2610"
  #level <- "A2"
  #scope <- "gender"

  #names(results14)
  #results14 %>% filter(!is.na(X2610)) %>% select(EVERFSM_6) %>% distinct(EVERFSM_6)
  #scope <- "gender" / "all" / "fsm" / "overall"

  #make sure that the name doesn't include any dodgy characters
  discMappings <- loadDiscMappings()
  DiscName <- discMappings %>% filter(MAPPING == if(substr(discID, 1, 1) == "X") substr(discID, 2, 5) else discID ) %>%
    select(MAPPING_DESCRIPTION) %>%
    mutate(MAPPING_DESCRIPTION = gsub("[^A-Za-z0-9]", "", MAPPING_DESCRIPTION))

  #filter to load just he requested discID
  coln <- match(discID,names(spreadResults))
  colID <- as.name(discID)
  cols <- c("GENDER", "EVERFSM_6", colID)

  #dataframe of Gender, FSM, result and n
  temp <- spreadResults %>% select_(.dots = cols) %>%
    filter_(paste0('!is.na(`', discID, '`)')) %>%
    group_by_(.dots = cols) %>%
    summarise(n = n()) %>%
    ungroup()

  #daraframe of results and gender only
  cols <- c("GENDER", colID, "n")
  gender_results <- temp %>%
    select_(.dots=cols) %>%
    group_by_(.dots=cols[1:2]) %>%
    summarise(gender_n = sum(n)) %>%
    spread_(discID, "gender_n") %>%
    rename(type = GENDER)

  #get totals for each grade
  overall_results <- gender_results %>% ungroup() %>%
    select(-type) %>%
    summarise_each(funs(sum))

  overall_results$type <- "overall"

  #daraframe of results and FSM only
  cols <- c("EVERFSM_6", colID, "n")
  FSM_results <- temp %>%
    select_(.dots=cols) %>%
    group_by_(.dots=cols[1:2]) %>%
    summarise(fsm_n = sum(n)) %>%
    spread_(discID, "fsm_n") %>%
    rename(type = EVERFSM_6)

  #combine the results together into a single dataframe
  overall_results <- rbind(overall_results, gender_results)
  overall_results <- rbind(overall_results, FSM_results)

  #convert A-Level/GCSE scores into letters
  if(level == "GCSE"){
    #convert GCSE scores into letter
    overall_results <- overall_results %>%
      rename(D = `34`) %>%
      rename(C = `40`) %>%
      rename(B = `46`) %>%
      rename(s = `58`) %>%
      rename(F = `22`) %>%
      rename(A = `52`) %>%
      rename(E = `28`) %>%
      rename(U = `0`) %>%
      rename(G = `16`)

    #re-order columns
    positions <- c("type", "s", "A", "B", "C", "D", "E", "F", "G", "U")
    overall_results <- overall_results[positions]

  }else if(level == "A2"){
    overall_results <- overall_results %>%
      rename(s = `300`) %>%
      rename(A = `270`) %>%
      rename(B = `240`) %>%
      rename(C = `210`) %>%
      rename(D = `180`) %>%
      rename(E = `150`) %>%
      rename(U = `0`)
    ### points are different in NPD data than in UCAS references
    #rename(S = `140`) %>%
    #rename(A = `120`) %>%
    #rename(B = `100`) %>%
    #rename(C = `80`) %>%
    #rename(D = `60`) %>%
    #rename(E = `40`) %>%
    #rename(U = `0`)

    #re-order columns
    positions <- c("type", "s", "A", "B", "C", "D", "E", "U")
    overall_results <- overall_results[positions]

  }else if(level == "AS"){
    overall_results <- overall_results %>%
      rename(A = `60`) %>%
      rename(B = `50`) %>%
      rename(C = `40`) %>%
      rename(D = `30`) %>%
      rename(E = `20`) %>%
      rename(U = `0`)

    #re-order columns
    positions <- c("type", "A", "B", "C", "D", "E", "U")
    overall_results <- overall_results[positions]
  }
  #get into format suitable for graphing
  overall_results <- gather(overall_results, grade, count, -type) %>%
    mutate(count = ifelse(is.na(count), 0, count))

  #add percentage to each result
  overall_results$percentage <- 0
  overall_results <- overall_results %>% group_by(type) %>%
    mutate_each(funs((count / sum(count))* 100), percentage)

  #remove the type from positions
  positions <- positions[c(-1)]


  #TODO: anonymise results with X


  #decide which graph to print out (note this also saves them, see printResults)
  if(scope=="gender"){
    graph <- printResults(overall_results %>% filter(type == "M" | type == "F"), DiscName, discID, year, level, "Gender", positions, 10)
  } else if(scope=="fsm"){
    graph <- printResults(overall_results %>% filter(type == 0 | type == 1), DiscName, discID, year, level, "FSM", positions, 10)
  } else if(scope=="overall"){
    graph <- printResults(overall_results %>% filter(type == "overall"), DiscName, discID, year, level, "Overall", positions, 5)
  } else if(scope=="all"){
    graph <- printResults(overall_results %>% filter(type == "overall"), DiscName, discID, year, level, "Overall", positions, 5)
    graph <- printResults(overall_results %>% filter(type == "M" | type == "F"), DiscName, discID, year, level, "Gender", positions, 10)
    graph <- printResults(overall_results %>% filter(type == 0 | type == 1), DiscName, discID, year, level, "FSM", positions, 10)
  }
  return(graph)
}

#output each banded school size with number taking subject and total school cohort
OutputSubjectBySchoolSizeClusters <- function(spreadResults, subject, type){

  #spreadResults <- Spread_GCSE_Current
  #subject <- quo(X2610)
  #type <- "combined"

  # get size of each school
  schoolSize <- getSchoolSize(spreadResults, "gender") %>%
    group_by(URN) %>%
    summarise(n = sum(ExamStu)) %>%
    arrange(desc(n))

  #split into 5 equal sized groups
  bar1 <- schoolSize[c(ceiling(0.2 * nrow(schoolSize))),]$n
  bar2 <- schoolSize[c(ceiling(0.4 * nrow(schoolSize))),]$n
  bar3 <- schoolSize[c(ceiling(0.6 * nrow(schoolSize))),]$n
  bar4 <- schoolSize[c(ceiling(0.8 * nrow(schoolSize))),]$n
  bar5 <- schoolSize[c(ceiling(1.0 * nrow(schoolSize))),]$n

  #if we want to get results by gender and grade, use this
  #cols <- c(as.name("URN"), as.name("GENDER"), as.name(subject))
  #group_by_(.dots = cols) %>%

  #get subject cohort size
  output <- spreadResults %>% select(URN, GENDER, !!subject) %>%
    filter(!is.na(!!subject)) %>%
    group_by(URN) %>%
    summarise(SubSch = n())

  #match subject cohort to school cohort
  output <- left_join(schoolSize, output, by=c("URN"))

  #combine male and female results if desired
  # #if(type == "combine"){
  # output <- output %>% ungroup() %>%
  #   group_by(URN) %>%
  #   summarise(ExamStu = sum(ExamStu), SubStu = sum(SubStu))


  output <- output %>%
    mutate(Size = ifelse(n <= bar4, 1,                 # paste0(bar5,":",bar4),
                      ifelse(n <= bar3, 2,                # paste0(bar4,":",bar3),
                       ifelse(n <= bar2, 3,               # paste0(bar3,":",bar2),
                        ifelse(n <= bar1, 4, 5))))) %>%   # paste0(bar2,":",bar1), paste0(bar1,":",0)))))) %>%
    group_by(Size) %>%
    summarise(entries = sum(!is.na(SubSch)),
              noentries = sum(is.na(SubSch)),
              Entries = 100 * entries/(noentries + entries),
              No_Entries = 100 * (1 - (entries/(noentries + entries)))) %>%
    mutate(label = ifelse(Size == 1, paste0(bar5,"-",bar4-1),
                     ifelse(Size == 2, paste0(bar4,"-",bar3-1),
                      ifelse(Size == 3, paste0(bar3,"-",bar2-1),
                       ifelse(Size == 4, paste0(bar2,"-",bar1-1), paste0(bar1,"-",max(entries)))))))


  #Get in format for stacked bar chart and PLOT
  #output <- output %>%
  #          gather(type, percentage, perentries:pernons)

  #ggplot(output, aes(x = ExamStu, y = percentage, fill=type)) +
  #  geom_bar(
  #    stat = "identity"
  #  )
  #}

  return(output)
}

#output each banded school size with number taking subject and total school cohort
OutputSubjectBySchoolSize <- function(spreadResults, subject, name){

  #spreadResults <- Spread_GCSE_Current
  #subject <- quo(X2610)

  output <- spreadResults %>% select(!!subject, URN) %>%
    filter(!is.na(!!subject)) %>%
    group_by(URN) %>%
    summarise(size = n()) %>%
    ungroup() %>%
    group_by(size) %>%
    summarise(n = n()) %>%
    ungroup()

  output$subject <- quo_name(subject)
  output$name <- name

  #mutate data into plottable format
  output <- output %>% group_by(subject) %>% mutate(cumulative = cumsum(n),
                                                total = sum(n),
                                                per = round(100*(cumulative/total),1))

  return(output)
}

OutputSubjectsBySchoolSize <- function(spreadResults, focus=quo(X2610), subsize=30){

  # spreadResults <- Spread_GCSE_Current
  #subject <- "X2610"
  #head(output)

  subjects <- buildSubjectList(spreadResults, quo_name(focus), subsize)

  dots <- append("URN", as.vector(subjects$ID))

  output <- spreadResults %>% select_(.dots = dots) %>%
    gather("subject", "grade", -URN) %>%
    filter(!is.na(grade)) %>%
    filter(subject %in% subjects$ID) %>%
    group_by(subject, URN) %>%
    summarise(size = n()) %>%
    left_join(subjects, by=c("subject"="ID")) %>%
    mutate(SubjectName = as.character(SubjectName))

  #get ALL quals
  dots <- append("URN",as.vector(names(spreadResults[,grep("X[0-9]", names(spreadResults))])))

  ALL <- spreadResults %>% select_(.dots = dots) %>%
    gather("subject", "grade", -URN) %>%
    filter(!is.na(grade)) %>%
    group_by(subject,URN) %>%
    summarise(size = n()) %>%
    ungroup() %>%
    mutate(subject = "ALL", n = sum(size), SubjectName="ALL")

  output <- bind_rows(output, ALL)

  #head(ALL)
  #head(output)
  #head(temp)

  output <- output %>% ungroup() %>% group_by(subject) %>%
    mutate(upper = quantile(size, c(0.1, 0.9))[[2]])

  return(output)
}



#output schools by number of quals that they offer and whether they offer the given subject
OutputSubjectBySchoolQuals <- function(spreadResults, subject, level){

   # spreadResults <- Spread_GCSE_Current
   # subject <- comparison_subjects[[1]]
  #level <- "GCSE"

  schoolQuals <- getSchoolQualNumber(spreadResults, quo_name(subject), level) %>% arrange(totalQuals)

  bar1 <- schoolQuals[c(ceiling(0.2 * nrow(schoolQuals))),]$totalQuals
  bar2 <- schoolQuals[c(ceiling(0.4 * nrow(schoolQuals))),]$totalQuals
  bar3 <- schoolQuals[c(ceiling(0.6 * nrow(schoolQuals))),]$totalQuals
  bar4 <- schoolQuals[c(ceiling(0.8 * nrow(schoolQuals))),]$totalQuals
  bar5 <- schoolQuals[c(ceiling(1.0 * nrow(schoolQuals))),]$totalQuals


  output <- schoolQuals %>%
    mutate(Size = ifelse(totalQuals >= bar4, 5,                 # paste0(bar5,":",bar4),
                    ifelse(totalQuals >= bar3, 4,                # paste0(bar4,":",bar3),
                       ifelse(totalQuals >= bar2, 3,               # paste0(bar3,":",bar2),
                         ifelse(totalQuals >= bar1, 2, 1))))) %>%   # paste0(bar2,":",bar1), paste0(bar1,":",0)))))) %>%
    group_by(Size) %>%
    summarise(entries = sum(computing),
              noentries = sum(!computing),
              Entries = 100 * entries/(noentries + entries),
              No_Entries = 100 * (1 - (entries/(noentries + entries)))) %>%
    mutate(label = ifelse(Size == 5, paste0(bar4+1,"-",bar5),
                    ifelse(Size == 4, paste0(bar3+1,"-",bar4),
                      ifelse(Size == 3, paste0(bar2+1,"-",bar3),
                        ifelse(Size == 2, paste0(bar1+1,"-",bar2), paste0(1,"-",bar1))))))

  return(output)
}

#output by ethnicity uptake for a given subject by Ethnicity Major
OutputEthnicityBreakdownEthMaj <- function(spreadResults, subject=quo(X2610)){

  #####THIS NOW FILTERS OUT MISSING DATA

  #spreadResults <- Gresults15

  #ethMaj  EthMin  GENDER  FSM  Grade
  #spreadResults %>% select(EthMaj, EthMin) %>% distinct(EthMaj, EthMin)

  #spreadResults %>% select(EthMaj) %>% distinct()

  EthTotals <- spreadResults %>% select(EthMaj) %>%
    filter(EthMaj != "") %>%
    group_by(EthMaj) %>% summarise(PopulationEthTotal = n()) %>%
    ungroup() %>%
    mutate(PopulationEthPercentage = round((PopulationEthTotal/sum(PopulationEthTotal)) * 100,2))

  EthSubject <- spreadResults %>% select(EthMaj, !!subject) %>% filter(!is.na(!!subject)) %>%
    filter(EthMaj != "") %>%
    group_by(EthMaj) %>% summarise(SubEthTotal = n()) %>%
    ungroup() %>%
    mutate(SubEthPercentage = round((SubEthTotal/sum(SubEthTotal)) * 100,2))

  EthTotals <- left_join(EthTotals, EthSubject[, c("EthMaj", "SubEthTotal", "SubEthPercentage")], by="EthMaj")

  EthTotals <- mutateEthnicity(EthTotals)

  #percentage of global population sitting computing
  Globalper <- sum(EthTotals$SubEthTotal) / sum(EthTotals$PopulationEthTotal)
  #how different are the ethnicity figures from the predicted figures using the overall population
  EthTotals$comparison <- round(100 * ((EthTotals$SubEthTotal / EthTotals$PopulationEthTotal) - Globalper) / Globalper,2)

  names(EthTotals) <- c("EthCode", "Total", "Pop %", "Sub Total", "Sub %", "Ethnicity", "difference from expected (%)")

  EthTotals <- EthTotals[c( "Ethnicity", "Total", "Pop %", "Sub Total", "Sub %","difference from expected (%)")]

  return(EthTotals)

  # Anonymise results

  #EthTotals <- EthTotals %>% mutate(EthPercentage =
  #                                    ifelse(Ethnicity < 6, "X", as.character((Ethnicity/sum(Ethnicity)) * 100)),
  #                                  Ethnicity = ifelse(Ethnicity < 6, "X", as.character(Ethnicity)))


  ### triple science ###
  #TripleEth <- results %>% select(X1010, X1110, X1210, EthMaj, EthMin) %>%
  #  filter(is.na(X1010) & is.na(X1010) & is.na(X1010)) %>%
  #  select(EthMaj, EthMin) %>%
  #  group_by(EthMaj, EthMin) %>%
  #  summarise(Ethnicity = n()) %>%
  #  ungroup() %>%
  #  mutate(EthPercentage = Ethnicity/sum(Ethnicity), name = "triple")

  #write.csv(results, paste0(folder, keystage, year, "Ethbreakdown.csv"))
}

#output by ethnicity uptake for a given subject by Ethnicity Minor
OutputEthnicityBreakdownEthMin <- function(spreadResults, subject="X2610"){

  EthTotals <- spreadResults %>% select(EthMaj, EthMin) %>%
    group_by(EthMaj, EthMin) %>% summarise(PopulationEthTotal = n()) %>%
    ungroup() %>%
    mutate(PopulationEthPercentage = (PopulationEthTotal/sum(PopulationEthTotal)) * 100)

  EthSubject <- spreadResults %>% select(EthMaj, EthMin, get(subject)) %>% filter_(paste0("!is.na(",subject,")")) %>%
    group_by(EthMaj, EthMin) %>% summarise(SubEthTotal = n()) %>%
    ungroup() %>%
    mutate(SubEthPercentage = (SubEthTotal/sum(SubEthTotal)) * 100)

  EthTotals <- left_join(EthTotals, EthSubject[, c("EthMin", "SubEthTotal", "SubEthPercentage")], by="EthMin")

  # Anonymise results

  #EthTotals <- EthTotals %>% mutate(EthPercentage =
  #                                    ifelse(Ethnicity < 6, "X", as.character((Ethnicity/sum(Ethnicity)) * 100)),
  #                                  Ethnicity = ifelse(Ethnicity < 6, "X", as.character(Ethnicity)))

  return(EthTotals)
}

#gets the A-Level or GCSE cohort size of each school along with other descriptors for mapping
OuputExamSchoolCohortSize <- function(spreadResults){

  schools <- loadSchools()
  school_size <- getSchoolSize(spreadResults, "all")

  names(schools)
  head(school_size)

  #combine NPD data and schools data to get GCSE cohort size per school for mapping
  school_size <- left_join(school_size, select(schools, URN, NumberOfPupils, Easting, Northing, TypeOfEstablishment..name.),
                           by = c("URN" = "URN"))

  ggplot(school_size, aes(Easting, Northing)) +
    geom_point(aes(size = ExamStu), alpha = 2/3) +
    scale_size_area()

  return(school_size)
}

# summarises the tables output by OutputSubectStudentSchoolsByCriteria(...)
OutputSubectStudentSchoolsByCriteriaTail <- function(df){
  df <- df %>% summarise(SchTypeTotalSchools = sum(SchTypeTotalSchools, na.rm = TRUE),
                       SchTotalStudents = sum(SchTotalStudents, na.rm = TRUE),
                       SubTotalSchools = sum(SubTotalSchools, na.rm = TRUE),
                       SubTotalStudents = sum(SubTotalStudents, na.rm = TRUE),
                       SchTotalGrade = sum(SchTotalGrade, na.rm = TRUE),           #/ SchTotalStudents,
                       SubStuAllGrades = sum(SubStuAllGrades, na.rm = TRUE),       #/ SubTotalStudents,
                       SubTotalGrade = sum(SubTotalGrade, na.rm = TRUE)) %>%       #/ SubTotalStudents) %>%
      ungroup()#
  return(df)
}

#outputs CSV file of which school types are offering a particular subject
OutputSubectStudentSchoolsByCriteria <- function(data, subject, type, grades = TRUE){

  #spreadResults <- Gresults15
  # data <- Spread_Alevel_Current
  # data <- Spread_GCSE_Current
  # subject <- subject_code
  # type <- "type"
  # grades <- TRUE

  # data = Spread_GCSE_16
  # subject = subject_code
  # type = "region"
  # grades = TRUE



  ###### get details on ALL students, schools and grades for all school types
  df <- OutputSubjectStudentSchoolsAll(data, subject)

  ######
  # depending on type option, collate results
  ######

  ##### get provision for each school
  if(type == "all"){
    df <- df %>% select(URN, SchTypeTotalSchools, SchTotalStudents, SubTotalSchools, SubTotalStudents, SchTotalGrade, SubStuAllGrades, SubTotalGrade) %>%
      ungroup() %>% group_by(URN) %>% OutputSubectStudentSchoolsByCriteriaTail(.)

    # map to MAT, Gender, Selective,

    names(df) <- c(c("Type"), names(df[,c(2:(length(names(df))))]))
  }

  ##### gender provision for mixed schools
  if(type == "mixedgender"){

    df <- data %>% select(GENDER, URN, schType, schGender, selective, !!subject) %>%
      mutate(schType = ifelse(regexpr("inde", schType) != -1, "Independent",
                              ifelse(regexpr("Selective", selective) != -1, "State Selective",
                                     "State Non Selective"))) %>%
      filter(!is.na(!!subject)) %>%
      group_by(schGender,schType, URN, GENDER) %>%
      summarise(n=n()) %>%
      spread(GENDER, n) %>%
      ungroup() %>%
      mutate(`F` = ifelse(is.na(`F`), 0, `F`),
             M = ifelse(is.na(M), 0, M)) %>%
      group_by(schGender,schType) %>%
      summarise(SD = sd(`F` + M),
                TotalF = sum(`F`),
                TotalM = sum(M),
                ZeroF = sum(`F` == 0),
                Institutions = n(),
                AvgSize = sum(TotalF, TotalM, na.rm=TRUE) / Institutions) %>%
      ungroup()

    names(df) <- c("Gender", "Type", "SD", "Female computing students", "Male computing students", "Providers with no females", "Total Computing Providers", "Average Size")
    df <- df[,c("Gender", "Type", "Total Computing Providers", "Female computing students", "Male computing students",  "Providers with no females")]

    df$`Percentage of providers` <- 100*(df$`Providers with no females` / df$`Total Computing Providers`)

    #TODO default to anonymising this data

    return(df)
  }

  ##### Local Education Authorities
  if(type == "lea"){
    #load local authorities
    LEAs <- loadLocalEducationAuthorities(dir=basefolder)

    df <- left_join(df, LEAs, by="URN")

    # keep track of missing data
    missing <- df %>% filter(is.na(`LA (name)`))
    message(paste("Missing LEA information for:", nrow(missing), " schools", sum(missing$SchTotalStudents), "students, check that Edubase school list and postcode lists are up to date"))

    df <- df %>% select(`GSSLACode (name)`,  `LA (name)`, SchTypeTotalSchools,
                        SchTotalStudents, SubTotalSchools, SubTotalStudents,
                        SchTotalGrade, SubStuAllGrades, SubTotalGrade) %>%
      ungroup() %>%
      group_by(`GSSLACode (name)`, `LA (name)`) %>%
      OutputSubectStudentSchoolsByCriteriaTail(.) %>%
      select(-`GSSLACode (name)`)

    names(df) <- c(c("Type"), names(df[,c(2:(length(names(df))))]))
  }

  ##### Regions
  if(type == "region"){
    # load Regions
    regions <- loadSchoolRegions(dir=basefolder)

    df <- left_join(df, regions %>% select(URN, Region), by="URN")
    missing <- df %>% filter(is.na(Region))
    message(paste("Missing regional information for:", nrow(missing), " schools", sum(missing$SchTotalStudents), "students, check that Edubase school list and postcode lists are up to date"))

    df <- df %>% select(Region, SchTypeTotalSchools, SchTotalStudents, SubTotalSchools, SubTotalStudents, SchTotalGrade, SubStuAllGrades, SubTotalGrade) %>%
      ungroup() %>% group_by(Region) %>% OutputSubectStudentSchoolsByCriteriaTail(.)

    names(df) <- c(c("Type"), names(df[,c(2:(length(names(df))))]))
  }


  ##### Provider type
  if(type == "type"){
    df <- df %>% select(schType, SchTypeTotalSchools, SchTotalStudents, SubTotalSchools, SubTotalStudents, SchTotalGrade, SubStuAllGrades, SubTotalGrade) %>%
      ungroup() %>% group_by(schType) %>% OutputSubectStudentSchoolsByCriteriaTail(.)

    names(df) <- c(c("Type"), names(df[,c(2:(length(names(df))))]))
  }

  ##### Provider chain
  if(type == "trust"){
    df <- df %>% select(trust, SchTypeTotalSchools, SchTotalStudents, SubTotalSchools, SubTotalStudents, SchTotalGrade, SubStuAllGrades, SubTotalGrade) %>%
      ungroup() %>% group_by(trust) %>% OutputSubectStudentSchoolsByCriteriaTail(.)

    names(df) <- c(c("Trust"), names(df[,c(2:(length(names(df))))]))
  }

  #only report on the gender of a school
  if(type == "schGender"){
    df <- df %>% select(schGender, SchTypeTotalSchools, SchTotalStudents, SubTotalSchools, SubTotalStudents, SchTotalGrade, SubStuAllGrades, SubTotalGrade) %>%
      ungroup() %>% group_by(schGender) %>% OutputSubectStudentSchoolsByCriteriaTail(.)

    names(df) <- c(c("Gender"), names(df[,c(2:(length(names(df))))]))
  }

  ##### Independent vs Selective vs Non Selective
  if(type == "selective"){
    df <- df %>% mutate(schType = ifelse(regexpr("inde", schType) != -1, "Independent",
                                         ifelse(regexpr("Selective", selective) != -1, "Grammar school",
                                                "State non-selective"))) %>%
      select(-selective, -schUrban, -URN) %>%
      group_by(schType) %>%
      arrange(schType) %>%
      OutputSubectStudentSchoolsByCriteriaTail(.)

    names(df) <- c(c("Type"), names(df[,c(2:(length(names(df))))]))

    # the definition of comprehensive and non comprehensive / secondary modern is messy so we clump them together
    # df %>% filter(regexpr("Inde", schType) != -1, regexpr("Selective", selective) != -1)
  }

  ##### Gender combined with Independent vs Selective vs Non Selective
  if(type == "selGender"){
    df <- df %>% mutate(schType = ifelse(regexpr("inde", schType) != -1, "Independent",
                                         ifelse(regexpr("Selective", selective) != -1, "Grammar school",
                                                "State non-selective"))) %>%
      select(-selective, -schUrban, -URN) %>%
      group_by(schGender, schType) %>%
      arrange(schGender, schType) %>%
      OutputSubectStudentSchoolsByCriteriaTail(.)

    names(df) <- c(c("Gender", "Type"), names(df[,c(3:(length(names(df))))]))
    #note that the code above doesn't differentiate on selective and non selective independent schools
    #df %>% filter(regexpr("Inde", schType) != -1, regexpr("Selective", selective) != -1)
  }

  ##### Urban Rural school types offering computing
  if(type == "UrbanRuralCombine"){

    #unique(df$schUrban)
    df <- df %>%
      select(-schType, -schGender, -selective, -URN) %>%
      filter(!is.na(schUrban)) %>%
      ungroup() %>%
      mutate(Type = ifelse(regexpr("Rural", schUrban) != -1, "Rural", "Urban")) %>%
      group_by(Type) %>%
      arrange(schUrban) %>%
      OutputSubectStudentSchoolsByCriteriaTail(.)

    names(df) <- c(c("Type"), names(df[,c(2:(length(names(df))))]))

  }

  ##### Urban Rural school types offering computing
  if(type == "UrbanRuralALL"){

    #unique(df$schUrban)
    df <- df %>%
      select(-schType, -schGender, -selective, -URN) %>%
      group_by(schUrban) %>%
      arrange(schUrban) %>%
      OutputSubectStudentSchoolsByCriteriaTail(.)

    names(df) <- c(c("Type"), names(df[,c(2:(length(names(df))))]))

  }

  ##### Coastal school types offering computing
  if(type == "Coastal"){
    if(!exists("basefolder") == TRUE){
      print("reseting base directory")
      basefolder = getwd()
    }

    coastalscutoff <- 5500 #distance in metres to the coast

    # loadcoastal schools WARNING THIS CAN TAKE SEVERAL MINUTES IF NOT PRE-CACHED
    coastalschools <- loadCoastalSchools(overwrite = FALSE, dir=basefolder)

    if(!"Coastal" %in% names(coastalschools)){

      coastalschools <- coastalschools %>%
        mutate(Coastal = ifelse(Distance <= 5500, TRUE, FALSE))
    }

    # join information on coastal schools
    df <- left_join(df, coastalschools, by="URN")

    df <- df %>%
      select(Coastal, SchTypeTotalSchools, SchTotalStudents, SubTotalSchools, SubTotalStudents, SchTotalGrade, SubStuAllGrades, SubTotalGrade) %>%
      group_by(Coastal) %>%
      arrange(Coastal) %>%
      OutputSubectStudentSchoolsByCriteriaTail(.) %>%
      ungroup() %>%
      mutate(Coastal = ifelse(Coastal, "Coastal", "Inland")) %>%
      rename(Type=Coastal)
  }

  #filter out any NA brought about by the #900000 URNs
  #TODO: work out what to do with these 67 GCSE providers
  df <- df[!is.na(df[1]),]
  df <- df[df[1] != "Not applicable",]  # filtering out that odd army school
  df <- df[df[1] != "",]  # filtering out odd A-level school without a rural / urban type

  # ANONYMISE DATA
  # IF THERE ARE ANY <= 6 FIGURES, then anonymise the whole dataset by rounding to nearest 5.
  # min(df %>% filter(SubTotalStudents != 0) %$% SubTotalStudents) < 6
  if(TRUE %in%
     c(df %>% filter(SubTotalStudents != 0) %$% SubTotalSchools >
       df %>% filter(SubTotalStudents != 0) %$% SubTotalStudents - 6) ){

    df <- df %>%
      mutate(SubTotalStudents =
               ifelse(SubTotalStudents == 0, 0,
                      ifelse(5*round(SubTotalStudents/5) == 0, 5, 5*round(SubTotalStudents/5))))
  }

  #add percentages
  df <- df %>%  mutate("% Schools"=round(100*(SubTotalSchools/SchTypeTotalSchools), digits = 1) ,
                       "% Students"=round(100*(SubTotalStudents/SchTotalStudents), digits = 1),
                       "AverageCohort" = ifelse(!is.finite(SubTotalStudents/SubTotalSchools), 0,
                                                round(SubTotalStudents/SubTotalSchools, digits = 1)),
                       "Grade Avg Sch" = ifelse(!is.finite(SchTotalGrade/SchTotalStudents), 0,
                                                round(SchTotalGrade/SchTotalStudents, digits = 1)),
                       "Grade Avg Sub Stu" = ifelse(!is.finite(SubStuAllGrades/SubTotalStudents), 0,
                                                    round(SubStuAllGrades/SubTotalStudents, digits = 1)),
                       "Grade Avg Sub" = ifelse(!is.finite(SubTotalGrade/SubTotalStudents), 0,
                                                round(SubTotalGrade/SubTotalStudents, digits = 1))
  )

  #add tally row at the bottom of the dataframe
  #in the case that the table has more than one descriptive column
  if(type == "selGender"){
    # ARRRANGE DATA BY GENDER and TYPE
    df <- df %>% arrange(Gender, Type)
    newRow <- addTotalRow(df, c("Gender", "Type"), grades)
  }else if(type %in% c("lea", "region")){

    df <- df %>% arrange(desc(`% Students`))

    newRow <- addTotalRow(df, c(names(df)[1]), grades)
  }else{    #for all other tables
    # ARRRANGE DATA BY TOTAL STUDENTS
    # message(paste("running:", type))
    # message(paste(names(df)))
    # message(nrow(df))
    df <- df %>% arrange(desc(SchTotalStudents))

    newRow <- addTotalRow(df, c(names(df)[1]), grades)
  }



  df <- rbind(df, newRow)
  df <- df %>% select(-SchTotalGrade, -SubStuAllGrades, -SubTotalGrade)

  #strip grades from return if not needed
  if(grades == TRUE){

    #adjust the names (leave the unique first one or two names)
    names(df) <- c(names(df[,c(1:(length(names(df)) - 10))]),
                   c("Total Schools", "Total Students", "Subject Providers", "Subject Students", "Providers %", "Students %",  "Average Cohort Size", "Grade Avg Sch", "Grade Avg Sub Students", "Grade Avg Subject"))

    #reorder the fields
    df <- df[,c(names(df[,c(1:(length(names(df)) - 10))]),
                c("Total Schools", "Total Students", "Subject Providers", "Providers %", "Subject Students", "Students %",  "Average Cohort Size", "Grade Avg Sch", "Grade Avg Sub Students", "Grade Avg Subject"))]
  }else{
    df <- df %>% select(-`Grade Avg Sch`, -`Grade Avg Sub Stu`, -`Grade Avg Sub`)
    #adjust the names (leave the unique first one or two names)
    names(df) <- c(names(df[,c(1:(length(names(df)) - 7))]),
                   c("Total Schools", "Total Students", "Subject Providers", "Subject Students", "Providers %", "Students %",  "Average Cohort Size"))

    #reorder the fields
    df <- df[,c(names(df[,c(1:(length(names(df)) - 7))]),
                c("Total Schools", "Total Students", "Subject Providers", "Providers %", "Subject Students", "Students %",  "Average Cohort Size"))]
  }

  #head(df)

  #anonymise results after all collation has been performed
  df <- df %>%
    mutate(  `Grade Avg Sch`      = ifelse(`Total Schools` > (`Total Students` - 5) & !is.na(`Total Students`) & .[[1]] != "Totals",
                                           ifelse(`Total Students` == 0,
                                                  as.character(0), "X"),
                                           as.character(`Grade Avg Sch`)),
             `Grade Avg Sub Students` = ifelse(`Subject Providers` > (`Subject Students` - 5) & !is.na(`Subject Students`) & .[[1]] != "Totals",
                                               ifelse(`Subject Students` == 0,
                                                      as.character(0), "X"),
                                               as.character(`Grade Avg Sub Students`)),
             `Grade Avg Subject` = ifelse(`Subject Providers` > (`Subject Students` - 5) & !is.na(`Subject Students`) & .[[1]] != "Totals",
                                          ifelse(`Subject Students` == 0,
                                                 as.character(0), "X"),
                                          as.character(`Grade Avg Subject`)),
             `Total Students`      = ifelse(`Total Schools` > (`Total Students` - 5) & !is.na(`Total Students`) & .[[1]] != "Totals",
                                            ifelse(`Total Students` == 0,
                                                   as.character(0), "X"),
                                            as.character(`Total Students`)),
             `Average Cohort Size` = ifelse(`Subject Providers` > (`Subject Students` - 5) & !is.na(`Subject Students`) & .[[1]] != "Totals",
                                            ifelse(`Subject Students` == 0,
                                                   as.character(0), "X"),
                                            as.character(`Average Cohort Size`)),
             `Students %` = ifelse(`Subject Providers` > (`Subject Students` - 5) & !is.na(`Subject Students`) & .[[1]] != "Totals",
                                   ifelse(`Subject Students` == 0,
                                          as.character(0), "X"),
                                   as.character(`Students %`)),
             `Subject Students` = ifelse(`Subject Providers` > (`Subject Students` - 5)& !is.na(`Subject Students`) & .[[1]] != "Totals",
                                         ifelse(`Subject Students` == 0,
                                                as.character(0), "X"),
                                         as.character(`Subject Students`)))

  return(df)

  #TODO: check that grammar % of students matches gov data
}



# output data for several years based on measure and specific items
# used for making longitudinal analysis
OutputTableByYearlyData <- function(data_name, grouping, measure, items, year_range, subject_name = "Computer science"){
  # seed empty table
  graph_data <- get(data_name) %>% select(grouping, measure) %>%
     mutate(Year = 0,Subject = "") %>% top_n(0)
  #
  # # data_name <- "table_GCSE_provider_type"
  # # measure <- "Students %"
  # # items <- c("Academy converter", "Community school", "Academy sponsor led", "Foundation school",
  #              "Voluntary aided school", "Other independent school", "Voluntary controlled school")
  # # grouping <- quo(Type)
  # # yr <- 16
  # grouping <- "Type"
  #
   for(yr in year_range){
     full_year <- as.numeric(paste0("20", yr))
     if(yr == year){
       yr <- ""
     }else{
       yr <- paste0("_", yr)
     }

     #message(yr)
     # yr <- "_16"
     # yr <- ""
     # full_year <- "2012"
     # print(paste(yr, full_year))
     print(paste0(data_name, yr))

     # a <- get(paste0(data_name, "")) %>% select(grouping, measure) %>% ungroup()    # %>% filter(get(grouping) %in% items)
     # b <- get(paste0(data_name, "_16")) %>% select(grouping, measure) %>% ungroup() # %>% filter(get(grouping) %in% items)
     #
     # dput(a)
     # dput(b)

     # unique(table_GCSE_provider_type$Type)
     # unique(table_GCSE_provider_type_16$Type)


     temp <- get(paste0(data_name, yr)) %>% select(grouping, measure) %>%
       filter(tolower(get(grouping)) %in% tolower(items)) %>%
       # mutate(!!measure := as.numeric(!!measure)) %>%
       mutate(Year = full_year,
              Subject = subject_name)
    #message(temp)
     graph_data <- rbind(graph_data, temp)
   }

  return(graph_data)
}


#Local Education Authority
OutputLEASummary <- function(spreadResults, subject){

  #spreadResults <- Aresults15
  #subject <- "X2610"

  #load local authorities
  LEAs <- loadLocalEducationAuthorities()

  temp <- spreadResults %>% select(PupilMatchingRefAnonymous, URN, get(subject))

  temp <- left_join(temp, LEAs)

  #get collated figures for each LEA Overall
  LEAData <- temp %>% group_by(LA..name., GSSLACode..name.) %>%
    summarise(totalRegion = n(),                      #get total students by region
              schRegion = length(unique(URN))) %>%    #get total schools by region
    filter(!is.na(LA..name.))

  #we're still missing 550 students due to URN: #900000
  #temp %>% filter(is.na(LA..name.))
  #names(temp)

  #get collated figures for each LA per subject
  SubData <- temp %>% group_by(LA..name., GSSLACode..name.) %>%
    filter_(paste0("!is.na(",subject,")")) %>%
    summarise(substuRegion = n(),                        #get subject students by region
              subschRegion = length(unique(URN))) %>% #get subject schools by region
    filter(!is.na(LA..name.))

  LEAData <- left_join(LEAData, SubData)

  head(LEAData)

  LEAData <- LEAData %>% mutate(subschRegion = ifelse(is.na(subschRegion), 0, as.numeric(subschRegion)),
                                substuRegion = ifelse(is.na(substuRegion), 0, as.numeric(substuRegion)))

  LEAData$PercentageSchools <- ((LEAData$subschRegion / LEAData$schRegion) * 100)
  LEAData$PercentageStudents <- ((LEAData$substuRegion / LEAData$totalRegion) * 100)
  LEAData$AvgCohort <- (LEAData$substuRegion / LEAData$subschRegion)

  #adjust the names (leave the unique first one or two names)
  names(LEAData) <- c("LEA Name", "Code", "Total\nStudents", "Total\nSchools","Subject\nStudents",
                      "Subject\nProviders", "Providers\n%", "Students\n%", "Average\nCohort\nSize")

  #reorder the fields
  LEAData <- LEAData[,c("LEA Name", "Total\nSchools", "Total\nStudents", "Subject\nProviders",
                          "Providers\n%", "Subject\nStudents", "Students\n%", "Average\nCohort\nSize", "Code")]

  LEAData <- addTotalRow(LEAData, c("LEA Name"))

  #sum(LEAData$`Subject\nStudents`)

  #to trim off the [,c(1:length(LEAData)-1)]
  LEAData <- LEAData %>% mutate(`Code` = ifelse(`LEA Name` == "Totals", NA, as.character(`Code`)))

  #TODO:  Anonymise data

  return(LEAData)
}


#get a summary of subjects taken for each LA
OutputLASummary <- function(spreadResults, subject){

  #spreadResults <- Gresults15

  #load local authorities
  localauthorities <- loadLocalAuthorities()

  #get collated figures for each LA Overall
  heatData <- spreadResults %>% group_by(area5) %>%
    summarise(totalRegion = n(),                      #get total students by region
              schRegion = length(unique(URN))) %>%    #get total schools by region
    filter(!is.na(area5))

  #get collated figures for each LA per subject
  SubData <- spreadResults %>% group_by(area5) %>%
    filter_(paste0("!is.na(",subject,")")) %>%
    summarise(substuRegion = n(),                        #get subject students by region
              subschRegion = length(unique(URN))) %>% #get subject schools by region
    filter(!is.na(area5))

  heatData <- left_join(heatData, SubData)

  heatData$PercentageSchools <- ((heatData$subschRegion / heatData$schRegion) * 100)
  heatData$PercentageStudents <- ((heatData$substuRegion / heatData$totalRegion) * 100)

  #add local authority names and add any local authorities where data = 0
  heatData <- left_join(heatData, localauthorities,
                        by=c("area5" = "Code"))

  #adjust the names (leave the unique first one or two names)
  names(heatData) <- c("Code","Total\nStudents", "Total\nSchools","Subject\nStudents", "Subject\nProviders", "Providers\n%", "Students\n%",  "LA Name")

  #reorder the fields
  heatData <- heatData[,c("LA Name", "Code", "Total\nSchools", "Total\nStudents", "Subject\nProviders",
                      "Providers\n%", "Subject\nStudents", "Students\n%")]
  return(heatData)
}

#output a subject map summary for 9 English regions, by student and school
OutputRegionSummary <- function(spreadResults, subject){

  postcodes <- loadPostcodes()

  #combine Region to spreadResults
  #add the Region data to each student record
  temp <- left_join(spreadResults %>%
                      select(PupilMatchingRefAnonymous, Postcode, URN, get(subject)),
                    postcodes[, c("Postcode", "Region")])

  #get collated figures for each region
  heatData <- temp %>% group_by(Region) %>%
    mutate(`Total\nStudents` = n(),                      #get total students by region
           `Total\nSchools` = length(unique(URN))) %>%    #get total schools by region
    filter_(paste0("!is.na(",subject,")")) %>%
    mutate(`Subject\nStudents` = n(),                        #get subject students by region
           `Subject\nProviders` = length(unique(URN))) %>% #get subject schools by region
    distinct(`Total\nStudents`, `Total\nSchools`, `Subject\nProviders`, `Subject\nStudents`) %>%
    filter(!is.na(Region))

  #sum(heatData$totalRegion)
  #sum(heatData$substuRegion)
  #sum(heatData$schRegion)
  #sum(heatData$subschRegion)

  heatData$`Providers\n%` <- ((heatData$`Subject\nProviders` / heatData$`Total\nSchools`) * 100)
  heatData$`Students\n%` <- ((heatData$`Subject\nStudents` / heatData$`Total\nStudents`) * 100)
  heatData$`Average\nCohort\nSize` <- ((heatData$`Subject\nStudents` / heatData$`Subject\nProviders`))

  #join the ID of each region into the dataframe so it links to the map
  heatData <- getRegionCodes(heatData)

  #reorder columns:
  heatData <- heatData[,c("Region","Code","Total\nSchools","Total\nStudents","Subject\nProviders"
                          ,"Providers\n%","Subject\nStudents","Students\n%","Average\nCohort\nSize")]

  #sort by number of school
  heatData <- heatData %>% arrange(desc(`Total\nStudents`))

  #add a total row:
  heatData <- addTotalRow(heatData, c("Region", "Code"))

  # TODO: at the moment we list all schools that offer a GCSE in anything,
  # do we need to filter this on secondary phase schools only using schPhase

  #sum(LA_data$students) 2014? 611157 For 2015 GCSE it was 600732
  #sum(LA_data$schools) 5244 For 2015 GCSE it was 5135 TODO: Where have all the schools gone?

  return(heatData)
}

OutputEBACCSummary <- function(spreadResults, subject="X2610"){
  #spreadResults <- Gresults15

  #make a vector of science subjects
  # X1110	Chemistry
  # X1210	Physics
  # X1300	Science (Core)
  # X1310	Science SA              #only X students sat this in 2015
  # X1320	Science (Additional)
  # X1330	Science: Dual Award A   #no entries 2015
  # 1350	Science: Dual Award B   #no entries 2015
  # 1370	Science: Double Award   #no entries 2015
  # 1390	Science: Double Award B #no entries 2015
  # 1410	Science: Biology & Chemistry  #no entries 2015
  # 1450	Science: Biology & Physics    #no entries 2015
  # 1470	Science: Chemistry & Physics  #no entries 2015
  # X1010	Biology
  # 1030	Biology: Human                #no entries 2015
  # 1050	Biology: Social               #no entries 2015
  # 1060	Biology: Human & Social       #no entries 2015


  sciences <- c("X2610","X1010","X1110","X1210","X1300","X1320")

  #convert the dataframe to record 1 for a c or above, and 0 otherwise
    temp <- spreadResults %>% select_(.dots=sciences) %>%
    mutate_each(funs(ifelse(.>=40,1,0))) %>%
    mutate(attemptcoreadd = !is.na(X1300) & !is.na(X1320),
           coreadd = ifelse(is.na(X1300) | is.na(X1320), FALSE,
                            ifelse(X1300 == 1 & X1320 == 1,TRUE,FALSE)),
           attemptsingles = ifelse(rowSums(!is.na(.[c("X2610","X1010","X1110","X1210")])) >=3, TRUE, FALSE),
           passedsingles = ifelse(rowSums(.[c("X2610","X1010","X1110","X1210")],na.rm=TRUE) >= 2 &
                                    attemptsingles,TRUE,FALSE),
           singlestaken = rowSums(!is.na(.[c("X2610","X1010","X1110","X1210")]))
    )

    #temp %>% filter(!is.na(X2610), singlestaken == 3) %>% summarise(n = n())

  temp <- temp %>% group_by(X2610,attemptsingles,passedsingles,singlestaken) %>% summarise(n=n())

  #TODO: get this to work for other sciences

  #get an A* to C in core and additional science GCSE (in core and additional science, pupils take 2 modules in each of the 3 main sciences: biology, chemistry and physics)
  #take 3 single sciences at GCSE and get an A* to C in at least 2 of them (the single sciences are biology, chemistry, computer science and physics)
  #get A* to C in GCSE science double award (in science double award, pupils take 2 GCSE exams that cover the 3 main sciences: biology, chemistry and physics)

  return(temp)

  #temp %>% filter(attemptcoreadd & attemptsingles) # 363 students attempted both qualifications!?
  #temp %>% filter(!attemptcoreadd & !attemptsingles) #181k not doing any EBACC combo

  #tempresults %>% filter(!is.na(X2610), singlestaken == 3) %>% ungroup() %>% summarise(n=sum(n))

  #296 / 33000 #2015 less than 1% of EBACC science grades relied on computing
}

#Output Pupil Premium data by gender
OutputPPGender <- function(spreadResults, subject="X2610"){
  temp <- spreadResults %>% select(EVERFSM_6, GENDER, !!subject)

  names(temp) <- c("EVERFSM_6", "GENDER","sub")

  temp <- temp %>%
    filter(!is.na(EVERFSM_6), !is.na(sub), !is.na(GENDER)) %>%
    group_by(GENDER) %>%
    mutate(total = n()) %>%
    group_by(GENDER,EVERFSM_6) %>%
    summarise(n = n(),
              per = round(100 * (n / max(total)), 2)) %>%
    ungroup()

  names(temp) <- c("Gender", "Pupil premium", "Computing students", "Percentage of gender taking computing")

  return(temp)
}

#Output Pupil Premium data by gender
OutputPPEthnicity <- function(spreadResults, subject="X2610"){
  temp <- spreadResults %>% select(EVERFSM_6, EthMaj, get(subject))

  names(temp) <- c("EVERFSM_6", "EthMaj","sub")

  temp <- temp %>%
    filter(!is.na(EVERFSM_6), !is.na(sub), !is.na(EthMaj)) %>%
    group_by(EthMaj) %>%
    mutate(total = n()) %>%
    group_by(EthMaj,EVERFSM_6) %>%
    summarise(n = n(),
              per = round(100 * (n / max(total)), 2)) %>%
    ungroup() %>% arrange(EVERFSM_6)

  names(temp) <- c("Ethnicity", "Pupil premium", "Computing students", "Percentage of ethnicity")

  return(temp)
}

#output a printable table for white working class males
OutputWWCSummary <- function(WWC,  sub="X2650", gender="F", PP=1){

  # WWC <- workingclass_students_CS

  #condense whole table into MajEth groupings
  MajEthWC <- WWC %>% ungroup() %>%
    group_by(Subject,SubjectName,SubTotaln,EthMaj,GENDER,EVERFSM_6) %>%
    summarise(PopEFGn = sum(PopEFGn),
              SubEGFn = sum(SubEGFn),
              representation=round(100 * (SubEGFn/PopEFGn),2))

  #get rows for WBRI and WHIOTH
  WWBri <- WWC %>% filter(EthMin=="WBRI")

  WWOth <- WWC %>% filter(EthMaj == "WHIT" & EthMin !="WBRI") %>% ungroup() %>%
    group_by(Subject,SubjectName,SubTotaln,EthMaj,GENDER,EVERFSM_6) %>%
    summarise(PopEFGn = sum(PopEFGn),
              SubEGFn = sum(SubEGFn),
              representation=round(100 * (SubEGFn/PopEFGn),2)) %>%
    ungroup()

  WWBri <- WWBri %>% select_(.dots=names(MajEthWC)) %>% mutate(EthMaj="WHIBRI")
  WWOth <- WWOth %>% select_(.dots=names(MajEthWC)) %>% mutate(EthMaj="WHIOTH")

  #####
  # WWOth %>% filter(Subject %in% c("X2650", "X2610"))
  # temp <- WWC %>% filter(Subject %in% c("X2650", "X2610"), EthMaj == "WHIT") %>%
  #  select(EthMaj, EthMin, GENDER, EVERFSM_6, Subject, PopEFGn) %>%
  #  arrange(EthMin, GENDER, EVERFSM_6, Subject)
  # temp <- Results_GCSE_Current %>% filter(SUBLEVNO == 310) %>% group_by(PupilMatchingRefAnonymous) %>%
  #  summarise(n = n()) %>% ungroup() %>% group_by(n) %>% summarise(total = n())
  #####

  #combine results
  MajEthWC <- bind_rows(MajEthWC, WWBri)
  MajEthWC <- bind_rows(MajEthWC, WWOth)

  #get rid of white only line
  MajEthWC <- MajEthWC %>% filter(EthMaj != "WHIT")

  tableout <- MajEthWC %>% ungroup() %>%
    filter(Subject==sub, GENDER==gender, EVERFSM_6==PP) %>%
    select(EthMaj, representation, SubEGFn, PopEFGn)

  tableout <- mutateEthnicity(tableout)

  if(PP == 1){
    non <- ""
  }else{
    non <- "non-"
  }

  if(gender == "M"){
    gen <- "males"
  }else{
    gen <- "females"
  }



  names(tableout) <- c("EthCode", "% taking computing", paste0("Computing ",non,"PP ", gen),
                       paste0("Total ",non,"PP ", gen), "Ethnicity")

  tableout <- tableout[c("Ethnicity", paste0("Total ",non,"PP ", gen),
                         paste0("Computing ",non,"PP ", gen), "% taking computing")]

  tableout <- tableout %>% arrange_(paste0("desc(`",names(tableout)[2],"`)"))

  totals <- tableout[1,]
  totals$Ethnicity <- "Total"
  totals[[2]] <- sum(tableout[[2]])
  totals[[3]] <- sum(tableout[[3]])
  totals[[4]] <- round(100*(totals[[3]]/totals[[2]]),2)

  tableout <- rbind(tableout, totals)

  return(tableout)
}

# returns one large table comparing ethnicity and PP for two subjects
OutputWWCTwoSubjects <- function(spread, sub1 = "X2610", sub1name = "CS", sub2 = "X2650", sub2name = "ICT"){

  # spread <- Spread_GCSE_16
  workingclass_students_CS <- OutputSubjectGroupWorkingClass(spread, sub1, 20)
  CS_wc_m <- OutputWWCSummary(workingclass_students_CS, sub1, "M", 1)
  CS_wc_m$Gender <- "M"
  CS_wc_m$`Pupil premium` <-  1
  names(CS_wc_m) <- c("Ethnicity", "n", "CS n", "CS %", "Gender", "Pupil premium")

  #####
  CS_wc_f <- OutputWWCSummary(workingclass_students_CS, sub1, "F", 1)
  CS_wc_f$Gender <- "F"
  CS_wc_f$`Pupil premium` <-  1
  names(CS_wc_f) <- c("Ethnicity", "n", "CS n", "CS %", "Gender", "Pupil premium")


  CS_nwc_m <- OutputWWCSummary(workingclass_students_CS, sub1, "M", 0)
  CS_nwc_m$Gender <- "M"
  CS_nwc_m$`Pupil premium` <-  0
  names(CS_nwc_m) <- c("Ethnicity", "n", "CS n", "CS %", "Gender", "Pupil premium")
  CS_nwc_f <- OutputWWCSummary(workingclass_students_CS, sub1, "F", 0)
  CS_nwc_f$Gender <- "F"
  CS_nwc_f$`Pupil premium` <-  0
  names(CS_nwc_f) <- c("Ethnicity", "n", "CS n", "CS %", "Gender", "Pupil premium")

  workingclass_students_ICT <- workingclass_students_CS # OutputSubjectGroupWorkingClass(spread, sub2, 20)
  ICT_wc_m <- OutputWWCSummary(workingclass_students_ICT, sub2, "M", 1)
  ICT_wc_m$Gender <- "M"
  ICT_wc_m$`Pupil premium` <-  1
  names(ICT_wc_m) <- c("Ethnicity", "n", "ICT n", "ICT %", "Gender", "Pupil premium")


  ######
  ICT_wc_f <- OutputWWCSummary(workingclass_students_ICT, sub2, "F", 1)
  ICT_wc_f$Gender <- "F"
  ICT_wc_f$`Pupil premium` <-  1
  names(ICT_wc_f) <- c("Ethnicity", "n", "ICT n", "ICT %", "Gender", "Pupil premium")


  ICT_nwc_m <- OutputWWCSummary(workingclass_students_ICT, sub2, "M", 0)
  ICT_nwc_m$Gender <- "M"
  ICT_nwc_m$`Pupil premium` <-  0
  names(ICT_nwc_m) <- c("Ethnicity", "n", "ICT n", "ICT %", "Gender", "Pupil premium")
  ICT_nwc_f <- OutputWWCSummary(workingclass_students_ICT, sub2, "F", 0)
  ICT_nwc_f$Gender <- "F"
  ICT_nwc_f$`Pupil premium` <-  0
  names(ICT_nwc_f) <- c("Ethnicity", "n", "ICT n", "ICT %", "Gender", "Pupil premium")

  CS_wc_m <- left_join(CS_wc_m, ICT_wc_m)
  CS_wc_f <- left_join(CS_wc_f, ICT_wc_f)
  CS_nwc_m <- left_join(CS_nwc_m, ICT_nwc_m)
  CS_nwc_f <- left_join(CS_nwc_f, ICT_nwc_f)

  all_students <- bind_rows(CS_wc_m, bind_rows(CS_wc_f, bind_rows(CS_nwc_m, CS_nwc_f)))
  names(all_students) <- c("Ethnicity", "n", paste0(sub1name," n"),  paste0(sub1name," %"), "Gender", "Pupil premium", paste0(sub2name," n"),  paste0(sub2name," %"))
  all_students <- all_students[c("Ethnicity","Gender", "Pupil premium","n", paste0(sub1name," n"), paste0(sub1name," %"), paste0(sub2name," n"), paste0(sub2name," %"))]
  all_students <- all_students %>% arrange(Gender, Ethnicity, `Pupil premium`)
  return(all_students)
}

# outputs stats on who has access to to and is taking a given subject, school n % etc
Output_access_to_subject <- function(spread, sub="X2610"){
  access <- NULL
  access$MAPPING <- sub

  All_URN <- spread %$% unique(URN)
  Sub_URN <- spread %>% filter_(paste0("!is.na(",sub,")")) %$% unique(URN)

  # number of schools offering the subject
  access$Schools_n <- length(Sub_URN)

  # percentage of all schools offering the subject
  access$Schools_per <- round(100 * (length(Sub_URN) / length(All_URN)),1)

  # number of students in  school offering given subject
  CS_School_Student_n <- spread %>% filter(URN %in% Sub_URN) %>% summarise(n = n())
  access$Student_n <- CS_School_Student_n$n

  # total number of students sitting any qualification
  All_School_Student_n <- spread %>% summarise(n = n())

  # percentage of all students in a school where subject is offered
  access$Student_per <- round(100*(CS_School_Student_n$n / All_School_Student_n$n),1)

  # number of students sitting the given subject
  access$Student_sub_n <- nrow(spread %>% filter_(paste0("!is.na(",sub,")")))

  # percentage of students sitting subject compared to population
  access$Student_sub_per <- round(100*(access$Student_sub_n/nrow(spread)),1)

  # percentage of students sitting subject compared to population
  access$Student_sub_possible_per <- round(100*(access$Student_sub_n/access$Student_n),1)

  return(access)
}

# output stats on who has access to and is taking all subjects, school n % etc
#names(Results_GCSE_17)
#spread <- Spread_GCSE_17
# length(Spread_GCSE_17 %>% filter(URN < 900000) %$% unique(URN))
#Results_GCSE_17 %>% group_by(SUBLEVNO, MAPPING) %>% summarise(n=n()) %>% arrange(desc(n)) %>% top_n(10,n)

Output_access_to_subjects <- function(spread){
  access_to_subjects <- data.frame()
  for (MAPPING in names(spread)[grepl("X",names(spread))]){
    output <- Output_access_to_subject(spread, MAPPING)

    # spread %>%
    #   mutate(All_students = n(),
    #          All_providers = length(unique(URN))) %>%
    #   select(PupilMatchingRefAnonymous, URN, All_students, All_providers,names(spread)[grepl("X",names(spread))]) %>%
    #   gather(`Subject code`, Grade, -PupilMatchingRefAnonymous, -URN, -All_students, -All_providers) %>%
    #   filter(!is.na(Grade)) %>%
    #   left_join(spread %>% group_by(`Subject code`) %>%)
    #   group_by(`Subject code`) %>%
    #   summarise(`Providers n` = length(unique(URN)),
    #             `Providers %` = printper0(length(unique(URN)) / All_providers),
    #             `Students n able to sit subject`,
    #             `Students % able to sit subject`,
    #             `Students sitting subject` = length(unique(PupilMatchingRefAnonymous)),
    #             `Student % of all students` = printper0(length(unique(PupilMatchingRefAnonymous)) / All_students),
    #             `Student % of able students`)




    message(output)
    access_to_subjects <- bind_rows(access_to_subjects, output)
  }

  sub_names <-loadDiscMappings()
  sub_names$MAPPING <- paste0("X",sub_names$MAPPING)
  access_to_subjects <- left_join(access_to_subjects, sub_names)

  names(access_to_subjects) <- c("Subject code", "Providers n","Providers %",
                               "Students n able to sit subject","Students % able to sit subject",
                               "Students sitting subject", "Student % of all students", "Student % of offered students", "Subject" )

  access_to_subjects <- access_to_subjects[c("Subject","Subject code", "Providers n","Providers %",
                                              "Students n able to sit subject","Students % able to sit subject",
                                              "Students sitting subject", "Student % of all students", "Student % of offered students" )]
  return(access_to_subjects)
}

# output a table showing the percentages of cohort taking given subject for each URN
Ouput_subject_cohort_percentage <- function(spread, sub, resolution=10){
  tmp <- spread %>%
    mutate(sub = ifelse(is.na(!!sub), 0, 1)) %>%
    group_by(URN, sub) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    group_by(URN) %>%
    mutate(total = sum(n),
           per = ifelse(sub == 1,
                        round_any(100 * (n/total), resolution, f= ceiling),
                        0)) %>%
    ungroup() %>%
    group_by(URN, total) %>%
    summarise(per = max(per)) %>% mutate(Subject = quo_name(sub))
  return(tmp)
}


#get the number of students taking a given subject per school
OutputSchoolsGivenSubjectX <- function(spreadResults, subject="X2610"){

  school_data <- spreadResults %>%
    select(PupilMatchingRefAnonymous, URN, area5, schType, schGender, selective, Easting, Northing, get(subject)) %>%
    group_by(URN) %>%
    mutate(totalStudents = n()) %>%
    filter_(paste0("!is.na(",subject,")")) %>%
    mutate(subStudents = n()) %>%
    select(URN, area5, schType, schGender, selective, Easting, Northing, totalStudents, subStudents) %>%
    ungroup() %>%
    distinct(URN, area5, schType, schGender, selective, Easting, Northing, totalStudents, subStudents) %>%
    mutate(schMapping = as.character(ifelse(regexpr("Independent", schType) != -1, "Independent",
                                            ifelse(regexpr("Selective", selective) != -1, "Selective", "Non-Selective"))))



  #sum(temp_schools$totalStudents)
  #sum(temp_schools$subStudents)

  school_data$PercentageSchools <- ((school_data$subStudents / school_data$totalStudents) * 100)

  return(school_data)
}

#get summary results by subject, GENDER, EVERFSM_6, EthMaj, EthMin, SEN, EAL,
getSubjectGradeDemographics <- function(spreadResults, subject, level="GCSE"){

  # spreadResults <- Spread_Alevel_Current
  # subject <- quo(X2610)
  # level <- "Alevel"

  #TODO: how are we treating X and U? both score 0 points

  cols <- c(as.name("GENDER"),
            as.name("EVERFSM_6"),
            as.name("EthMaj"),
            as.name("EthMin"),
            as.name("SEN"),
            as.name("EAL"),
            as.name(quo_name(subject)))

  subSummary <- spreadResults %>%
    select(GENDER, EVERFSM_6, EthMaj, EthMin, SEN, EAL, !!subject) %>%
    filter(!is.na(!!subject)) %>%
    group_by_(.dots=cols) %>%
    summarise(total = n()) %>%
    ungroup()

  names(subSummary)[names(subSummary) == quo_name(subject)] <- "grade"

  subSummary$EVERFSM_6 <- as.factor(subSummary$EVERFSM_6)

  subSummary <- convertGrades(subSummary, level)

  return(subSummary)
}

#get summary results by subject, GENDER, EVERFSM_6, EthMaj, EthMin, SEN, EAL
OutputSubjectGradeDemographicSummary <- function(subSummary, focus=c("GENDER", "EVERFSM_6")){

  cols <- c(focus, "grade", "total")

  totals <- subSummary %>% select(!!cols) %>%
    group_by_(.dots=cols[-length(cols)]) %>%
    summarise(total=sum(total)) %>%
    ungroup() %>%
    spread(grade, total) %>%
    mutate(type = "n")

  percentages <- subSummary %>% select(!!cols) %>%
    group_by_(.dots=cols[-length(cols)]) %>%
    summarise(total=sum(total)) %>%
    ungroup() %>%
    group_by_(.dots=cols[c(1:(length(cols)-2))]) %>%
    mutate(grand_total=sum(total),
           `%` = round(100 * (total / grand_total), 1)) %>%
    ungroup() %>%
    select(-grand_total, -total) %>%
    spread(grade, `%`) %>%
    mutate(type = "%")

  exclude <- paste0("!is.na(",focus,")")
  if (length(exclude) > 1){
    exclude <- paste(exclude, collapse = "|")
  }

  output <- bind_rows(totals, percentages) %>%
    group_by_(.dots=cols[c(1:(length(cols)-2))]) %>%
    arrange_(.dots=c(cols[c(1:(length(cols)-2))],"type")) %>%
    mutate_all(funs(as.character(.))) %>%
    filter_(exclude)

  output <- output[c(focus, "type", sort(unique(subSummary$grade)))] %>% rename(` ` = type)

  return(output)
}

#get summary results by subject, grade and FSM
OutputSubjectGradeFSMSummary <- function(subSummary){

  output <- subSummary %>% select(EVERFSM_6, grade, total) %>%
    group_by(EVERFSM_6, grade) %>% summarise(total = sum(total)) %>%
    ungroup() %>%
    spread(grade, total) %>%
    rename(`Pupil premium` = EVERFSM_6)

  return(output)
}

#outputs the number of subjects taken per student as a population and for a particular subject
OutputSubjectChoicesNumberPerStudent <- function(spreadResults, subject="X2610"){

  #get the names of subjects
  subnames <- names(spreadResults[,grep("X[0-9]", names(spreadResults))])

  temp <- spreadResults %>% mutate(totalSubs = rowSums(!is.na(.[subnames]))) %>% select(EVERFSM_6, get(subject), totalSubs)

  ### get the overall subject distribution for all subjects

  overall <- temp %>% group_by(totalSubs) %>% summarise(total = n())
  overall$grandtotal <- overall$totalSubs * overall$total
  overall <- anonymiseGraph(overall)

  ######get the distribution for given subject
  sub <- temp %>% filter_(paste0("!is.na(",subject,")")) %>% group_by(totalSubs) %>% summarise(total = n())
  sub$grandtotal <- sub$totalSubs * sub$total
  sub <- anonymiseGraph(sub)

  names(sub) <- c("totalSubs","subTotal","subgrandTotal","subAnon")

  ######get the distribution for Pupil Premium
  pp <- temp %>% filter(!is.na(EVERFSM_6)) %>% group_by(EVERFSM_6, totalSubs) %>% summarise(total = n())

  pp1 <- pp %>% filter(EVERFSM_6 == 1)
  pp1$grandtotal <- pp1$totalSubs * pp1$total
  pp1 <- anonymiseGraph(pp1)[-1]
  names(pp1) <- c("totalSubs","pp1Total","pp1grandTotal","pp1Anon")

  pp0 <- pp %>% filter(EVERFSM_6 == 0)
  pp0$grandtotal <- pp0$totalSubs * pp0$total
  pp0 <- anonymiseGraph(pp0)[-1]
  names(pp0) <- c("totalSubs","pp0Total","pp0grandTotal","pp0Anon")

  overall <- left_join(overall, sub)
  overall <- left_join(overall, pp1)
  overall <- left_join(overall, pp0)

  #plotSubjectsperStudent(temp)

  return(overall)
}


##############
### OUTPUT SUBJECT GROUPINGS
##############
#get the average of any given item in the spreadResults dataframe
#FOR USE BELOW ONLY
OutputOverallAvg <- function(data, what="KS2Mat", fact=FALSE){
  #data  <- spreadResults

  #rename for ease of manipulation
  data <- data %>% rename_(.dots=setNames(list(what), "what"))

  if(fact == TRUE){
    temp <- data %>% select(what) %>%
      filter(!is.na(what)) %>%
      mutate(tot_n = n()) %>%
      group_by(what) %>%
      summarise(n =n(),
                mean = printper0(n/max(tot_n)),
                sd=NA)
  }else{
    temp <- data %>% select(what) %>%
      filter(!is.na(what)) %>%
      mutate(what = as.character(what)) %>%
      mutate(what = as.numeric(what)) %>%
      summarise_all(funs(mean(., na.rm=TRUE), sd(.,  na.rm=TRUE)))

  }

  temp$n <- sum(!is.na(data %>% select(what)))

  #names(temp) <- c("Average")
  temp$Subject <- "All"

  return(temp)
}

#get the Overall Cplus Passrate for any given 'what'
#FOR USE BELOW ONLY
OutputCplusPassOverallAvg <- function(spreadResults, what="EVERFSM_6", level){
  ALLsubjects <- names(spreadResults[grep("X[0-9]", names(spreadResults))])

  gathercols <- as.vector(ALLsubjects)

  cols <- append(what, gathercols)

  if(level == "GCSE"){
    temp <- spreadResults %>% select_(.dots=cols) %>%
      filter(!is.na(!!what)) %>%
      gather_("Subject", "Grade", gathercols, na.rm=TRUE) %>%
      group_by_(quo_name(what)) %>%
      mutate(Cpass = ifelse(Grade >= 5, 1,0)) %>%
      select(Grade, Cpass, what) %>%
      summarise_all(funs(mean(.)))
  }
  if(level == "Alevel"){
    temp <- spreadResults %>% select_(.dots=cols) %>%
      filter(!is.na(!!what)) %>%
      gather_("Subject", "Grade", gathercols, na.rm=TRUE) %>%
      group_by_(quo_name(what)) %>%
      mutate(Cpass = ifelse(Grade >= 210, 1,0)) %>%
      select(Grade, Cpass, what) %>%
      summarise_all(funs(mean(.)))
  }

  temp$Subject <- "All"

  temp <-  temp[c("Subject", what, "Grade", "Cpass")]

  return(temp)
}

#output a list of subjects by Percentage of students with Ethnicity Major
OutputSubjectGroupEthinicity <- function(spreadResults, focus="X2610"){
  subjects <- buildSubjectList(spreadResults, focus, 20)

  #make a vector of the desired columns
  dots <- append("EthMaj", as.vector(subjects$ID))

  temp <- spreadResults %>% select_(.dots = dots) %>%
    gather(Subject, Grade, -EthMaj, na.rm=TRUE) %>%
    group_by(Subject, EthMaj) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    group_by(Subject) %>%
    mutate(SubjectTotal = sum(n)) %>%
    group_by(EthMaj) %>%
    mutate(Percentage = 100 * (n / SubjectTotal))

  temp <- left_join(temp, subjects[,c(1,3)], by=c("Subject" = "ID"))

  return(temp)
}

#one function to return subject average ___OF___ the given "what"
# fact determines whether the waht is a factor (TRUE) or numeric (FALSE)
OutputSubjectGroupAverageByWhat <- function(spreadResults, focus="X2610", what="KS2Mat", subsize=20, fact=FALSE){

  #TODO: Debug this to make sure that standard deviations are working correctly
  #TODO: check that we want to trim out the NAs when working out averages.
  #       See filter(!is.na(what)) %>%

  #what <- "X2610"
  #what <- "X1110"
  #what <- "something"
  # what <- "GENDER"

  # spreadResults <- Spread_GCSE_17
  # fact = TRUE

  #if looking to match to another subject, make a copy of it
  if(length(grep("X[0-9]", what)) == 1){
    spreadResults$temp <- spreadResults[[what]]
    what <- "temp"
  }

  #get a list of the largest subjects to compare focus with
  subjects <- buildSubjectList(spreadResults, focus, subsize)

  #rename what field to 'what' for easier manipulation
  temp <- spreadResults %>% rename_(.dots=setNames(list(what), "what"))

  #make a vector of the columns to focus on
  dots <- append("what", as.vector(subjects$ID))

  #all columns to gather excluding the 'what'
  gathercols <- as.vector(subjects$ID)

  #get stats for each subject
  temp <- temp %>% select_(.dots = dots) %>%
    filter(!is.na(what)) %>%
    gather_("Subject", "Grade", gathercols, na.rm=TRUE) %>%
    filter(!is.na(what)) %>%
    select(-Grade)

  # what does this do?
  if(fact == TRUE){
    temp <- temp %>%
      group_by(Subject) %>%
      mutate(tot_n=n()) %>%
      ungroup() %>%
      group_by(Subject, what) %>%
      summarise(n = n(),
                mean = printper0(n/max(tot_n)),
                sd=NA) %>%
      ungroup()
  }else{
    temp <- temp %>%
      mutate(what = as.character(what)) %>%
      mutate(what = as.numeric(what)) %>%
      group_by(Subject) %>%
      mutate(n = sum(!is.na(what))) %>%
      ungroup() %>%
      group_by(Subject, n) %>%
      summarise_all(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE))) %>%
      ungroup()
  }


  #get overall stats for whole population
  all <- OutputOverallAvg(spreadResults, what, fact)

  temp <- left_join(temp, subjects %>% select(ID, SubjectName), by=c("Subject" = "ID"))
  temp <- bind_rows(temp, all)
  #fix subject name of All
  temp <- temp %>%
    mutate(SubjectName =
      ifelse(is.na(SubjectName), "ALL", as.character(SubjectName))) %>%
    arrange(desc(mean))

  temp$SubjectName <- as.vector(temp$SubjectName)
  #temp$SubjectName <- factor(temp$SubjectName,temp$SubjectName)

  #create an additional column to highlight the focus subject:
  temp$Highlight <- ifelse(temp$Subject == focus, temp$mean,0)

  return(temp)
}

#gets cumulative Cplus grades ___OF___ a given "what"
OutputSubjectCplusPassByWhat <- function(spreadResults, level="Alevel", focus=quo(X2610), what="GENDER", subsize=20){

  # spreadResults <- Spread_GCSE_Current

  #get a list of the largest subjects to compare focus with
  subjects <- buildSubjectList(spreadResults, quo_name(focus), subsize)

  #make a vector of the desired columns
  cols <- append(what, as.vector(subjects$ID))

  gathercols <- as.vector(subjects$ID)

  if(level == "GCSE"){
    temp <- spreadResults %>% select_(.dots = cols) %>%
      gather_("Subject", "Grade", gathercols, na.rm=TRUE) %>%
      filter(!is.na(!!what)) %>%
      group_by_("Subject", what) %>%
      mutate(Cpass = ifelse(Grade >= 5, 1,0)) %>%
      summarise_all(funs(mean(.)))
    #TODO: check standard deviation
  }
  if(level == "Alevel"){
    temp <- spreadResults %>% select_(.dots = cols) %>%
      gather_("Subject", "Grade", gathercols, na.rm=TRUE) %>%
      filter(!is.na(!!what)) %>%
      group_by_("Subject", what) %>%
      mutate(Cpass = ifelse(Grade >= 210, 1,0)) %>%
      summarise_all(funs(mean(.)))
    #TODO: check the UCAS points allocated here
    #TODO: check standard deviation
  }


  #get overall stats for whole population
  all <- OutputCplusPassOverallAvg(spreadResults, what, level)
  temp <- bind_rows(temp, all)
  temp <- left_join(temp, subjects[,c(1,3)], by=c("Subject" = "ID"))
  #temp$Cpass <- temp$Cpass * 100

  #fix subject name of All
  temp <- temp %>% mutate(SubjectName =
                            ifelse(is.na(SubjectName), "ALL", as.character(SubjectName))) %>%
    arrange_(append(what,as.vector("Cpass")))

  temp$SubjectName <- as.vector(temp$SubjectName)
  temp$SubjectName <- factor(temp$SubjectName,unique(temp$SubjectName))

  #create an additional column to highlight the focus subject:
  temp$Highlight <- ifelse(temp$Subject == quo_name(focus), temp$Cpass,0)

  return(temp)
}


#output a list of subjects by Schooltype
OutputSubjectGroupSchType <- function(spreadResults, focus="X2610"){
  subjects <- buildSubjectList(spreadResults, focus, 20)

  #make a vector of the desired columns
  dots <- append("schType", as.vector(subjects$ID))

  temp <- spreadResults %>% select_(.dots = dots) %>%
    gather(Subject, Grade, -schType, na.rm=TRUE) %>%
    group_by(Subject, schType) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    group_by(Subject) %>%
    mutate(SubjectTotal = sum(n)) %>%
    group_by(schType) %>%
    mutate(Percentage = 100 * (n / SubjectTotal))

  temp <- left_join(temp, subjects[,c(1,3)], by=c("Subject" = "ID"))

  return(temp)
}

#outputs details on working class entry by gender (currently returns all ethnicities)
OutputSubjectGroupWorkingClass <- function(spreadResults, focus="X2610", subsize=20){

  # spreadResults <- Spread_GCSE_16

  #see page 10 of https://www.lkmco.org/wp-content/uploads/2016/07/The-underrepresentation-of-white-working-class-boys-in-higher-education-baars-et-al-2016.pdf
  #which argues that White working class boys can be seen as White British only

  subjects <- buildSubjectList(spreadResults, focus, subsize)

  #make a vector of the desired columns
  dots <- append("EthMaj", append("EthMin",append("GENDER",append("EVERFSM_6", as.vector(subjects$ID)))))

  WWBTotals <- spreadResults %>% select_(.dots = dots) %>%
    gather(Subject, Grade, -EthMaj, -EthMin, -GENDER, -EVERFSM_6, na.rm=TRUE) %>%
    group_by(Subject, EthMaj, EthMin, GENDER, EVERFSM_6) %>%
    summarise(SubEGFn = n()) %>%
    ungroup() %>%
    group_by(Subject) %>%
    mutate(SubTotaln = sum(SubEGFn)) %>% #get total students for each subject
    group_by(Subject, GENDER, EVERFSM_6) %>%
    mutate(TotalGenderFSMn = sum(SubEGFn)) %>% #get total for each gender
    ungroup() # %>% complete(Subject, GENDER, EVERFSM_6)
    #rowwise()
    #mutate(SubGenderFSMPercentage = 100*(SubEGFn / TotalGenderFSMn)) #get percentage

  # create an empty dataframe with Subject EthMaj EthMin GENDER EVERFSM_6 so we don;t get different totals elsewhere
  AllCombinations<- expand.grid(unique(WWBTotals$Subject),
              unique(WWBTotals$GENDER),
              unique(WWBTotals$EVERFSM_6),
              unique(WWBTotals$EthMaj),
              unique(WWBTotals$EthMin))
  names(AllCombinations) <- c("Subject","GENDER","EVERFSM_6", "EthMaj", "EthMin")

  #filter to valid ethnicity combinations only
  EthCombinations <- WWBTotals %>% mutate(EthComb = paste(EthMaj, EthMin)) %>% distinct(EthComb) %$% EthComb
  AllCombinations <- AllCombinations %>%
    mutate(EthComb = paste(EthMaj, EthMin)) %>%
    filter(EthComb %in% EthCombinations) %>%
    select(-EthComb)


  # merge with data for: SubTotaln TotalGenderFSM
  AllCombinations <- left_join(AllCombinations, WWBTotals %>% distinct(Subject,SubTotaln), by=c("Subject"))

  AllCombinations <- left_join(AllCombinations,
                               WWBTotals %>% distinct(Subject, GENDER, EVERFSM_6, TotalGenderFSMn),
                               by=c("Subject", "GENDER", "EVERFSM_6"))

  WWBTotals <- left_join(AllCombinations,
                    WWBTotals,
                    by=c("Subject", "GENDER", "EVERFSM_6", "EthMaj", "EthMin", "SubTotaln", "TotalGenderFSMn" )) %>%
          mutate(SubEGFn = ifelse(is.na(SubEGFn), 0, SubEGFn))

  # temp <- WWBTotals %>% filter(Subject %in% c("X2650", "X2610"), EthMaj == "WHIT", EthMin != "WBRI")



    #  select(EthMaj, EthMin, GENDER, EVERFSM_6, Subject, PopEFGn) %>%
    #  arrange(EthMin, GENDER, EVERFSM_6, Subject)

  #head(WWBTotals %>% filter(Subject=="X2610"))
#  sum(WWBTotals %>% filter(Subject=="X2610") %$% SubEGFn)


  #add names to the dataframe
  WWBTotals <- left_join(WWBTotals, subjects[c(1,3)], by=c("Subject"="ID"))

  #check that figure is correct DONE
  #sum((WWBTotals %>% ungroup() %>% filter(Subject == "X2610"))$SubEGFn)

  #ADD DATA ON ETHNICITY, GENDER, FSM FOR ALL SUBJECTS
  #names(spreadResults)
  ALL <- spreadResults %>% select(GENDER, EVERFSM_6, EthMaj, EthMin) %>%
    group_by(EthMaj, EthMin, GENDER, EVERFSM_6) %>%
    summarise(SubEGFn = n()) %>%
    mutate(Subject = "All", SubjectName = "All") %>%
    ungroup() %>%
    mutate(SubTotaln = sum(SubEGFn)) %>% #get total students for ALL population
    group_by(Subject, GENDER, EVERFSM_6) %>%
    mutate(TotalGenderFSMn = sum(SubEGFn)) %>% #get total for each gender, subject and FSM
    ungroup() #%>%
    #rowwise()
    #mutate(SubGenderFSMPercentage = 100*(SubEGFn / TotalGenderFSMn)) #get percentage

  #check that figure is correct DONE
  #sum((ALL %>% ungroup() %>% distinct(GENDER, EVERFSM_6, TotalGenderFSMn))$TotalGenderFSMn)
  #spreadResults %>% summarise(count = n())

  #combine ALL and subject totals
  #check names are the same
  #setdiff(names(WWBTotals),names(ALL))
  temp <- rbind(ALL, WWBTotals)



  names(ALL) <- c("EthMaj", "EthMin", "GENDER", "EVERFSM_6", "PopEFGn",
                  "Subject", "SubjectName", "SubTotaln", "TotalGenderFSMn")

  #####HACKING HERE TODO, replication of data on left_join below
  #FIXED?
  #sum(temp %>% filter(Subject=="X2610") %$% SubEGFn)

  #sum(WWBTotals %>% filter(Subject=="X2610", EthMin != "") %$% SubEGFn)
  #nrow(temp)
  #nrow(WWBTotals)

  WWBTotals <- left_join(temp, ALL[c("EthMaj", "EthMin", "GENDER", "EVERFSM_6", "PopEFGn")],
            by=c("EthMaj"="EthMaj", "EthMin"="EthMin", "GENDER"="GENDER", "EVERFSM_6"="EVERFSM_6"))

  #get representation for each subject, gender, fsm combination
  WWBTotals$representation <- round(100 * (WWBTotals$SubEGFn / WWBTotals$PopEFGn),2)

  #order the data for a graph
  WWBTotals <- WWBTotals %>% arrange(representation)

  #adjust the levels so that the graph is correctly ordered
  WWBTotals$SubjectName <- as.vector(WWBTotals$SubjectName)
  WWBTotals$SubjectName = factor(WWBTotals$SubjectName,unique(WWBTotals$SubjectName))

  #create an additional column to highlight the focus subject:
  WWBTotals$Highlight = ifelse(WWBTotals$Subject == focus, WWBTotals$representation,0)

  WWBTotals <- WWBTotals %>% filter(!is.na(GENDER),!is.na(EVERFSM_6))

  return(WWBTotals)
}

#outputs figures on how often a subject is offered with another subject
OutputSubjectOfferCombo <- function(spreadResults, sub1="X2610", sub2="X2650"){

  #filter to results from given subjects
  temp <- spreadResults %>% select(URN, get(sub1), get(sub2)) %>%
    filter_(paste0("!is.na(",sub1,") | !is.na(",sub2,")"))

  names(temp) <- c("URN", "sub1", "sub2")

  #note when subjects are taken together
  temp <- temp %>%  mutate(sub1present = ifelse(is.na(sub1), FALSE, TRUE),
                   sub2present = ifelse(is.na(sub2), FALSE, TRUE)) %>%
    group_by(URN) %>%
    summarise(sub1present = ifelse(sum(sub1present) >= 1, 1, 0),
              sub2present = ifelse(sum(sub2present) >= 1, 1, 0),
              intersection = ifelse(sub1present & sub2present, 1, 0)) %>%
    ungroup() %>%
    summarise(sub1 = sum(sub1present),
              sub2 = sum(sub2present),
              intersection = sum(intersection))

  return(temp)
}


#outputs other subjects taken with given subject
OutputSubjectGroupCombinations <- function(spreadResults, focus="X2610"){
  #spreadResults <- Spread_GCSE_Current
  # focus <- comparison_subjects[[x]]

  # get total number of subjects
  size <- length(grep("X[0-9]", colnames(spreadResults)))

  #get cohort size of each subject
  subjects <- buildSubjectList(spreadResults, quo_name(focus), size)

  # get all the subject result columns
  subCombinations <- spreadResults %>% select(grep("X[0-9]", colnames(spreadResults))) %>%
    filter(!is.na(!!focus)) %>%
    gather(Subject, Grade, na.rm=TRUE) %>%
    group_by(Subject) %>%
    summarise(combonum = n()) %>%
    arrange(desc(combonum)) %>%
    ungroup()

  subCombinations <- left_join(subCombinations, subjects, by=c("Subject" = "ID"))

  #adjust the levels so that the graph is correctly ordered
  subCombinations$SubjectName <- as.vector(subCombinations$SubjectName)
  subCombinations$SubjectName = factor(subCombinations$SubjectName,subCombinations$SubjectName)

  #one type of percentage
  subNum <- subCombinations %>% filter(Subject == quo_name(focus)) %>% select(combonum)
  subCombinations$SubPercentage <- 100 * (subCombinations$combonum / subCombinations$n)
  subCombinations$Percentage <- 100 * (subCombinations$combonum / subNum$combonum)

  return(subCombinations)
}


#get school summary showing FSM, Gender, School Type, Location for each Subject
getSchoolSummaryRAW <- function(comResults){
  #head(school_data)
  #head(comResults)
  #aggregate by school and subject

  schools <- loadSchools()
  postcodes <- loadPostcodes()

  #names(comResults)
  #head(schools[!(schools$Postcode %in% postcodes$Postcode), c(1,11,13,14)])

  names(comResults)

  #temp <- comResults %>% filter(URN.x == "100001") %>% select(PupilMatchingRefAnonymous)


  #unique(temp$PupilMatchingRefAnonymous)

  school_data <- comResults %>%
    group_by(URN) %>%
    mutate(SchTotalStu = n_distinct(PupilMatchingRefAnonymous)) %>%
    ungroup() %>%
    group_by(URN, SchTotalStu, MAPPING, MAPPING_DESCRIPTION, EVERFSM_6, GENDER) %>%
    summarise(SchTotalEntry = n()) %>%
    ungroup() %>%
    mutate(MAPPING_DESCRIPTION=gsub("[^A-Za-z0-9]", "", MAPPING_DESCRIPTION)) %>%
    left_join(schools[,c(1,3,4,6,10,11,12,13,14)], by="URN") %>%
    left_join(postcodes[,c(1,3,4,9)], by="Postcode") %>%
    rename(laua = area5) %>%
    mutate(SchType = ifelse(regexpr("Inde", TypeOfEstablishment..name.) != -1, "Independent",
                            ifelse(regexpr("Selective", AdmissionsPolicy..name.) != -1, "Selective",
                                   "Comprehensive")))

  # All the schools that are missing Easting or Northing, suggesting that postcode match didn't work
  temp <- unique(school_data[xor(is.na(school_data$Northing), is.na(school_data$northing)), ][c(1, 7, 13, 14, 15, 16)])
  school_data[!(is.na(school_data$Northing)) & is.na(school_data$northing), ]$northing <- school_data[!(is.na(school_data$Northing)) & is.na(school_data$northing), ]$Northing
  school_data[!(is.na(school_data$Easting)) & is.na(school_data$easting), ]$easting <-  school_data[!(is.na(school_data$Easting)) & is.na(school_data$easting), ]$Easting
  school_data[!(is.na(school_data$northing)) & is.na(school_data$Northing), ]$Northing <- school_data[!(is.na(school_data$northing)) & is.na(school_data$Northing), ]$northing
  school_data[!(is.na(school_data$easting)) & is.na(school_data$Easting), ]$Easting <-  school_data[!(is.na(school_data$easting)) & is.na(school_data$Easting), ]$easting

  # what about the schools with no Northing and northing, where are they!?
  temp <- unique(school_data[is.na(school_data$Northing) & is.na(school_data$northing), ][c(1, 7, 13, 14, 15, 16)])



  return(school_data)
}

#Output total students by Region
OutputStuByRegion <- function(school_data, LA_data){
  ###############
  #Output total students per region (for london and given subject)
  ###############

  #print(paste("TOTAL Students:", length(unique(results_stu$PupilMatchingRefAnonymous))))

  comp_results <- school_data %>%
    filter(MAPPING==2610) %>%
    select(URN, MAPPING, EVERFSM_6, Total, laua) %>%
    group_by(EVERFSM_6, laua) %>%
    summarise(TotalFSM = sum(Total))

  print(sum(comp_results$Total))

  comp_results <- LA_data %>%
    filter(MAPPING==2610)

  print(sum(comp_results$SubTotal))

  #get London Borough results
  comp_results <- comp_results[comp_results$laua %in% c("E09000001","E09000002","E09000003","E09000004","E09000005","E09000006",
                                                        "E09000007","E09000008","E09000009","E09000010","E09000011","E09000012",
                                                        "E09000013","E09000014","E09000015","E09000016","E09000017","E09000018",
                                                        "E09000019","E09000020","E09000021","E09000022","E09000023","E09000024",
                                                        "E09000025","E09000026","E09000027","E09000028","E09000029","E09000030",
                                                        "E09000031","E09000032","E09000033"),]

  print(comp_results[comp_results$SubTotal < 20,])

  print(sum(comp_results$SubTotal))

  write.csv(comp_results, file = paste(folder,"comp15.csv",sep=""))

}

#Output total students by School Type
OutputSchoolType <- function(school_data){
  ###############
  #Output total number of schools and total students per school type
  ###############

  names(school_data)

  temp <- school_data %>%
    group_by(SchType) %>%
    mutate(EngTotalSchools = n_distinct(URN), SchTypeTotalQuals=sum(SchTotalEntry)) %>%
    ungroup() %>%
    group_by(SchType, MAPPING, MAPPING_DESCRIPTION, EngTotalSchools, SchTypeTotalQuals) %>%
    mutate(SubTotSchools=n_distinct(URN)) %>%
    ungroup() %>%
    group_by(SchType, MAPPING, MAPPING_DESCRIPTION, GENDER, EngTotalSchools, SchTypeTotalQuals, SubTotSchools) %>%
    summarise(GenTotStudents=sum(SchTotalEntry), GenTotSchools=n_distinct(URN)) %>%
    ungroup() %>%
    filter(MAPPING == 2610)

  #TODO: SchTotalStudents is giving total qualifications, not total students
  #  group_by(SchType, URN) %>% ####
  #  mutate(SchTypeTotStudents=sum(SchTotalStu)) %>% ####
  #  ungroup() %>% ####
  #  select(SchType,EngTotalSchools,SchTypeTotalQuals,SchTypeTotStudents)####

  #2610 == ICT
  #2650 == Computing

  write.csv(temp, file = paste(folder,"csv output/SchoolType",keystage,year,".csv",sep=""))
}

#Output FSM, Gender
OutputGenderbySubjectandRegion <- function(school_data, LA_data){
  ###############
  #summarise results by Gender
  ###############

  comp_results <- school_data %>%
    filter(MAPPING==2610) %>%
    select(URN, MAPPING, GENDER, Total, laua) %>%
    group_by(GENDER, laua) %>%
    summarise(TotalGender = sum(Total))

  comp_results %>% group_by(GENDER) %>% summarise(Total = sum(TotalGender))

  #get London Borough results
  comp_results <- comp_results[comp_results$laua %in% c("E09000001","E09000002","E09000003","E09000004","E09000005","E09000006",
                                                        "E09000007","E09000008","E09000009","E09000010","E09000011","E09000012",
                                                        "E09000013","E09000014","E09000015","E09000016","E09000017","E09000018",
                                                        "E09000019","E09000020","E09000021","E09000022","E09000023","E09000024",
                                                        "E09000025","E09000026","E09000027","E09000028","E09000029","E09000030",
                                                        "E09000031","E09000032","E09000033"),]

  print(sum(comp_results$TotalGender))

  comp_results %>% group_by(GENDER) %>% summarise(Total = sum(TotalGender))
}
