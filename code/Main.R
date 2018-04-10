# WARNING: you must load functions from these files before running main command
codefolder <- getwd()
source(paste0(codefolder,'Tools.R'))
source(paste0(codefolder,'Collation.R'))
source(paste0(codefolder,'Graphs.R'))
source(paste0(codefolder,'Loading.R'))
source(paste0(codefolder,'Tables.R'))
source(paste0(codefolder,'Statistics.R'))
source(paste0(codefolder,'PrettyPrinting.R')) 

# clean the datasets, clean data will go into TRACER/data/cleaned
# WARNING: this takes 30-60 minutes
Main(years = c(12:17), keystages = c("KS4","KS5"))

#load the data into RAM
for(yr in year_range){
  # 310 = GCSE; 111 = A-Level; 121 = AS-Level
  message("loading 20",yr)
  initialiseDataFrames("KS4", "GCSE",   as.character(yr))
  initialiseDataFrames("KS5", "Alevel", as.character(yr))
}

# initiliase all the cleaned dataframes into cleaned folder
# make spread using clean students and clean results
Main <- function(years = c(12:17), keystages = c("KS4", "KS5")){

  message("loading all data for years:", years)

  postcodes <- loadPostcodes()
  schools <- loadSchools()

  # initiliase list of all GCSEs for matching to Alevel
  AllGCSEStudents <- NULL
  AllGCSEResults <- NULL

  # initialise cleaned data and spreads
  for(keystage in keystages){
    # keystage <- "KS5"

    message("cleaning ", keystage)

    if(keystage == "KS4"){
      # no need to match the ata from previous years
      match_old = FALSE
      level <- c(310, 391) # 2017 added 391 to cope with 1-9 GCSE grades
      AllGCSE <- NULL
      QualName <- "GCSE"
      what_to_match_person <- NULL
      what_to_match_results <- NULL
    }
    if(keystage == "KS5"){
      # build up matching KS4 data to fill in missing fields
      for (yr in years){
        AllGCSEStudents[[yr]] <- get(paste0("Students_GCSE_",yr))
        AllGCSEResults[[yr]] <- get(paste0("Results_GCSE_",yr))
      }
      match_old = TRUE
      level <- c(111)
      QualName <- "Alevel"
      what_to_match_person <- list("EthMaj" = "EthMaj",
                           "EthMin" = "EthMin",
                           "EVERFSM_6" = "EVERFSM_6",
                           "IDACIScore" = "IDACIScore",
                           "EAL" = "EAL",
                           "KS2Mat" = "KS2Mat",
                           "KS2Eng" = "KS2Eng")
      what_to_match_results <- list( "X2210" = "GCSEMaths")
    }

    for(yr in years){
      # yr <- 16
      # temp_students <- Students_Alevel_17

      message("cleaning ", keystage," for year ", yr)

      temp_students <- outputCleanStudents(yr,
                                           keystage,
                                           what_to_match_person = what_to_match_person,
                                           what_to_match_results = what_to_match_results,
                                           match=match_old,
                                           matches_students=AllGCSEStudents,
                                           matches_results=AllGCSEResults)

      # make sure that all GENDER values are upper
      # added to deal with errors in 2017 A-level student set 107 lower case entries
      temp_students <- temp_students %>% mutate(GENDER = ifelse(!is.na(GENDER), toupper(GENDER), GENDER))

      temp_results <- outputCleanResults(yr,keystage)

      ##### TODO: finds the repeat QANs
      # temp <- temp_results %>%
      #   group_by(PupilMatchingRefAnonymous, URN, SUBLEVNO, MAPPING) %>% summarise(n = n()) %>%
      #   filter(n > 1)
      #
      # repeat_QANs <- temp %>% ungroup() %>% group_by(MAPPING) %>% summarise(n=n()) %>% arrange(desc(n))
      #
      # write_csv(repeat_QANs, "C:/tmp/repeat_QANs.csv")
      # we're dropping about 0.5% of results, mostly 1320 (additional Science) nd CN1 (IT stuff)

      ### find out which student numbers for computing for each QAN
      # temp_results %>% filter(MAPPING == "2210") %>%
      #   group_by(MAPPING, QAN) %>%
      #   summarise(n = n()) %>%
      #   arrange(desc(n))

      # filter out multiple QANs for a student
      single_QAN_results <- temp_results %>% # filter out repeat QANs
        group_by(PupilMatchingRefAnonymous, URN, SUBLEVNO, MAPPING) %>%
        filter(POINTS == max(POINTS)) %>% #get rid of repeated grades, only use the highest grade
        ungroup() %>%
        arrange(desc(POINTS)) %>%  #deals with case X and U both being worth 0 points. distinct picks X over U at the moment
        distinct(PupilMatchingRefAnonymous, URN, SUBLEVNO, MAPPING, POINTS)

      #TODO: fix this properly.., it will distort maths results
      # adjust the 2017+ results to deal with 1-9 grading for maths and two English exams
      if(yr >= 17 & keystage == "KS4"){
        single_QAN_results <- single_QAN_results %>%
          mutate(POINTS = case_when(!MAPPING %in% c(2210, 5030, 5110) & POINTS == 8.5 ~ 8.0,
                                    !MAPPING %in% c(2210, 5030, 5110) & POINTS == 7.0 ~ 7.0,
                                    !MAPPING %in% c(2210, 5030, 5110) & POINTS == 5.5 ~ 6.0,
                                    !MAPPING %in% c(2210, 5030, 5110) & POINTS == 4.0 ~ 5.0,
                                    !MAPPING %in% c(2210, 5030, 5110) & POINTS == 3.0 ~ 4.0,
                                    !MAPPING %in% c(2210, 5030, 5110) & POINTS == 2.0 ~ 3.0,
                                    !MAPPING %in% c(2210, 5030, 5110) & POINTS == 1.5 ~ 2.0,
                                    !MAPPING %in% c(2210, 5030, 5110) & POINTS == 1.0 ~ 1.0,
                                    TRUE ~ as.double(POINTS)))
      }


      ### check to see if any repeats get through.
      # single_QAN_results %>% group_by(PupilMatchingRefAnonymous, URN, SUBLEVNO, MAPPING) %>% summarise(n = n()) %>%
      #   filter(n > 1)


      temp_spread <- outputCleanSpread(
                        temp_students,
                        single_QAN_results,
                        schools,
                        postcodes,
                        level,
                        yr,
                        keystage)

      # load clearned data into local memory
      initialiseDataFrames(keystage, QualName,   as.character(yr), get_local = TRUE)
    }
  }

  #clear list of GCSE data
  AllGCSEStudents <- NULL
  AllGCSEResults <- NULL
}
