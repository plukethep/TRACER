year <- 15
keystage <- "KS4"
level <- c(310,311,320)

# populate spreadResults for further analysis
# WARNING: this takes 30-60 minutes

Main()
for(yr in year_range){
  # 310 = GCSE; 111 = A-Level; 121 = AS-Level
  initialiseDataFrames("KS4", "GCSE",   as.character(yr))
  initialiseDataFrames("KS5", "Alevel", as.character(yr))
}

# initiliase all the cleaned dataframes into cleaned folder
# make spread using clean students and clean results
Main <- function(years = c(12:16)){
  postcodes <- loadPostcodes()
  schools <- loadSchools()
  
  # initialise cleaned data and spreads
  for(keystage in c("KS4", "KS5")){
    
    if(keystage == "KS4"){
      # no need to match the ata from previous years
      match_old = FALSE
      level <- c(310)
      AllGCSE <- NULL
    }
    if(keystage == "KS5"){
      # build up matching KS4 data to fill in missing fields
      for (yr in years){
        AllGCSE[[yr]] <- get(paste0("Students_GCSE_",yr))
      }
      match_old = TRUE
      level <- c(111)
    }
    
    for(yr in years){
      outputCleanSpread(outputCleanStudents(yr,keystage, match=match_old, matches=AllGCSE), 
                    outputCleanResults(yr,keystage), 
                    schools, postcodes, level, yr, keystage)
    }
  }
}
