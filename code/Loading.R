#################
#####LOADING DATA
#################

# load students, results and spreads into RAM for each keystage year combo
initialiseDataFrames <- function(keystage, qual, year){
  # keystage="KS4"
  # qual="GCSE"
  # year = 16
  # level = c(310)
  
  Students_filename <- paste0(cleanfolder, "CleanStudents_20", year, keystage,".csv")
  Results_filename <- paste0(cleanfolder, "CleanResults_20", year, keystage,".csv")
  Spread_filename <- paste0(cleanfolder, "CleanSpread_20", year, keystage,".csv")
  
  if(!file.exists(Students_filename) | !file.exists(Results_filename) | !file.exists(Spread_filename)){
    print(paste("ERROR: missing cleaned file(s) for", year))
    print(paste(Students_filename, "|",Results_filename))
    print("create them with outputCleanStudents(..) or Main()")
  }else{
    assign(paste0("Students_", qual,"_", year), read_csv(Students_filename, col_names = TRUE), envir = .GlobalEnv)
    assign(paste0("Results_", qual,"_", year), read_csv(Results_filename, col_names = TRUE), envir = .GlobalEnv)
    
    # fix column types for new readr implementation, which sets some results columns to character 2017-10-02    
    assign(paste0("Spread_", qual,"_", year), read_csv(Spread_filename, col_names = TRUE) %>% mutate_all(funs(type.convert(as.character(.)))), envir = .GlobalEnv)
    # sapply(temp, class)
    
    print(paste("Init", keystage, qual, year))
  }
}


#load all results for a particular year
loadResults <- function(year, keystage, n=0){
  warning("loading results")
  # year <- 15
  # keystage <- "KS5"

  filename <- paste(getwd(), "/data/results/", keystage, "Results_20",year,".txt",sep="")

  if(year == 16){
      ### get column names of wanted fields ###
      fields <- read.csv( text="MatchString,NewName
                          PupilMatchingRefAnonymous, PupilMatchingRefAnonymous
                          _URN$, URN
                          EXAMYEAR, EXAMYEAR
                          SUBLEVNO,SUBLEVNO
                          QAN, QAN
                          MAPPING, MAPPING
                          GRADE, GRADE
                          POINTS$, POINTS")
  }else{
    ### get column names of wanted fields ###
    fields <- read.csv( text="MatchString,NewName
                        PupilMatchingRefAnonymous, PupilMatchingRefAnonymous
                        _URN$, URN
                        EXAMYEAR, EXAMYEAR
                        SUBLEVNO,SUBLEVNO
                        QAN, QAN
                        MAPPING, MAPPING
                        GRADE, GRADE
                        POINTS, POINTS")
  }



  #TODO: check that KS4_POINTS_PTQ is the correct field for getting points for KS4

  #build a vector to load only the desired columns from the file
  ff <- buildCSVfilter(getColumnIds(fields, filename, "\t"), filename, "\t")

  if (n==0){# get all records
    results <- read_delim(filename, delim="\t", col_names = TRUE, col_types = ff) #, n_max=Inf)
  }else{
    results <- read_delim(filename, delim="\t", col_names = TRUE, col_types = ff, n_max=n)
  }
  results <- replaceIds(results, fields)

  return(results)
}

#load all students for a particular year
loadStudents <- function(year, keystage, n=0){
  warning("loading students")
  
  
  # year <- 16
  # keystage <- "KS4"

  if (keystage == "KS4"){
    filename <- paste(getwd(), "/data/students/", keystage, "Pupil_20",year,"_Census.txt",sep="")
  }
  if (keystage == "KS5"){
    filename <- paste(getwd(), "/data/students/", keystage, "CandInd_20",year,"_KS4_KS3_Census.txt",sep="")
  }

  #URN used to include
  #",
  #keystage, "_URN$|$URN_, URN
  # All years have the format where KSX_URN supply the URN of a student. However, this changes for KS5 2016 where it is URN_SPR16

  if(year == 16){
    ### get column names of wanted fields ###
    fields <- read.csv( text=paste0("MatchString,NewName
                                    PupilMatchingRefAnonymous, PupilMatchingRefAnonymous
                                    _GENDER, GENDER\n",
                                    paste0(keystage,"_URN$, URN"),
                                    "\nEthnicGroupMinor_, EthMin
                                    EthnicGroupMajor_, EthMaj
                                    FSMeligible_, FSMeligible
                                    EVERFSM_3, EVERFSM_3
                                    EVERFSM_6, EVERFSM_6
                                    IDACIScore_15, IDACIScore
                                    KS4_IDACI, IDACIScore
                                    SENprovision_, SEN
                                    SENprovisionMajor_, SENMaj
                                    KS2ENG, KS2Eng
                                    KS2MAT, KS2Mat
                                    SENPS, SENPS
                                    SENA, SENA
                                    SCITALEV, KS3Sci
                                    ENGTALEV, KS3Eng
                                    MATTALEV, KS3Mat
                                    _EAL, EAL
                                    KS4_FSM, KS4_FSM"))
  }else if(keystage == "KS5" & year == 16){
    ### get column names of wanted fields ###
    fields <- read.csv( text=paste0("MatchString,NewName
                                    PupilMatchingRefAnonymous, PupilMatchingRefAnonymous
                                    _GENDER, GENDER
                                    URN_SPR16$, URN
                                    EthnicGroupMinor_, EthMin
                                    EthnicGroupMajor_, EthMaj
                                    FSMeligible_, FSMeligible
                                    EVERFSM_3, EVERFSM_3
                                    EVERFSM_6, EVERFSM_6
                                    IDACIScore_15, IDACIScore
                                    KS4_IDACI, IDACIScore
                                    SENprovision_, SEN
                                    SENprovisionMajor_, SENMaj
                                    KS2ENG, KS2Eng
                                    KS2MAT, KS2Mat
                                    SENPS, SENPS
                                    SENA, SENA
                                    SCITALEV, KS3Sci
                                    ENGTALEV, KS3Eng
                                    MATTALEV, KS3Mat
                                    _EAL, EAL
                                    KS4_FSM, KS4_FSM"))
  }else if(keystage == "KS5" & year == 12){
    ### NOTE, this ignores EAL as there is a double match ###
    #"DUPLICATE MATCH ERROR on  _EAL : 626 KS4_EAL_P2APS"   "DUPLICATE MATCH ERROR on  _EAL : 627 KS4_EAL_P2APSSQ"

    warning(paste("matching KS5 2012, ignoring EAL"))

    fields <- read.csv( text=paste0("MatchString,NewName
                                    PupilMatchingRefAnonymous, PupilMatchingRefAnonymous
                                    _GENDER, GENDER\n",
                                    paste0(keystage,"_URN$, URN"),
                                    "\nEthnicGroupMinor_, EthMin
                                    EthnicGroupMajor_, EthMaj
                                    FSMeligible_, FSMeligible
                                    EVERFSM_3, EVERFSM_3
                                    EVERFSM_6, EVERFSM_6
                                    IDACIScore, IDACIScore
                                    KS4_IDACI, IDACIScore
                                    SENprovision_, SEN
                                    SENprovisionMajor_, SENMaj
                                    KS2ENG, KS2Eng
                                    KS2MAT, KS2Mat
                                    SENPS, SENPS
                                    SENA, SENA
                                    SCITALEV, KS3Sci
                                    ENGTALEV, KS3Eng
                                    MATTALEV, KS3Mat
                                    KS4_FSM, KS4_FSM"))



  } else{
    ### get column names of wanted fields ###
    fields <- read.csv( text=paste0("MatchString,NewName
                                    PupilMatchingRefAnonymous, PupilMatchingRefAnonymous
                                    _GENDER, GENDER\n",
                                    paste0(keystage,"_URN$, URN"),
                                    "\nEthnicGroupMinor_, EthMin
                                    EthnicGroupMajor_, EthMaj
                                    FSMeligible_, FSMeligible
                                    EVERFSM_3, EVERFSM_3
                                    EVERFSM_6, EVERFSM_6
                                    IDACIScore, IDACIScore
                                    KS4_IDACI, IDACIScore
                                    SENprovision_, SEN
                                    SENprovisionMajor_, SENMaj
                                    KS2ENG, KS2Eng
                                    KS2MAT, KS2Mat
                                    SENPS, SENPS
                                    SENA, SENA
                                    SCITALEV, KS3Sci
                                    ENGTALEV, KS3Eng
                                    MATTALEV, KS3Mat
                                    _EAL, EAL
                                    KS4_FSM, KS4_FSM"))

  }




  # IDACIScore_10_SPR16:  IDACI (Income Deprivation Affecting Children Indices) score derived from the pupil's postcode; based on Index of Multiple Deprivation (IMD) 2010.   (As held in Spring Census 2016 – SPR16)
  #IDACIScore_15_SPR16:  IDACI (Income Deprivation Affecting Children Indices) score derived from the pupil's postcode; based on Index of Multiple Deprivation (IMD) 2015.   (As held in Spring Census 2016 – SPR16)

  #removed IDACIRank_, IDACIRank

  #NOTE THAT _URN$, URN -> keystage, "_URN$, URN as KS5 results tables had multiple URNs for each keystage


  ## KS4 Only fields
  #"KS2ENG","KS2MAT",IDACIScore_, IDACIRank_

  ## KS5 Only fields
  #"KS4_FSM","SENPS","SENA","SCITALEV","ENGTALEV","MATTALEV", KS4_IDACI

  #KS5 to add?
  #"KS5_NCN"               "KS5_GRADEA_GA"                 "KS4_FSM"

  #build a vector to load only the desired columns from the file
  ff <- buildCSVfilter(getColumnIds(fields, filename, "\t"), filename, "\t")

  if (n==0){ #load everything
    warning("loading all students")
    students <- read_delim(filename, delim="\t", col_names = TRUE, col_types = ff) #, n_max=Inf)
   
    
  }else{
    warning(paste("loading", n,"students"))
    students <-  read_delim(filename, delim="\t", col_names = TRUE, col_types = ff, n_max=n)
  }


  # replace column names
  students <- replaceIds(students, fields)

  # clean data to get FSM/PP for KS5
  if (keystage == "KS5"){
    # NOTE different order of checks give different numbers of FSM/PP students.
    # reasoning here is if they were FSM in KS4 then EVERFSM and FSM Eligible at
    # KS5 might not have been updated to reflect this

    #if data provided has KS4_FSM provided

    if("KS4_FSM" %in% names(students)){
      students <- students %>% mutate(EVERFSM_6 = ifelse(!is.na(KS4_FSM), KS4_FSM,
                                                         ifelse(!is.na(EVERFSM_6), EVERFSM_6,
                                                                ifelse(!is.na(EVERFSM_3), EVERFSM_3,
                                                                       FSMeligible))))
    }else{
      students <- students %>% mutate(EVERFSM_6 = ifelse(!is.na(EVERFSM_6), EVERFSM_6,
                                                         ifelse(!is.na(EVERFSM_3), EVERFSM_3,
                                                                FSMeligible)))
      warning("KS4_FSM unavailable, try to merge from GCSE")
    }


    #Find out most commonly used FSM data for KS5
    #students %>% select(FSMeligible_SPR14, EVERFSM_3_SPR14, EVERFSM_6_SPR14, KS4_FSM) %>%
    #  gather(FSMType, Outcome) %>%
    #  group_by(FSMType, Outcome) %>%
    #  summarise(n = n())

    #Find out how many of the blank KS4_FSM can be filled by FSMeligible_SPR14
    #  students %>% select(FSMeligible_SPR14, EVERFSM_3_SPR14, EVERFSM_6_SPR14, KS4_FSM) %>%
    #  filter(is.na(KS4_FSM)) %>% group_by(FSMeligible_SPR14) %>% summarise(n = n())
  }

  #631673
  #deal with duplicates
  students <- students %>% distinct() %>% arrange(PupilMatchingRefAnonymous, EthMaj, IDACIScore)

  # NOTE: at this stage we delete students with multiple schools, this makes up ~0.07% of a cohort
  warning(paste0("Due to duplicate PupilRefNo, deleting ", nrow(students) - length(unique(students$PupilMatchingRefAnonymous)), " out of ", nrow(students), " records"))

  #get any duplicated students
  IDs <- as.vector(students[duplicated(students$PupilMatchingRefAnonymous),]$PupilMatchingRefAnonymous)

  students <- students %>% filter(!PupilMatchingRefAnonymous %in% IDs)

  #which students have the same school and multiple entries?!
  #t <- students %>% distinct(PupilMatchingRefAnonymous, URN)
  #IDs <- as.vector(students[duplicated(students$PupilMatchingRefAnonymous, students$URN),]$PupilMatchingRefAnonymous)


  #631664

  #TODO: deal with data missing from student records where student attend two institutions, keep this data

  #TODO: deal with students who have an ID and URN but are entered twice!
  # students %>% aggregate()
  # aggregate(x=DF[c("v1","v2","v3","v4")], by=list(name=DF$name), min, na.rm = TRUE)

  #TODO - output which students have repeated records to determine which ones we have kept
  #nrow(students[duplicated(students),])
  #students[students$PupilMatchingRefAnonymous=="####",]

  return(students)
}

#load and sanitise results filtering on qual type (SUBLEVNO)
loadStudentResults <- function(year, keystage, level, n=0){
  #year <- 16
  #keystage <- "KS4"
  #n=0
  results <- loadResults(year, keystage, n)

  ##TODO: check what we should do with results from previous years being claimed in one year##

  # 0 resulst for iGCSE
  # temp <- results %>%
  #   group_by(SUBLEVNO, MAPPING, EXAMYEAR) %>%
  #   filter(GRADE != "X" ) %>%
  #   summarise(n=n()) %>%
  #   filter(MAPPING == 2610)

  # Filter on requested levels only:
  # KS4 GCSE $SUBLEVNO = c(310,311,320)
  # KS5 ALevel $SUBLEVNO = c(110, 111, 120, 121)

  if(!is.null(level)){
    results <- subset(results, results$SUBLEVNO %in% level)
  }

  ##### SHOW which exams are attached to which EXAMYEAR #####
  #head(unique(results$EXAMYEAR))
  #
  #results %>% group_by(EXAMYEAR) %>% summarise(n = n())
  #
  #results %>% group_by(EXAMYEAR, SUBLEVNO) %>% filter(SUBLEVNO == 310) %>% summarise(n = n())

  ##delete duplicates, keeping the top score.
  results <- results %>%
    filter_(paste0("EXAMYEAR == \"20",year,"\"")) %>%    #TODO: check that we need this line
    select(PupilMatchingRefAnonymous, URN, MAPPING, POINTS, GRADE, SUBLEVNO) %>%
    group_by(PupilMatchingRefAnonymous, MAPPING) %>%
    filter(POINTS == max(POINTS)) %>% #get rid of repeated grades, only use the highest grade
    ungroup() %>%
    arrange(GRADE) %>%  #deals with case X and U both being worth 0 points. distinct picks X over U at the moment
    distinct(PupilMatchingRefAnonymous, URN, SUBLEVNO, MAPPING, POINTS) #gets rid of two grades earning same points

  #TODO: deal with X results, how to do this?

  #TODO: deal with students having results from multiple schools - currently all records are deleted in the loadStudents()
  #length(unique(temp$PupilMatchingRefAnonymous))
  #nrow(unique(temp %>% select(PupilMatchingRefAnonymous, URN)))

  #print(paste("the following students appear in two schools", length(unique(IDs$PupilMatchingRefAnonymous))," deleting them"))

  #these students appear to be double counted, so delete them
  #doubleCount <- temp %>% filter(PupilMatchingRefAnonymous %in% IDs$PupilMatchingRefAnonymous) %>% group_by(PupilMatchingRefAnonymous, URN) %>% summarise(n = n())
  #results %>% filter(PupilMatchingRefAnonymous == "#####")
  #nrow(results)
  #results <- results %>% filter(!PupilMatchingRefAnonymous %in% IDs$PupilMatchingRefAnonymous)

  return(results)
}

#load cleaned results for a particular year and keystage
loadCleanedResults <- function(year, keystage){
  #year <- 16
  #keystage <- "KS4"

  filename <- paste0(getwd(), "/data/cleaned/CleanResults_20",year, keystage,".csv")
  results <- read_csv(filename, col_names = TRUE)
  return(results)
}

#load cleaned students for a particular year and keystage
loadCleanedStudents <- function(year, keystage){
  #year <- 16
  #keystage <- "KS4"

  filename <- paste(getwd(), "/data/cleaned/CleanStudents_20",year, keystage,".csv",sep="")
  results <- type_convert(read_csv(filename, col_types = cols(.default = "c")))
  return(results)
}

# laod schoosl from Edubase data
loadSchools <- function(dir=getwd(), overwrite = FALSE){
  print("loading schools")
  #schools from: http://www.education.gov.uk/edubase/home.xhtml
  if (!exists("schools") | overwrite)
  {
    filename <- paste(dir, "/data/schools/", "schools.csv", sep="")

    # removed ^LA..code., LA..code.

    fields <- read.csv( text="MatchString,NewName
                        EstablishmentName, Name
                        URN, URN
                        Trusts..name., trust
                        GSSLACode..name., LA..code.
                        GOR..name., Region
                        TypeOfEstablishment..name., schType
                        PhaseOfEducation..name., schPhase
                        Gender..name., schGender
                        ReligiousCharacter..name., ReligiousCharacter..name.
                        AdmissionsPolicy..name., selective
                        NumberOfPupils, NumberOfPupils
                        NumberOfBoys, NumberOfBoys
                        NumberOfGirls, NumberOfGirls
                        PercentageFSM, PercentageFSM
                        Postcode, Postcode
                        UrbanRural..name., schUrban
                        Easting, Easting
                        Northing, Northing")

    ff <- buildCSVfilter(
      getColumnIds(fields, filename, s=","), 
      filename, ",")

    #read the data for each school
    schools <- read_delim(filename, delim=",", col_names = TRUE, col_types = ff, n_max=Inf)
    #schools <- read_csv  (filename, head=TRUE,sep=",", colClasses = ff)

    #get rid of all spaces in postcodes
    schools$Postcode <- gsub(" ", "", schools$Postcode, fixed = TRUE)

    #give friendlier names to schools fields
    schools <- replaceIds(schools, fields)
  }

  #hack to make Yorkshire and the humber match
  schools <- schools %>% mutate(Region = ifelse(Region == "Yorkshire and the Humber", "Yorkshire and The Humber", as.character(Region)))
  return(schools)
}


loadDiscMappings <- function(){
  filename <- paste(getwd(), "/data/qualifications/", "MappingCodes.txt",sep="")
  DiscCodes <- read_delim(filename, col_names = TRUE, delim="\t")

  return(DiscCodes)
}

loadSUBLEVNO <- function(){
  filename <- paste(getwd(), "/data/qualifications/", "SUBLEVNO.csv",sep="")
  SUBLEVNO <- read_csv(filename, col_names = TRUE, delim=",")

  return(SUBLEVNO)
}

#give all the computing subject desscriptions
loadComputingSubjectNames <- function(){
  subNames <- c("Office Technology","Computer Appreciation / Introduction","ICT",
                "Computing","D&T Sys & Control","Handling & Interpreting Data",
                "Applied ICT","Music Technology (Electronic)","Computer Architecture / Systems",
                "Keyboarding","Applications","WebSite Development", "Computer help","desk Operations",
                "Systems / Network Management","Computer Games","Electronic / Electrical Engineering")

  #"Principal Learning Creative and Media",

  return(subNames)
}

#get all the MAPPINGS and SUBLEVNOs for a given of subfilter names
loadFilteredMappings <- function(results, subfilter){
  #subfilter<- loadComputingSubjectNames()

  mappings <- results %>% distinct(SUBLEVNO,MAPPING)
  mappings <- left_join(mappings, loadSUBLEVNO(), by=c("SUBLEVNO" = "Qual_Number"))
  mappings <- left_join(mappings, loadDiscMappings())
  mappings <- mappings %>% filter(MAPPING_DESCRIPTION %in% subfilter)

  return(mappings)
}

loadPostcodes <- function(dir=getwd()){
  #postcode data here: https://www.ordnancesurvey.co.uk/opendatadownload/products.html
  # Combine postcodes into one file: http://webpierat.com/2011/05/23/merging-csv-files-using-the-command-line/

  #alternatively, postcodes from here: https://www.doogal.co.uk/PostcodeDownloads.php with regions
  #read postcode data and clean to extract region
  #temp <- read.csv(paste0(folder, "postcodes/England postcodes.csv"), head=TRUE, sep=",")
  #temp <- temp %>% select(Postcode, Region) %>%
  #  mutate(Postcode = gsub(" ", "", Postcode, fixed = TRUE))
  #write back to file
  #write.csv(temp, file=paste0(folder, "postcodes/Region postcodes.csv"), row.names=FALSE)

  #alternatively postcodes from here: https://ons.maps.arcgis.com/home/item.html?id=495696a86cf244afb5c396d3f9b0adde


  #find distinct values for given areas
  #temp %>% filter(area1 == "E92000001") %>% select(area2) %>% distinct()
  #postcodes %>% filter(area1 == "E92000001") %>% group_by(Region) %>% summarise(n = n())

  #### lONDON area 3:
  #"E09000001","E09000002","E09000003","E09000004","E09000005","E09000006",
  #"E09000007","E09000008","E09000009","E09000010","E09000011","E09000012",
  #"E09000013","E09000014","E09000015","E09000016","E09000017","E09000018",
  #"E09000019","E09000020","E09000021","E09000022","E09000023","E09000024",
  #"E09000025","E09000026","E09000027","E09000028","E09000029","E09000030",
  #"E09000031","E09000032","E09000033"

  if (!exists("postcodes"))
  {
    postcodes <- read_delim(paste(dir, "/data/postcodes/postcodes.csv", sep=""),
                          col_names=TRUE,delim=",")
  }
  return(postcodes)
}

loadMap <- function(resolution, dir=getwd()){
  #resolution <- "LA"
  # load shapefile from
  if (resolution == "LA"){
    # https://geoportal.statistics.gov.uk/Docs/Boundaries/Local_authority_district_(GB)_2014_Boundaries_(Generalised_Clipped).zip
    UK <- readOGR(dsn = paste(dir, "/data/shapefiles/UK lad 2014/LAD_DEC_2014_GB_BGC.shp", sep=""), "LAD_DEC_2014_GB_BGC")
    #get all the LAD14CD where they begin with E to show England only
    England3 <- UK[grep("^E", UK@data$LAD14CD),]
    # simplify the polygons
    England2 <- gSimplify(England3,tol=200, topologyPreserve=FALSE)
    England = SpatialPolygonsDataFrame(England2, data=England3@data)

    England@data$regionID <- England@data$LAD14CD

    UK <-NULL

    #reduce the size of the shapefile
    England <- fortify(England, region='regionID')
    England <- England[order(England$order), ]
    #head(England)

    England2 <-NULL
    England3 <-NULL
  }
  if (resolution == "LEA"){
    # http://geoportal.statistics.gov.uk/datasets/c4a62d87de9f4b6087cf5f1515d5a0c1_0?geometry=-8.141%2C54.005%2C4.933%2C55.897&uiTab=table&orderByFields=ctyua14nm+ASC_
    LEAs <- readOGR(dsn = paste0(dir, "/data/maps/England LEAs/Counties_and_Unitary_Authorities_December_2014_Full_Clipped_Boundaries_in_England_and_Wales.shp"),
                    layer= "Counties_and_Unitary_Authorities_December_2014_Full_Clipped_Boundaries_in_England_and_Wales")
    #get all the LAD14CD where they begin with E to show England only
    England3 <- LEAs[grep("^E", LEAs@data$ctyua14cd),]
    # simplify the polygons
    England2 <- gSimplify(England3,tol=200, topologyPreserve=FALSE)
    England = SpatialPolygonsDataFrame(England2, data=England3@data)

    England@data$regionID <- England@data$ctyua14cd

    LEAs <-NULL

    #reduce the size of the shapefile
    England <- fortify(England, region='regionID')
    England <- England[order(England$order), ]
    #head(England)

    England2 <-NULL
    England3 <-NULL
  }


  if (resolution == "Region"){
    #gives 9 regions for England
    #data from: https://www.sharegeo.ac.uk/handle/10672/50

    Regions <- readOGR(dsn = paste(dir, "/data/maps/England regions/Regions.shp", sep=""), "Regions")

    #simplify the polygons
    SimpleRegions <- gSimplify(Regions, tol=3000, topologyPreserve=TRUE)
    England = SpatialPolygonsDataFrame(SimpleRegions, data=Regions@data)

    #Fortify for use with ggplot
    England <- fortify(England, region='POLYGON_ID')

    England <- England[order(England$order), ]

    #test plot
    #ggplot(England, aes(x=long, y=lat, group=group)) +
    #  geom_polygon(colour='black', fill='white') + theme_bw()


    Regions <-NULL
    SimpleRegions <-NULL
  }
  if (resolution == "Coast"){
    # coast map from: http://www.diva-gis.org/datadown
    # help here: http://gis.stackexchange.com/questions/127959/uk-coastline-shapefile
    #head(UK@polygons)
    #UK <- readOGR(dsn = paste(folder, "shapefiles/UK lad 2014/LAD_DEC_2014_GB_BGC.shp", sep=""), "LAD_DEC_2014_GB_BGC")


    #high water map from OS
    #alternative map available from here: Boundary-Line™ https://www.doogal.co.uk/PostcodeDownloads.php
    #UK <- readOGR(dsn = paste(folder, "shapefiles/UK coast/high_water_polyline.shp", sep=""), "high_water_polyline")

    UK <- readOGR(dsn = paste(dir, "/data/maps/UK coast/GBR_adm0.shp", sep=""), "GBR_adm0")

    # simplify the polygons
    #England <- gSimplify(UK,tol=200, topologyPreserve=FALSE)
    #England = SpatialPolygonsDataFrame(England, data=UK@data)

    #reduce the size of the shapefile
    England <- fortify(UK)
    #England <- England[order(England$order), ]

    #ggplot() +
    #  geom_polygon(data=England, aes(x=long, y=lat), colour='black', fill='white') +
    #  theme_bw()

    #head(England)
  }

  return(England)
}

loadCoastalSchools <- function(overwrite = FALSE, dir=getwd()){

  filename <- paste0(dir, "/data/schools/CoastalSchools.csv")

  # print(folder)
  # print(filename)

  #maximum distance from the coast
  cutoffdistance <- 5500

  #check that coastal school file doesn't exist in: ./Data/
  if(file.exists(filename) & !overwrite){
    schools <- read_delim(filename, col_names=TRUE, delim=",")
  }else{ #if it doesn't exist, then we can VERY slowly make it...

    message("WARNING: This might be slow!")

    if (is.null(schools)){
      #load schools
      #rm(schools)
      schools <- loadSchools(overwrite = TRUE)

      #reduce number of schools to search through
      schools <- schools %>%
        select(Northing, Easting, URN) %>%
        filter(!is.na(Northing), !is.na(Easting))
    }

    #load coastal map
    print(dir)
    map <- loadMap("Coast", dir)
    temp <- map %>% select(long, lat)

    #use this: http://www.alex-singleton.com/R-Tutorial-Materials/7-converting-coordinates.pdf
    #convert long lat of coastal map to easting and northing
    longlatcoor<-SpatialPoints(as.matrix(temp), proj4string= CRS("+proj=longlat"))
    tempp <- paste0("+init=epsg:27700")
    mapcoor<-spTransform(longlatcoor,CRS(tempp))
    mapcoor <- as.data.frame(mapcoor)
    names(mapcoor) <- c("Easting", "Northing")
    
    # work out the nearest costal point from each school 
    Distance <- sapply(1:nrow(schools), function(x)
                          with(schools[x,], min(sqrt((mapcoor$Easting - Easting)^2+
                                                       (mapcoor$Northing - Northing)^2))))
													   
    # merge back into the schools dataframe
    temp <- cbind(schools, Distance) %>% mutate(Coastal = ifelse(Distance > cutoffdistance, FALSE, TRUE))
        
    write.csv(temp, file = paste0(getwd(), "/data/schools/CoastalSchools.csv"), row.names = FALSE)
  }

  return(schools)
}

#if you have alrerady cleaned the data, use this load function to get student results in a sane format
loadTrainingProviders <- function(){
  library(XLConnect)

  #https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/478095/Provider_level_tables_SFR_46_2015_to_2016.xlsx
  xlsname <- paste(getwd(), "/data/trainers/Provider_level_tables_SFR_46_2015_to_2016.xlsx", sep="")
  training_data <- readWorksheet(loadWorkbook(xlsname),sheet="Table 3", startRow=6)
  #delete "total" row
  training_data <- training_data[-c(1), ]
  #head(training_data)

  #get provider info to map from http://learning-provider.data.ac.uk/ <- incomplete Uni only
  #complete: https://www.ukrlp.co.uk/
  xlsname <- paste(folder, "FINAL UKPRN list.xlsx", sep="")
  training_provider <- readWorksheet(loadWorkbook(xlsname),sheet=1)[,c(1,2)] #get UKPRN and Postcode
  #head(training_provider)

  #make sure that matching column is the same datatype
  training_provider[, c(1)] <- sapply(training_provider[, c(1)], as.character)

  #combine training provision info and location info for training providers
  training_data <- left_join(training_data, training_provider)
  head(training_data)
  training_data <- training_data %>% mutate(Postcode = gsub(" ", "", Postcode)) #get rid of spaces in postcodes for matching

  #read postcode file in with easting and northing
  postcodes <- loadPostcodes()

  postcodes <- postcodes %>% mutate(Postcode = gsub(" ", "", Postcode))
  #head(postcodes)

  training_data <- left_join(training_data, postcodes)
  training_data[, c(5)] <- sapply(training_data[, c(5)], as.numeric)

  training_data <- training_data %>% select(UKPRN, Provider.name, Region, Subject6,
                                            Total.number.of.first.year.postgraduates, easting, northing ) %>%
    filter(Subject6 != "Primary")

  training_data <- rename(training_data, Postgrads=Total.number.of.first.year.postgraduates)
  training_data[, c(5)] <- sapply(training_data[, c(5)], as.numeric)

  return(training_data)
}

loadLondonLEAs <- function(){
  london <- c("E09000017","E09000015","E09000003","E09000010","E09000031",
              "E09000026","E09000016","E09000004","E09000006","E09000008",
              "E09000029","E09000021","E09000027","E09000018","E09000009",
              "E09000005","E09000007","E09000019","E09000012","E09000014",
              "E09000031","E09000002","E09000025","E09000011","E09000023",
              "E09000028","E09000022","E09000032","E09000013","E09000020",
              "E09000033","E09000001","E09000030","E09000024")

  return(london)
}

loadBirminghamLEAs <- function(){
  bham <- c("E08000031","E08000027","E08000030","E08000028","E08000025","E08000029")
  return(bham)
}

loadManchLiverLEAs <- function(){
  manliv <- c("E08000011","E08000012","E08000014","E06000006","E06000007",
              "E08000013","E08000015","E08000006","E08000010","E08000001",
              "E08000003","E08000007","E08000008","E08000004","E08000005",
              "E08000002","E08000009")
  return(manliv)
}

loadLeedsLEAs <- function(){
  leeds <- c("E08000033","E08000032","E08000034","E08000035","E08000036",
             "E08000016","E08000019","E08000018","E08000017")

  return(leeds)
}

loadLocalAuthorities <- function(){
  #data from: http://www.ons.gov.uk/ons/guide-method/
  #geography/products/area-classifications/ns-area-classifications/
  #index/corresponding-authorities/local-authorities/corresponding-las.xls
  filename <- paste(getwd(), "/data/schools/Local Authorities.csv",sep="")
  localauthorities <- read_delim(filename, col_names=TRUE, delim=",") #, nrows = 100)

  #TODO: rename double of Cornwall, Isles of Scilly and
  # double City of London, Westminster
  # add missing LA codes and find out why they are missing
  # E07000243, Stevenage
  # E07000241, Welwyn Hatfield
  # E07000240, Northumberland
  # E08000020, Gateshead   {NOT E08000037}
  # E07000242, East Hertfordshire
  # Northumberland is E06000057
  # St Albans is E07000240

  return(localauthorities)
}

loadLocalEducationAuthorities <- function(LEAs=NULL, dir=getwd()){
  if(is.null(LEAs)){
    filename <- paste(dir, "/data/schools/schools.csv",sep="")
    LEAs <- read_delim(filename, col_names=TRUE, delim=",") #, nrows = 100)
  }
  #get all LEAs with open schools. (this picks up the Isles of Scilly)
  LEAs <- LEAs %>% filter(grepl("E", `GSSLACode (name)`)) %>%
    select(URN, EstablishmentNumber, `LA (name)`, `GSSLACode (name)`)
  
  #update Northumberland for 2015 (E06000048) / other code swap (E06000057)
  LEAs$`GSSLACode (name)` <- as.character(LEAs$`GSSLACode (name)`)
  LEAs[LEAs$`LA (name)` == "Northumberland",]$`GSSLACode (name)` <- "E06000057"
  LEAs[LEAs$`LA (name)` == "Gateshead",]$`GSSLACode (name)` <- "E08000037"
  LEAs$`GSSLACode (name)` <- as.factor(LEAs$`GSSLACode (name)`)
  return(LEAs)
}

#return schools with LA and Region codes
loadSchoolRegions <- function(dir=getwd()){
  schools <- loadSchools(dir=dir)
  postcodes <- loadPostcodes(dir=dir)
  
  #get region and LA of each schools
  schoollist <- left_join(
    schools[,c("URN", "Name", "LA..code.",
               "schType", "schGender",
               "selective","Postcode")],
    postcodes[,c("Postcode", "Region", "area3")],
    by = "Postcode") %>% ungroup()
  
  return(schoollist)
}

loadSchoolsSubset <- function(URNS, folder, filename){

  temp <- type_convert(read_csv(paste0(folder, filename, ".csv"), col_types = cols(.default = "c")))
  temp <- temp %>% filter(URN %in% URNS | URN == "TOTAL")
  return(temp)
}

#if you have already cleaned the data, use this load function to get student results in a sane format
loadStudentSpreadResults <- function(year, keystage){
  filename <- paste(getwd(), "/data/spreads/", keystage, year, "spreadResults.csv",sep="")
  results <- read_delim(filename, col_names=TRUE, delim=",") #, nrows = 100)
  return(results)
}
