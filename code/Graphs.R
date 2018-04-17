
#################
#####GRAPH OUTPUT
#################


plotYearlyData <- function(data_name, grouping, measure, items, year_range=c(14,15,16,17), subject_name){
  # data_name <- "table_GCSE_provider_type"
  # measure <- "Providers %"
  # items <- c("Academy converter", "Community school", "Academy sponsor led", "Foundation school", "Voluntary aided school", "Other independent school", "Voluntary controlled school")
  # grouping <- "Type"
  # subject_name <- "computer science"


  graph_data <- OutputTableByYearlyData(data_name, grouping, measure, items, year_range, subject_name)

  p <- ggplot(graph_data, aes(x=Year, y=as.numeric(get(measure)), group=get(grouping), colour=get(grouping))) +
    geom_line() + geom_point(stat="identity") +
    scale_y_continuous(measure) +
    expand_limits(y=0) +
	  labs(colour="Type")
	# ADDED 2018-02-22
    #   geom_text(aes(label=anon,x=grade,y=percentage),
    #           position=position_dodge(width=1), vjust=-0.5) +
    # theme(legend.title=element_blank(),
    #       plot.title = element_text(size=10)) +
    # ggtitle(title) #, " ( n = ", total ,")"))

  return(p)
}


#plot schools or students on a heatmap of England
plotResultsOnHeatMap <- function(map, region_data, resolution="region", regions=NULL, title="Computing schools", type="provider"){
  warning(resolution, " - ", type)

  # map <- LAmap
  # region_data <- get(paste0("table_",course,"_provider_lea")) %>% rename(Region = Type)
  # resolution <- "lea"
  # regions <- NULL
  # title <- paste(subject_name, focus, "% - local education authorities"),
  # type <- focus


  #message(0, nrow(region_data))
   # map <- LAmap
   # map <- Regionmap
   # region_data <- table_GCSE_provider_lea %>% rename(Region = Type)

  #merge the region/LEA IDs to the region_data
  if(resolution == "region"){
    region_data <- getRegionCodes(region_data)
  }
  if(resolution == "lea"){
    region_data <- getLocalEducationAuthorities(region_data, dir=basefolder)
  }

  #message("1", nrow(region_data))

  #if specific regions are being mapped, e.g. to focus on london
  if(!is.null(regions)){
    region_data <- region_data %>% filter(Code %in% regions)
    map <- map %>% filter(id %in% regions)
  }

  #message("2", nrow(region_data))

  region_data <- region_data[region_data[,c(1)] != "Totals",]
  region_data <- region_data %>% filter(!is.na(Region))

  #merge LA data into the map
  temp_map <- left_join(map %>% mutate(id = as.character(id)),
                        region_data %>% mutate(Code = as.character(Code)),
                        by=c("id"="Code"))
  #temp_map <- merge(map, region_data %>% select(Code, `Subject Students`, `Subject Providers`, `Providers %`, `Students %`), by.x="id", by.y="Code", all.x = TRUE)

  # message("3", nrow(temp_map))

  #check that all regions have a value to plot
  temp <- distinct(temp_map, id, `Subject Students`)

  if(nrow(temp[is.na(temp$`Subject Students`),]) > 0)
  {
    warning(paste(" ERROR map data, missing figures for ",
                  temp[is.na(temp$`Subject Students`),]$id))
  }

  # message("4", nrow(temp_map))

  #check that there is data for each LA. The map matches the data
  mapIds <- unique(temp_map$id)
  subIds <- unique(region_data$Code)

  if(length(subIds) != length(mapIds))
  {
    warning(paste("ERROR in LA ID match:", length(subIds) , " vs ",
                  length(mapIds), " = {", setdiff(mapIds,subIds), "}"))
  }

  # message("5", nrow(temp_map))

  #arrange the pieces and order so regions draw correctly
  temp_map <- temp_map %>% arrange(piece, order)

  ###### TODO
  ###ANONYMISE RESULTS WITH LESS THAN 6 per region?
  ######

  # check that numbers match those expected:
  # temp <- temp_map %>% group_by(id) %>% distinct(`Subject\nStudents`) %>% select(`Subject\nStudents`)
  # sum(temp$`Subject\nStudents`, na.rm = TRUE)

  #deal with the holes in the LEA maps, print areas with holes first
  # the craziness explained here:
  #http://stackoverflow.com/questions/21748852/choropleth-map-in-ggplot-with-polygons-that-have-holes
  if(type == "provider"){
    total <- sum(region_data$`Subject Providers`, na.rm=TRUE)
    p <- ggplot(temp_map, aes(x=long, y=lat, group=group)) +
      geom_polygon(data=temp_map[temp_map$id %in%
                                   temp_map[temp_map$hole,]$id,],
                   aes(fill=`Providers %`))+
      geom_polygon(data=temp_map[!temp_map$id %in%
                                   temp_map[temp_map$hole,]$id,],
                   aes(fill=`Providers %`))
  }else if(type=="student"){

    # message("6", nrow(temp_map))

    # TODO: deal with any X data
    temp_map <- temp_map %>% mutate(`Total Students` = as.numeric(`Total Students`),
                                    `Students %` = ifelse(`Students %` == "X",
                                                          ifelse(6 / `Total Students` > 0.05, 5, 100*(6/as.numeric(`Total Students`))),
                                                          as.numeric(`Students %`)),
                                    `Subject Students` = ifelse(`Subject Students` == "X", 6, as.numeric(`Students %`)))

    # message("7", nrow(temp_map))

    total <- sum(temp_map$`Subject Students`, na.rm=TRUE)

    p <- ggplot(temp_map, aes(x=long, y=lat, group=group)) +
      geom_polygon(data=temp_map[temp_map$id %in%
                                   temp_map[temp_map$hole,]$id,],
                   aes(fill=`Students %`))+
      geom_polygon(data=temp_map[!temp_map$id %in%
                                   temp_map[temp_map$hole,]$id,],
                   aes(fill=`Students %`))

    # message("8", nrow(temp_map))

  }

  #add the rest of the data to the map
  p <- p + scale_fill_distiller(palette = "Spectral", name="%") +
    theme_bw() +
    ggtitle(title) +
    theme(axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_text(size=10))

  # message("9", nrow(temp_map))

  #move the legend into the map
  p <- p + theme(legend.position = c(0.10, 0.80))

  # message("10", nrow(temp_map))

  return(p)
}

#function for the Royal Society Report
plotResultsOnHeatMapRS <- function(map, region_data, regions=NULL, title="Computing schools", type="school"){

   #  map <- Regionmap
   #  map <- LAmap
   # region_data <- temp LAfemaleStats %>% filter(GENDER == "F")

  #if specific regions are being mapped, e.g. to focus on london
  if(!is.null(regions)){
    region_data <- region_data %>% filter(Code %in% regions)
    map <- map %>% filter(id %in% regions)
  }

  region_data <- region_data[region_data[,c(1)] != "Totals",]
  region_data <- region_data %>% filter(!is.na(Code))

  #merge LA data into the map
  temp_map <- merge(map, region_data, by.x="id", by.y="Code", all.x = TRUE)

  #check that all regions have a value to plot
  temp <- distinct(temp_map, id, subTotalWhatPercentage)
  if(nrow(temp[is.na(temp$id),]) > 0)
  {
    warning(paste("ERROR map data, missing figures for",
                  temp[is.na(temp$`Subject\nStudents`),]$id))
  }

  #check that there is data for each LA. The map matches the data
  mapIds <- unique(temp_map$id)
  subIds <- unique(region_data$Code)

  #setdiff(region_data$Code, map$id)
  #Missing Gateshead -> E08000037 according to school data, E08000020 according to mapping data
  #find out missing data
  #temp <- unique(temp_map %>% select(id,subTotalWhatPercentage, LA..name.))
  #head(temp_map)

  if(length(subIds) != length(mapIds))
  {
    warning(paste("ERROR in LA ID match:", length(subIds) , " vs ",
                  length(mapIds), " = {", setdiff(mapIds,subIds), "}"))
  }

  if(length(setdiff(region_data$Code, map$id)) > 0){
    warning(paste("map has empty region:", setdiff(region_data$Code, map$id)))
  }

  #arrange the pieces and order so regions draw correctly
  temp_map <- temp_map %>% arrange(piece, order)

  ###### TODO
  ###ANONYMISE RESULTS WITH LESS THAN 6 per region?
  ######

  # check that numbers match those expected:
  # temp <- temp_map %>% group_by(id) %>% distinct(`Subject\nStudents`) %>% select(`Subject\nStudents`)
  # sum(temp$`Subject\nStudents`, na.rm = TRUE)

  #deal with the holes in the LEA maps, print areas with holes first
  # the craziness explained here:
  #http://stackoverflow.com/questions/21748852/choropleth-map-in-ggplot-with-polygons-that-have-holes

  total <- sum(region_data$subStudents, na.rm=TRUE)



  p <- ggplot(temp_map, aes(x=long, y=lat, group=group)) +
    geom_polygon(data=temp_map[temp_map$id %in%
                                 temp_map[temp_map$hole,]$id,],
                 aes(fill=subTotalWhatPercentage)) +
    geom_polygon(data=temp_map[!temp_map$id %in%
                                 temp_map[temp_map$hole,]$id,],
                 aes(fill=subTotalWhatPercentage))

  #add the rest of the data to the map
  p <- p + scale_fill_distiller(palette = "Spectral", name="%") +
    theme_bw() +
    theme(axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_text(size=10))

  #move the legend into the map
  p <- p + theme(legend.position = c(0.10, 0.80))

  return(p)
}

#plot each school on a map
plotResultsOnScatterMap <- function(map, provider_data, regions=NULL,  title="Computing schools"){

  # map <- loadMap("LEA")
  # provider_data <- table_GCSE_provider_all
  # regions <- loadLondonLEAs()
  #
  # clean the data
  provider_data <- provider_data[provider_data[,c(1)] != "Totals",]
  provider_data <- provider_data %>% filter(!is.na(Type))

  # sum(as.numeric(provider_data$`Total Students`), na.rm=TRUE)
  # sum(as.numeric(provider_data$`Subject Students`), na.rm=TRUE)

  provider_info <- loadSchools()
  provider_data <- left_join(provider_data, provider_info %>%
                               select(URN, schType, selective, schGender, Easting, Northing) %>% mutate(URN = as.character(URN)),
                             by = c("Type" = "URN"))

  # add selective column
  provider_data <- provider_data %>% convertSchType() %>%
                      rename(`Provider Type` = schType,
                             Gender = schGender) %>%
                      select(-selective) %>%
                      filter(`Subject Students` > 0)

  #if specific regions are being mapped, e.g. to focus on london

  # TODO: filter on london schools only

  if(!is.null(regions)){
    # provider_data <- provider_data %>% filter(Code %in% regions)
    map <- map %>% filter(id %in% regions)
    provider_data <- provider_data %>%
      filter(Easting < max(map$long), Easting > min(map$long),
             Northing < max(map$lat), Northing > min(map$lat))


    q <- theme(legend.title = element_text(colour="black"))
  }else{
    #move the legend into the map
    q <- theme(legend.position = c(0.2, 0.8))
  }

  #TODO: code to check that the school_data values corrrespond to JCQ sizes
  total <- nrow(provider_data)

  #anonymise small schools
  provider_data <- provider_data %>% mutate(`Subject Students` = ifelse(`Subject Students` == "X", 5, as.numeric(`Subject Students`))) %>%
    rename(Cohort = `Subject Students`)

  provider_data <- provider_data %>% filter(Easting != 0)

  #TODO: add area names. add note for anonymised data

  #plotted crosses denote a school that has fewer than 6 students
  q <- ggplot(provider_data, aes(x=Easting , y=Northing)) +
    geom_polygon(data=map, aes(x=long, y=lat, group=group), colour='grey', fill='white') + q

  #plot all schools that have 5 or more students
  q <- q + geom_point(data=provider_data, alpha = 1/3,
               aes(pch=16, size=Cohort, colour=`Provider Type`))

  #deal with key and axis
  q <- q + theme(axis.text.y=element_blank(),
              axis.text.x=element_blank(),
              axis.title.y=element_blank(),
              axis.title.x=element_blank()) +
            scale_shape_identity() + guides(shape=FALSE)



  #plot all schools with fewer than 5 students
  # q <- q + geom_point(data=filter(temp_school, Cohort <= 5),  alpha = 1/3,
  #              aes(pch=schMapping, size=5))

  #plot a red cross to signify the school is anonymised data
  #q <- q + geom_point(data=filter(temp_school,  Cohort <= 5), colour="red", alpha = 1/5,
  #             aes(pch=3, size=7, label=schMapping)) +


    #ggtitle(paste(title, "( n = ", total, ")"))

  return(q)
}

#function for the Royal Society Report
plotResultsOnScatterMapRS <- function(map, school_data, regions=NULL, title="Computing schools"){
  # map<-LAmap
  # school_data <- SchfemaleStats
  # regions <- NULL

  #regions <- loadLeedsLEAs()

  #if specific regions are being mapped, e.g. to focus on london
  if(!is.null(regions)){
    #get list of LEAs with their names
    tempLEAs <- getLocalEducationAuthorities() %>%
      distinct(LA..name., GSSLACode..name.) %>%
      rename(LEAName = LA..name.,
             id = GSSLACode..name.)

    map <- map %>% filter(id %in% regions)
    school_data <- school_data %>% filter(area5 %in% regions)

    #mjoin the LEA names to their IDs
    map <- left_join(map, tempLEAs)

    #move the legend into the map
    q <- theme(legend.title = element_text(colour="black"))

  }else{
    #move the legend into the map
    q <- theme(legend.position = c(0.2, 0.8))
  }

  temp_school <- school_data

  #TODO: code to check that the school_data values corrrespond to JCQ sizes
  total <- nrow(school_data)

  #TODO: anonymise schools
  temp_school$subStudents <- ifelse(school_data$subStudents < 6 & school_data$subStudents > 0,
                                                                      5, school_data$subStudents)

  temp_school$subTotalWhatPercentage <- ifelse(temp_school$subStudents > 0,
                                               100 * (temp_school$subStudents / temp_school$subTotal), 0)

  #TODO: find out why this school has no data location data - 132916
  # school_data %>% arrange(Easting)
  temp_school <- temp_school %>% filter(Easting != 0)

  #TODO: add area names. add note for anonymised data

  #plotted crosses denote a school that has fewer than 6 students
  q <- ggplot() +
    geom_polygon(data=map, aes(x=long, y=lat, group=group), colour='grey', fill='white')

  #plot all schools that have 5 or more students
  q <- q + geom_point(data=temp_school,
                      aes(x=Easting, y=Northing, pch="16",
                          size=subTotal, colour=subTotalWhatPercentage,
                          palette = "Spectral"),
                      alpha = 1/3)

  #deal with key and axis
  q <- q + theme(axis.text.y=element_blank(),
                 axis.text.x=element_blank(),
                 axis.title.y=element_blank(),
                 axis.title.x=element_blank()) +
    scale_shape_identity() + guides(shape=FALSE)

  return(q)
}

#scatterplot of results of GCSE and A-level looking for correlations
drawGtoAScatterPlot <- function(data){
  line <- lm(KS4_POINTS ~ KS5_POINTS, data)
  print(coef(line))

  print(summary(line))

  c <- coef(line)

  p <- ggplot(line, aes(x=KS5_POINTS, y=KS4_POINTS, group=KS5_POINTS)) +
    geom_jitter(shape=1, position=position_jitter(7)) + geom_point() +
    geom_abline(intercept=coef(line)[1],slope = coef(line)[2],
                colour="Blue", size=1) +
    expand_limits(x = 0, y = 0)

  print(p)
  return(line)
}

#get yearly teacher training info and map it
plotTrainingProvider <- function(training_data, subject="Computing"){

  basemap <- loadMap("Region")

  all_providers <- training_data %>% select(UKPRN, Provider.name, easting, northing) %>% distinct(UKPRN, easting, northing)

  map_data <- training_data %>% filter(Subject6 == subject)

  #get rid of punctuation in subject name
  name <- gsub("[^A-Za-z0-9]", "", subject)

  #total students
  mintotal <- sum(map_data$Postgrads, na.rm=TRUE)

  #total providers
  prosub <- nrow(map_data)
  prototal <- nrow(all_providers)

  #remove map data URNs from otherprovider plot:
  other_providers <- all_providers %>% filter(!(UKPRN %in% map_data$UKPRN))

  p <- ggplot() +
    geom_polygon(data=basemap, aes(x=long, y=lat, group=group), colour='black', fill='white') +
    geom_point(data=other_providers, aes(easting, northing), colour="grey", shape=6, alpha = 2/5) +
    geom_point(data=map_data, aes(easting, northing, size = Postgrads), colour='black', alpha = 2/3) +
    geom_point(data=map_data[is.na(map_data$Postgrads),], aes(easting, northing), colour='blue',
               alpha = 1/2,  shape=3) + theme_bw() + scale_size_area() +
                  ggtitle(paste0(subject,
                      " Secondary training providers 2015/16 (n >",mintotal,
                      ", providers = ", prosub, "/", prototal, " )"))

  return(p)

  #ggsave(p, file=paste(folder, "graphics/training/", name, "-2015.svg", sep=""), width = 10, height = 8)
}

#plots results for a given focus by total/percentage students
plotResultsbyFocus <- function(subSummary, focus=c("EVERFSM_6"), what="total", title="Computing students"){

  # subSummary <- table_GCSE_student_demographic_summary

  # filter out the NAs
  cols <- c(focus, "grade")

  # filter out the NAs for focus columns
  subSummary <- subSummary[complete.cases(subSummary %>% select(cols)),]

  # subSummary %>% filter(is.na(EVERFSM_6))

  #get total number of students taking the subject
  total <- sum(subSummary$total)

  data <- subSummary %>% group_by_(.dots=cols) %>%
    summarise(total = sum(total)) %>%
    mutate(totalGender = sum(total),
           percentage = (total / totalGender)* 100)

  data <- anonymiseGraph(data)

  p <- ggplot(data, aes_string(x="grade", y=what, fill=focus[1])) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    scale_y_continuous(what) + #,breaks=seq(0, max(d$column3), 2)) +
    scale_x_discrete("Grade") +
    geom_text(aes_string(label="anon",x="grade",y=what),
                 position=position_dodge(width=1), vjust=-0.5) +
    theme(legend.title=element_blank(),
          plot.title = element_text(size=10)) +
    ggtitle(title)#, " ( n = ", total ,")"))

  return(p)
}

#plots results for FSM Pupil premium by total
plotEthMinPercentageOffset <- function(EthTotals,  title="Computing students"){

  #get total number of students taking the subject
  total <- sum(EthTotals$SubEthTotal)

  data <-

  data <- anonymiseGraph(data)

  p <- ggplot(data, aes(x=grade, y=percentage, fill=EVERFSM_6)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    scale_y_continuous("Percentage") +
    scale_x_discrete("Grade") +
    geom_text(aes(label=anon,x=grade,y=percentage),
              position=position_dodge(width=1), vjust=-0.5) +
    theme(plot.title = element_text(size=10)) +
    ggtitle(paste0(title, " ( n = ", total ,")"))

  return(p)
}

#plot entries for a given subject against the cohort size of each insitution
plotEntriesCohortSizeCluster <- function(data, title = "Computing", level = "GCSE", bar_order){

  output <- ggplot(data , aes(x = Size, y = Entries, fill="red")) +
    geom_bar(stat = "identity") +
    xlab(paste("Clusters of schools\nby",level,"cohort size")) +
    ylab("% of providers offering subject") +
    scale_x_discrete(limits = bar_order) +
    scale_y_continuous(limits = c(0, 100)) +
    ggtitle(title) +
    theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size=10)) +
    labs(fill="") +
    geom_text(aes(label=paste(round(Entries,1),"%")),
              position=position_dodge(width=1),
              vjust=-0.5,size=2.5)

  return(output)
}

#plot entries for a given subject against the cohort size of each insitution
plotEntriesCohortSize <- function(data, title = "Computing", subjects, line=19){

  # data <- cohortdata
  # subjects <- comparison_subjects

  # unique(data$subject)

  maxX <- data %>% filter(subject == quo_name(subjects[[1]])) %$% max(size)

  Comyline <- data %>% filter(subject == quo_name(comparison_subjects[[1]]), size == line) %$% per
  Phyyline <- data %>% filter(subject == quo_name(comparison_subjects[[2]]), size == line) %$% per
  ICTyline <- data %>% filter(subject == quo_name(comparison_subjects[[3]]), size == line) %$% per

  output <- ggplot(data, aes(x=size, y=per, colour=name)) +
    geom_line(stat = "identity") + geom_point() +
    scale_y_continuous("Cumulative percentage of providers", limits=c(0,100)) +
    scale_x_continuous("Subject cohort size of provider", limits=c(0,maxX)) +
    geom_vline(aes(xintercept = line), linetype="dashed") +
    geom_text(aes( 0, line, label = paste0("cohort size = ",round(line,1)),
                   vjust=4, angle=90), colour = "grey30", size = 2.5) +
    geom_hline(aes(yintercept = Comyline)) +
    geom_text(aes( 0, Comyline, label = paste0(round(Comyline,1),"%"), vjust = -0.5), colour = "grey30", size = 2.5) +
    geom_hline(aes(yintercept = Phyyline)) +
    geom_text(aes( 0, Phyyline, label = paste0(round(Phyyline,1),"%"), vjust = -0.5), colour = "grey30", size = 2.5) +
    geom_hline(aes(yintercept = ICTyline)) +
    geom_text(aes( 0, ICTyline, label = paste0(round(ICTyline,1),"%"), vjust = -0.5), colour = "grey30", size = 2.5)


  return(output)
}

plotBoxEntriesCohortSize <- function(data, title = "Computing", line=6){

  # data <- box_data

  #order by median size
  data <- data %>% ungroup() %>%
    group_by(subject) %>%
    mutate(mdn = median(as.numeric(size))) %>%
    ungroup() %>%
    arrange(mdn, SubjectName)

  data$SubjectName <- as.vector(data$SubjectName) #get rid of factors
  data$SubjectName <- factor(data$SubjectName,unique(data$SubjectName))

  output <- ggplot(data, aes(SubjectName, size)) +
    geom_boxplot(outlier.shape = NA) +
    geom_hline(aes(yintercept = line), colour="red", linetype="dashed") +
    scale_x_discrete("Subjects") +
    scale_y_continuous(paste0("Provider subject cohort size\nred line marks cohort size of ",line,""), limits=c(0,max(data$upper))) +
    coord_flip()

  #TODO: - highlight computing

  #coord_cartesian(ylim = quantile(output$size, c(0, 0.9)))
  #scale_y_continuous("Cumulative percentage of providers", limits=c(0,100))
  return(output)
}

#plot entries for a given subject against the number of subjects offered by each insitution
plotEntriesSubjectNumber <- function(data, title = "Computing", level="GCSE", bar_order){

  # data <- output

  output <- ggplot(data , aes(x = Size, y = Entries, fill="red")) +
    geom_bar(stat = "identity") +
    xlab(paste("Clusters of schools\nby",level,"subs offered")) +
    ylab("% of providers offering subject") +
    scale_x_discrete(limits = bar_order) +
    scale_y_continuous(limits = c(0, 100)) +
    ggtitle(title) +
    theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size=10)) +
    labs(fill="") +
    geom_text(aes(label=paste(round(Entries,1),"%")),
                            position=position_dodge(width=1),
              vjust=-0.5,size=2.5)

  #line <- unname(coef(lm(totalQuals ~ perComp, data)))
  #round(line[1], digits=1)

  # output <- ggplot(data, aes(x = totalQuals, y = (perComp * 100))) +
  #   geom_point(shape=1, aes(size = NoComputing) ) +
  #   xlab("Total qualifications offered by providers") +
  #   ylim(-1, 100) +
  #   ylab("Percentage of providers") +
  #   ggtitle(paste(title)) +
  #   theme(legend.position="bottom",
  #         plot.title = element_text(size=10))

  return(output)
}

#a graph showing the subject combinations with a given subject
plotBarSubjectCombo <-function(SubSummary, spreadResults, subject, name, n=15){
  SubSummary<- OutputSubjectGroupCombinations(spreadResults, subject)
  #
  #  spreadResults <- Gresults15
  #  subject <- "X2610"
  #  SubSummary <- compcombo
  #  name <- "Computing"
  maxY <- 1.05 * SubSummary[2,names(SubSummary) == "Percentage"] %$% Percentage

  Subn <- SubSummary[SubSummary$Subject == subject, ]$n

  #get largest subjects
  subjects <- buildSubjectList(spreadResults, subject, n)
  names(subjects) <- c("Subject","gross","Name")

  SubSummary <- SubSummary[c(2:n),]

  SubSummary <- left_join(SubSummary, subjects)

  #add highlight column
  SubSummary <- SubSummary %>% mutate(highlight = ifelse(!is.na(Name), 0, Percentage))

  p <- ggplot(data=SubSummary, aes(x = SubjectName, y = Percentage)) +
      geom_bar(position="dodge", stat="identity", fill="thistle4") +
      geom_bar(data=SubSummary, position="dodge", stat="identity", fill="royalblue3",
               aes(x=SubjectName, y=highlight)) +
      geom_text(aes(label=combonum), vjust = -0.8, size=2.5) +
      scale_y_continuous("Percentage taking both") +
      ylim(0, maxY) +
      scale_x_discrete(NULL, limits=factor(SubSummary$SubjectName)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size=10)) +
      ggtitle(paste0("Subjects combined with ",name," (n=",Subn,")"))


  return(p)
}

#plot a bar graph of the number of subjects per student
plotSubjectsperStudent <- function(summary){

  #summary <- overall

  p <- ggplot(temp, aes(x=totalSubs, y=total)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    geom_text(aes(label=anon,x=totalSubs,y=total),
              position=position_dodge(width=1), vjust=-0.5)

  return(p)
}

# plot graph showing those subjects with an over representation and those with an under representation of something
plotComparisonSubjectAgainstAll <- function(comparison, n_subjects=15, name){
  comparison <- comparison %>% arrange(desc(n)) %>% top_n(n_subjects, wt=n)
  # print(comparison$n)


  # coordinates of upper and lower areas
  trsup <- data.frame(x=c(0,0,100),
                      y=c(0,100,100))
  trinf <- data.frame(x=c(0,100,100),
                      y=c(0,0,100))

  p <- ggplot(comparison, aes(x= all, y = subject)) +
    geom_point(aes(colour=ifelse(all > subject, 'red', 'green'))) +
    guides(colour=FALSE) +
    geom_text_repel(aes(all, subject,
                        label=as.character(MAPPING_DESCRIPTION))) +
  # geom_abline(intercept = 0, slope = 1) +
    geom_segment(aes(x = 0, y = 0, xend = 100, yend = 100)) +
    geom_polygon(aes(x=x, y=y), data = trsup, fill="#00FF0033") +
    geom_polygon(aes(x=x, y=y), data = trinf, fill="#FF000033") +
    coord_cartesian(xlim=c(0,max(comparison$all)),
                    ylim=c(0,max(comparison$subject))) +
    theme(plot.margin = unit(c(0,0,0,0), "cm")) #+  # gets rid of the padding around the plot
    #ggtitle(name)

  return(p)
}


plotEntriesLongitudinal <- function(data){

  p <- ggplot(data=data, aes(x=Year,y=value, group=focus, colour=focus)) + geom_line() +
    geom_point() + expand_limits(y=0)

  return(p)
}

plotEntriesPercentage <- function(overall, what="GENDER", by="F", plot){

  #what <- "EVERFSM_6"
  # by=1
  # overall <- PPoverall

  #select subjects to highlight
  temp <- overall %>%
    filter(comboSub %in% plot) %>%
    select(comboSub, get(what), year, Total, Percentage, MAPPING_DESCRIPTION) %>%
    filter_(.dots = paste(what, "==",
                          ifelse(is.character(by),
                                 paste0("'",by,"'"),
                                 by))) %>%
    mutate(`Computing qualifications` =
             ifelse(comboSub == "310 2610", "GCSE Computing",
                    ifelse(comboSub == "310 2650", "GCSE ICT", comboSub)))  %>%
    select(`Computing qualifications`, year, Total, Percentage)

  p <- ggplot(data = temp,
         aes(x=year,y=Percentage, group=`Computing qualifications`, colour=`Computing qualifications`)) +
    geom_point() +
    geom_line(data = temp %>% filter(`Computing qualifications` != "Population"),
              size = 1,
              aes(linetype="solid", x=year,y=Percentage, group=`Computing qualifications`, colour=`Computing qualifications`)) +
    geom_line(data = temp %>% filter(`Computing qualifications` == "Population"),
              size = 1,
              aes(linetype="dotted", x=year,y=Percentage, group=`Computing qualifications`, colour=`Computing qualifications`)) +
    guides(linetype=FALSE)

  write.csv(temp, paste0(outputfolder, "/Time_Series_",what,".csv"), row.names = FALSE)

  return(p)
}



#########
##GRAPHS COMBINING MULTIPLE SUBJECTS
#########

#plot comparisons of subjects for a given factor e.g. gender/FSM/Idaci
plotSubjectComparisonSingleItem <- function(subTotals,
                                            focus="X2610",
                                            type="FSM",
                                            title="Computing IDACI comparison",
                                            key="Pupil premium",
                                            label="Average IDACI intake"){

  #subTotals <- table_GCSE_student_taking_fsm
  #subTotals <- subject_sizes

  if(type=="IDACI"){
    decimals <- 3
  }else{
    decimals <- 1
  }

  # rename the 'mean' field if present
  if(sum(grepl("mean",names(subTotals))) > 0){
    # print("renaming")
    subTotals <- rename(subTotals, Average = mean)
  }

  #subTotals <- temp
  #order dataframe
  subTotals <- subTotals %>% arrange(Average)
  subTotals$SubjectName <- as.vector(subTotals$SubjectName)
  subTotals$SubjectName <- factor(subTotals$SubjectName, unique(subTotals$SubjectName))

  p <- ggplot(data=subTotals, aes(x=SubjectName, y=Average)) +
    geom_bar(position="dodge", stat="identity", colour="black",
             fill=ifelse(subTotals$SubjectName=="ALL", "black","grey53")) +
    scale_y_continuous(label) +
    scale_x_discrete(NULL) + #get rid of X axis label
    geom_text(aes(label=format(round(Average,decimals), nsmall=decimals),
                  angle = 90, hjust = 1.2), vjust=0.35, colour="white") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.005)) +
    geom_bar(data=subTotals, aes(x=SubjectName, y=Highlight),
             position="dodge", stat="identity", fill="royalblue4") +
    geom_text(aes(label=format(round(Average,decimals), nsmall=decimals),
                  angle = 90, hjust = 1.2), vjust=0.35, colour="white") +
    guides(fill = guide_legend(title = key, title.position = "top"))
    #ggtitle(title)

  #TODO: highlight the ALL column data

  #geom_errorbar(aes(x=SubjectName, ymin=Average-SD, ymax=Average+SD), width=0.25) +
  #the errors bars to too huge to really notice a difference

  return(p)
}

#plot comparisons of subjects for a given factor e.g. gender/FSM/Idaci
plotSubjectComparisonDoubleItem <- function(subTotals,
                                            type="GENDER",
                                            title="Computing IDACI comparison",
                                            key="Pupil\npremium",
                                            label="Avg IDACI score of entry"){

  # subTotals <- temp

  #TODO: order this!
  subTotals <- subTotals %>% arrange(Average)
  subTotals$SubjectName <- as.vector(subTotals$SubjectName)
  subTotals$SubjectName <- factor(subTotals$SubjectName,unique(subTotals$SubjectName))

  p <- ggplot() +
    geom_bar(data=subTotals,
             aes_string(x="SubjectName", y="Average", fill=type),
             position="dodge", stat="identity", colour="white", alpha=0.6) +
    scale_y_continuous(label) +
    scale_x_discrete("Subject") +
    #geom_text(aes(label=round(Average,3), angle = 90, hjust = -0.1), vjust=-0.5) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.005)) +
    geom_bar(data=subTotals, aes_string(x="SubjectName", y="Highlight", fill=type),
             position="dodge", stat="identity", alpha=1, colour="Black") +
    guides(fill = guide_legend(title = key, title.position = "top")) +
    theme(axis.title.x=element_blank())
    #ggtitle(title)

    #TODO: plot the standard deviation bar on graph for IDACI and KS2
    #geom_errorbar(aes(x=SubjectName, ymin=Average-SD, ymax=Average+SD), width=0.25) +
    #the errors bars to too huge to really notice a difference

    return(p)
}

plotSingleSubjectEthnicityDifference <- function(EthBreakdown, title="Computing", first=TRUE, min=NULL, max=NULL){

  #min = -60
  #max = 100
  #EthBreakdown <- EthPhys
  #first = FALSE
  p <-  ggplot(EthBreakdown, aes(x=EthName, y=comparison)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.005),
          axis.title.x=element_blank(),
          plot.title = element_text(size=10)) +
    ggtitle(title)

  #add title if this is the first graph in the series
  if(first){
    p <- p + scale_y_continuous("Difference from expected (%)")
  }else{
    p <- p + theme(axis.title.y=element_blank())
  }

  #add max and min values so all graphs line up
  if(!is.null(max) | is.null(min)){
    p <- p + ylim(c(min, max))
  }

  return(p)
}


plotEthnicityPercentageAllSubs <- function(spreadResults, Ethnicity, EthName, subject){
  #subject <- "X2610"
  #Ethnicity <- "ASIA"
  #spreadResults <- Gresults15

  temp <- spreadResults %>% mutate(EthMaj = ifelse(EthMaj == Ethnicity, 1,
                                                ifelse(!is.na(EthMaj) & EthMaj != "", 0, NA)))

  GsubTotals <- OutputSubjectGroupAverageByWhat(temp, subject, "EthMaj", 30)
  GsubTotals$mean <- 100 * GsubTotals$mean
  GsubTotals$Highlight <- 100 * GsubTotals$Highlight

  p <- plotSubjectComparisonSingleItem(GsubTotals,
                                  subject,
                                  "EthMaj",
                                  "Non pupil premium C and above",
                                  "Pupil\npremium",
                                  paste0("% Ethnically " , EthName, " students"))
  return(p)
}


####OLD#####
#Map GCSE results to A-Level results
drawGCSEtoAlevel <- function()
{
  #get GCSE results
  GCSEresults <- loadResults(12, "KS4")

  #get A-Level results
  #ALresults <- loadResults(13, "KS5")

  #####temp ALevel results from python script
  filename <- paste(folder, "KS5Results_2014.txt",sep="")
  ALresults <- read.csv(filename, head=FALSE, sep=",")
  colnames(ALresults) <- c("KS5_GRADE","QAN","DiscCode Name","KS5_MAPPING",
                           "KS5_PupilMatchingRefAnonymous","KS5_EXAMYEAR","Gender",
                           "qual type AS AA","FSM","KS4_SENPS","KS4_SENA","KS4_IDACI",
                           "EthnicGroupMinor_SPR14","EthnicGroupMajor_SPR14","FSMelible_SPR14",
                           "SENprovision_SPR14","SENprovisionMajor_SPR14","KS3_ENGTALEV",
                           "KS3_MATTALEV","KS3_SCITALEV","KS5_URN")

  #   ALresults$KS5_POINTS[ALresults$KS5_GRADE == "*"] <- 140
  #   ALresults$KS5_POINTS[ALresults$KS5_GRADE == "A"] <- 120
  #   ALresults$KS5_POINTS[ALresults$KS5_GRADE == "B"] <- 100
  #   ALresults$KS5_POINTS[ALresults$KS5_GRADE == "C"] <- 80
  #   ALresults$KS5_POINTS[ALresults$KS5_GRADE == "D"] <- 60
  #   ALresults$KS5_POINTS[ALresults$KS5_GRADE == "E"] <- 40
  #   ALresults$KS5_POINTS[ALresults$KS5_GRADE == "U"] <- 0

  ALresults$KS5_POINTS[ALresults$KS5_GRADE == "*"] <- 70
  ALresults$KS5_POINTS[ALresults$KS5_GRADE == "A"] <- 60
  ALresults$KS5_POINTS[ALresults$KS5_GRADE == "B"] <- 50
  ALresults$KS5_POINTS[ALresults$KS5_GRADE == "C"] <- 40
  ALresults$KS5_POINTS[ALresults$KS5_GRADE == "D"] <- 30
  ALresults$KS5_POINTS[ALresults$KS5_GRADE == "E"] <- 20
  ALresults$KS5_POINTS[ALresults$KS5_GRADE == "U"] <- 0
}



####OLD####
#draw a graph showing other subjects taken with subject d (if specified as vector)
drawBarSubjectCombo <-function(SubSummary, d=NULL)
{
  #determine whether to print one or more subjects
  if(is.null(d)){
    DiscCodes <- unique(SubSummary$MAPPING)
  } else {
    DiscCodes <- d
  }

  #get totals for each other subject taken with subject d by gender
  data <- SubSummary %>%
    select(OrigID, MAPPING, GENDER, total) %>%
    group_by(OrigID, MAPPING, GENDER) %>%
    summarise(gentotal = sum(total)) %>%
    ungroup() %>%
    arrange(OrigID, desc(gentotal))

  #get DiscCode names
  DiscCodes <- loadDiscMappings()

  #map orig sub names to IDs
  data <- left_join(data, DiscCodes, c("OrigID"="MAPPING"))
  colnames(data)[which(names(data) == "MAPPING_DESCRIPTION")] <- "OrigName"

  #map matching sub names to IDs
  data <- left_join(data, DiscCodes)
  colnames(data)[which(names(data) == "MAPPING_DESCRIPTION")] <- "MapName"

  for(d in unique(data$OrigID))
  {
    #d = "1110"
    print(d)
    graph_data <- data %>%
      filter(OrigID != MAPPING, OrigID == d) %>%
      arrange(OrigID, desc(gentotal))

    temp <- graph_data %>%
      group_by(MAPPING) %>%
      summarise(mappingtotal = sum(gentotal))

    graph_data <- merge(graph_data, temp) %>%
      ungroup() %>%
      arrange(desc(mappingtotal), GENDER) %>%
      filter(row_number() <= 20)

    graphdeatils <- data %>%
      filter(OrigID == d,  MAPPING == d) %>%
      summarise(grandtotal = sum(gentotal), name = unique(OrigName), ID = unique(OrigID))

    graphdeatils$name <- gsub("[^A-Za-z0-9]", "", graphdeatils$name)


    head(graph_data)

    graph_data$percentage <- round((graph_data$gentotal / graphdeatils$grandtotal) * 100,1)
    graph_data <- graph_data[order(-graph_data$mappingtotal),]

    p <- ggplot(graph_data,
                aes(x = MapName, y = gentotal, fill=GENDER)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=percentage)) +
      scale_y_continuous("Number taking both") +
      scale_x_discrete("Subjects combined", limits=factor(graph_data$MapName)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            plot.title = element_text(size=10)) +
      ggtitle(paste(graphdeatils$ID, " ", graphdeatils$name, " (n = ", graphdeatils$grandtotal ,") subject combinations", sep=""))

    fname <- paste(folder, "graphics/combos/", year, " ", graphdeatils$ID, " ", graphdeatils$name, ".svg", sep="")

    ggsave(p, file=fname)
  }
}



####OLD####
plotHeatMap <- function(map, name, total, level, year, ID, location)
{
  print(paste(name, ID))

  p <- ggplot(map, aes(x=long, y=lat, group=group, fill=Percentage)) +
    geom_polygon() +
    scale_fill_distiller(palette = "Spectral", name="Totals") +
    theme_bw() +
    ggtitle(paste(level, name, " - ", total))

  ggsave(p, file=paste(folder, "graphics/", name, " ", level, " ", year, ".svg", sep=""), width = 10, height = 10)
}

#print a graph of results
printResults <- function(data, DiscName, discID, year, level, type, bar_order, w)
{
  print(paste(DiscName, discID, year, type, w))
  #print(data)

  p <- ggplot(data, aes(x=grade, y=percentage, fill=type)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    scale_y_continuous("Percentage") +
    scale_x_discrete(limits = bar_order) +
    geom_text(aes(label=count,x=grade),
              position=position_dodge(width=0.9), vjust=-0.5) +
    ggtitle(paste(DiscName, " (", discID ,") subject results", sep=""))
  print(p)

  filename <- paste(folder, "graphics/results/", year, level, DiscName, " ", type, ".svg", sep="")
  #print(filename)
  ggsave(p, file=filename, width=w)
}


