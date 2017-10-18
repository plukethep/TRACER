
#################
#####GRAPH OUTPUT
#################

# plot given data for given grouping, filtered on items for a range of years
plotYearlyData <- function(data_name, grouping, measure, items, year_range=c(12,13,14,15,16), subject_name){  

  graph_data <- OutputTableByYearlyData(data_name, grouping, measure, items, year_range, subject_name)

  p <- ggplot(graph_data, aes(x=Year, y=as.numeric(get(measure)), group=get(grouping), colour=get(grouping))) + 
    geom_line() + geom_point(stat="identity") +
    scale_y_continuous("Percentage of providers")
    #   geom_text(aes(label=anon,x=grade,y=percentage),
    #           position=position_dodge(width=1), vjust=-0.5) +
    # theme(legend.title=element_blank(),
    #       plot.title = element_text(size=10)) +
    # ggtitle(title) #, " ( n = ", total ,")"))

  return(p)
}

#plot schools or students on a heatmap of England
plotResultsOnHeatMap <- function(map, region_data, resolution="region", regions=NULL, title="Computing schools", type="provider"){
  warning(resolution, type)
  
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

#plot each school on a map
plotResultsOnScatterMap <- function(map, provider_data, regions=NULL,  title="Computing schools"){
  
  # provider_data <- table_GCSE_provider_all

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
  provider_data <- provider_data %>% 
                      mutate(schType = ifelse(regexpr("inde", schType) != -1, 
                                                      "Independent",
                                                      ifelse(regexpr("Selective", selective) != -1, "Grammar school",
                                                                    "State non-selective"))) %>%
                      rename(`Provider Type` = schType,
                             Gender = schGender) %>%
                      select(-selective)

  #if specific regions are being mapped, e.g. to focus on london
  
  # TODO: filter on london schools only
  
  if(!is.null(regions)){
    provider_data <- provider_data %>% filter(Code %in% regions)
    map <- map %>% filter(id %in% regions)
    map <- left_join(map, tempLEAs)
    
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
  q <- ggplot(provider_data, aes(x=Easting, y=Northing)) +
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

#plots results for gender by total students
plotResultsGenderTotal <- function(subSummary,  title="Computing students"){

  #get total number of students taking the subject
  total <- sum(subSummary$total)

  data <- subSummary %>% group_by(GENDER,grade) %>%
    summarise(total = sum(total)) %>%
    mutate(totalGender = sum(total),
           percentage = (total / totalGender)* 100)

  data <- anonymiseGraph(data)

  p <- ggplot(data, aes(x=grade, y=total, fill=GENDER)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    scale_y_continuous("Number") + #,breaks=seq(0, max(d$column3), 2)) +
    scale_x_discrete("Grade") +
    geom_text(aes(label=anon,x=grade,y=total),
                 position=position_dodge(width=1), vjust=-0.5) +
    theme(legend.title=element_blank(),
          plot.title = element_text(size=10)) +
    ggtitle(title)#, " ( n = ", total ,")"))

  return(p)
}

#plots results for gender by percentage
plotResultsGenderPecentage <- function(subSummary,  title="Computing students"){


  #subSummary <- GsubSummary


  #get total number of students taking the subject
  total <- sum(subSummary$total)

  #add percentage values to the table
  data <- subSummary %>% group_by(GENDER,grade) %>%
    summarise(total = sum(total)) %>%
    mutate(totalGender = sum(total),
           percentage = (total / totalGender)* 100)

  data <- anonymiseGraph(data)

  p <- ggplot(data, aes(x=grade, y=percentage, fill=GENDER)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    scale_y_continuous("Percentage") +
    scale_x_discrete("Grade") +
    geom_text(aes(label=anon,x=grade,y=percentage),
              position=position_dodge(width=1), vjust=-0.5) +
    theme(legend.title=element_blank(),
          plot.title = element_text(size=10)) +
    ggtitle(title) #, " ( n = ", total ,")"))

  return(p)
}

#plots results for FSM Pupil premium by total
plotResultsFSMTotal <- function(subSummary,  title="Computing students"){

  #get total number of students taking the subject
  total <- sum(subSummary$total)

  data <- subSummary %>% group_by(EVERFSM_6,grade) %>%
    summarise(total = sum(total)) %>%
    mutate(totalFSM = sum(total),
           percentage = (total / totalFSM)* 100) %>%
    filter(!is.na(EVERFSM_6)) %>%
    rename(`Pupil premium` = EVERFSM_6)

  data <- anonymiseGraph(data)

  p <- ggplot(data, aes(x=grade, y=total, fill=`Pupil premium`)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    scale_y_continuous("Number") +
    scale_x_discrete("Grade") +
    geom_text(aes(label=anon,x=grade,y=total),
              position=position_dodge(width=1), vjust=-0.5) +
    guides(fill = guide_legend(title = "Pupil\npremium", title.position = "top")) +
    theme(plot.title = element_text(size=10)) +
    ggtitle(title)

    #theme(legend.title=element_blank())
    #ggtitle(paste0(title, " ( n = ", total ,")"))

  return(p)
}

#plots results for FSM Pupil premium by total
plotResultsFSMPercentage <- function(subSummary,  title="Computing students"){

  #get total number of students taking the subject
  total <- sum(subSummary$total)

  # include dropping the NAs for FSM
  data <- subSummary %>% group_by(EVERFSM_6,grade) %>%
    filter(!is.na(EVERFSM_6)) %>%
    summarise(total = sum(total)) %>%
    mutate(totalFSM = sum(total),
           percentage = (total / totalFSM)* 100) %>%
    filter(!is.na(EVERFSM_6)) %>%
    rename(`Pupil premium` = EVERFSM_6)

  data <- anonymiseGraph(data)

  p <- ggplot(data, aes(x=grade, y=percentage, fill=`Pupil premium`)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    scale_y_continuous("Percentage") +
    scale_x_discrete("Grade") +
    geom_text(aes(label=anon,x=grade,y=percentage),
              position=position_dodge(width=1), vjust=-0.5) +
    guides(fill = guide_legend(title = "Pupil\npremium", title.position = "top")) +
    theme(plot.title = element_text(size=10)) +
    ggtitle(title)
    #theme(legend.title=element_blank())
    #ggtitle(paste0(title, " ( n = ", total ,")"))

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
plotEntriesCohortSize <- function(data, title = "Computing", line=20){

  #data <- cohortdata
  maxX <- data %>% filter(subject == "X2610") %$% max(size)

  Comyline <- data %>% filter(subject == "X2610", size == line) %$% per
  ICTyline <- data %>% filter(subject == "X2650", size == line) %$% per
  Phyyline <- data %>% filter(subject == "X1210", size == line) %$% per

  output <- ggplot(data, aes(x=size, y=per, colour=name)) +
    geom_line(stat = "identity") + geom_point() +
    scale_y_continuous("Cumulative percentage of providers", limits=c(0,100)) +
    scale_x_continuous("Subject cohort size of provider", limits=c(0,maxX)) +
    geom_vline(aes(xintercept = line), linetype="dashed") +
    geom_text(aes( 0, line, label = paste0("cohort size = ",round(line,1)),
                   vjust=4, angle=90), colour = "grey30", size = 2.5) +
    geom_hline(aes(yintercept = Comyline)) +
    geom_text(aes( 0, Comyline, label = paste0(round(Comyline,1),"%"),  vjust = -0.5), colour = "grey30",size = 2.5) +
    geom_hline(aes(yintercept = ICTyline)) +
    geom_text(aes( 0, ICTyline, label = paste0(round(ICTyline,1),"%"), vjust = -0.5), colour = "grey30", size = 2.5) +
    geom_hline(aes(yintercept = Phyyline)) +
    geom_text(aes( 0, Phyyline, label = paste0(round(Phyyline,1),"%"), vjust = -0.5), colour = "grey30", size = 2.5)

  return(output)
}

plotBoxEntriesCohortSize <- function(data, title = "Computing", line=6){

  #order by median size
  data <- data %>% ungroup() %>%
    group_by(subject) %>%
    mutate(mdn = median(as.numeric(size))) %>%
    ungroup() %>%
    arrange(mdn, SubjectName)

  data$SubjectName <- as.vector(data$SubjectName) #get rid of factors
  data$SubjectName = factor(data$SubjectName,data$SubjectName)

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

  data <- output

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
  # SubSummary<- OutputSubjectGroupCombinations(Gresults15, "X2610")
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
                                            type="IDACI",
                                            title="Computing IDACI comparison",
                                            key="Pupil\npremium",
                                            label="Average IDACI intake"){

  #subTotals <- GsubTotals

  if(type=="IDACI"){
    decimals <- 3
  }else{
    decimals <- 1
  }

  #subTotals <- temp
  #order dataframe
  subTotals <- subTotals %>% arrange(mean)
  subTotals$SubjectName <- as.vector(subTotals$SubjectName)
  subTotals$SubjectName <- factor(subTotals$SubjectName,subTotals$SubjectName)

  p <- ggplot(data=subTotals, aes(x=SubjectName, y=mean)) +
    geom_bar(position="dodge", stat="identity", colour="black",
             fill=ifelse(subTotals$SubjectName=="ALL", "black","grey53")) +
    scale_y_continuous(label) +
    scale_x_discrete(NULL) + #get rid of X axis label
    geom_text(aes(label=format(round(mean,decimals), nsmall=decimals),
                  angle = 90, hjust = 1.2), vjust=0.35, colour="white") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.005)) +
    geom_bar(data=subTotals, aes(x=SubjectName, y=Highlight),
             position="dodge", stat="identity", fill="royalblue4") +
    geom_text(aes(label=format(round(mean,decimals), nsmall=decimals),
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


  #TODO: order this!
  subTotals <- subTotals %>% arrange(Average)
  subTotals$SubjectName <- as.vector(subTotals$SubjectName)
  subTotals$SubjectName <- factor(subTotals$SubjectName,subTotals$SubjectName)

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
                                  "X2610",
                                  "EthMaj",
                                  "Non pupil premium C and above",
                                  "Pupil\npremium",
                                  paste0("% Ethnically " , EthName, " students"))
  return(p)
}
