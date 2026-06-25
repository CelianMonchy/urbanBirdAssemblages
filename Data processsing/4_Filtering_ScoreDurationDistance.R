
                         #  FILTERING & CLASSIFICATION  #

# To run with sampling each ecoregion, after 3bis_Calculation

library(dplyr) ;library(ggplot2) ; library(data.table) 

# Reduces the sampling time interval [15min; 60min]
# Reduces the sampling distance to <= 2 km
# Restricts valid observers to the 80th percentile
# Sorts the lists according to urban and non-urban areas
# Keep observations related to saved lists

# wd <- "C:/AMonchy/scripts/eBird_Project"
# setwd(wd)

# get files with lists remaining on 2 months
data_dir1 <- "data/3_Calculation/bis"
lf <- list.files(path = data_dir1, pattern="season")
data_dir2 <- "data/4_Filtering/bis"

#REPORT
report <- read.table(file="4_report.txt",sep="\t", header=T)

# report <- read.table(file="4_report.txt",sep="\t", header=T, skip=236)
# colnames(report) <- colnames

for(i in 1:length(lf)){
  sampling <- read.table(file = file.path(data_dir1, lf[i]), sep = "\t", header=T)
  # sampling <- fread(file = file.path(data_dir, lf[i]), select=seq(2,21,1))
  
  ecocode <- unlist(strsplit(lf[i],split = "_"))[1]
  # report[nrow(report)+1,"eco"] <- ecocode
  # a <- nrow(sampling) #REPORT
  # report$kept_duration[report$eco==ecocode] <- round(nrow(subset(sampling, (duration_minutes >= 15) & (duration_minutes <=60))) / a, digits=3)
  # # report$kept_duration[report$eco==ecocode] <- round(nrow(subset(sampling, (duration_minutes >= 15) & (duration_minutes <=90))) / a, digits=3)
  # report$kept_score[report$eco==ecocode] <- round(nrow(subset(sampling, score <= quantile(unique(score),0.9) & score >= quantile(unique(score),0.1))) / a, digits=3)
  # report$kept_distance[report$eco==ecocode] <- round(nrow(sampling[sampling$effort_distance_km <= 2, ]) / a, digits=3)
  # # report$kept_distance[report$eco==ecocode] <- round(nrow(sampling[sampling$effort_distance_km <= 3, ]) / a, digits=3)
  
  ## FILTERING ---- 
  sampling <- sampling %>%
    filter((duration_minutes >= 15) & (duration_minutes <=60),
    # filter((duration_minutes >= 15) & (duration_minutes <=90),
           effort_distance_km <= 2,
           # effort_distance_km <= 3,
           score <= quantile(unique(score),0.9) & score >= quantile(unique(score),0.1))
  
  # report$kept_filtering[report$eco==ecocode] <- round((nrow(sampling) / a), digits=3)
  # report$nlist_final[report$eco==ecocode] <- nrow(sampling) #REPORT
  
  if(nrow(sampling)==0){
    report$urban[report$eco==ecocode] <- 0
    report$nonurban[report$eco==ecocode] <- 0
  }
  else{
    ## SORTING according to URBAN ----
    sampling$land <- NA
    sampling$land[(sampling$urb2015_1km <= 1) & (sampling$urb2015_3km <= 5)] <- "NonUrban" 
    sampling$land[(sampling$urb2015_1km >= 50) & (sampling$urb2015_1km <= 90) & (sampling$urb2015_3km >= 20)] <- "Urban" 
    sampling$land[is.na(sampling$land)] <- "NotClassified"
    
    sampling$land <- as.factor(sampling$land)
    
    ## list count in each type of landcover
    # table(sampling$land)
    # barplot(table(sampling$land), col=rev(heat.colors(3)), ylab="# checklist", main="Repartition des classes de listes", space=0)
    
    # report$urban[report$eco==ecocode] <- nrow(sampling[sampling$land=="Urban",])
    # report$nonurban[report$eco==ecocode] <- nrow(sampling[sampling$land=="NonUrban",])
    
    #clean columns : get sure there's no group id
    if(all(is.na(sampling$group_identifier))==T & all(sampling$checklist_id == sampling$sampling_event_identifier)){
      sampling <- sampling[,c(1:9,12:22)]
    }

    #save sampling table
    write.table(sampling, file=file.path(data_dir2, paste0(ecocode,"_sampling_final.txt")), sep="\t", row.names = F, quote=F)


    ## GET OBSERVATIONS related to REMAINING LISTS ----
      ebd <- fread(file = file.path("data",paste0(ecocode,"_ebd.txt")), select=seq(2,19,1))
    ebd <- ebd[(ebd$checklist_id %in% sampling$checklist_id)==T,]

    #clean columns : get sure there's no group id
    if(all(is.na(ebd$group_identifier))==T & all(ebd$checklist_id == ebd$sampling_event_identifier)){
      ebd <- ebd[,c(1:13)]
    }

    #save ebd table
    write.table(ebd, file=file.path(data_dir2, paste0(ecocode,"_ebd_final.txt")), sep="\t", row.names = F, quote=F)
  }
}

#edit report
write.table(report, file="4_report.txt", sep="\t", quote=F,row.names = F, col.names = T ,eol="\r") #, append=T
