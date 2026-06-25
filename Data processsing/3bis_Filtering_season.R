

# To run with sampling each ecoregion, after 3_Calculation

library(dplyr) ; library(sf) ;library(rgdal);library(ggplot2) ; library(data.table)

# Reduce season to keep only 2 months of sampling according to the hemisphere 
# Correct data in report about urban rate, score, distance and duration


# wd <- "C:/AMonchy/scripts/eBird_Project"
# setwd(wd)

# initialization
ecoreg <- st_read(dsn = "../../SIG/Terrestrial Ecoregions of the World_WWF", layer ="wwf_terr_ecos", quiet=F)
data_dir <- "data_out" # "data/3_Calculation"
lf <- list.files(path = data_dir, pattern="calculation") #season
report <- read.table(file="3_report.txt",sep="\t", header=T)

# report[nrow(report)+1,"eco"] <- ecocode

# # choose the ecoregion to focus on
# ecocode <- "NA0410"
# ecoreg[ecoreg$eco_code==ecocode,]

for(i in 1:length(lf)){
  # import files
  sampling <- read.table(file = file.path(data_dir, lf[i]), sep = "\t", header=T)
  # sampling <- fread(file = file.path(data_dir, lf[i]), select=seq(2,21,1))
  
  ecocode <- unlist(strsplit(lf[i],split = "_"))[1]
  
## SEASONAL FILTERING ----
  # # get the day of the year
  sampling$observation_date <- as.Date(sampling$observation_date)
  sampling$day <- as.numeric(format(sampling$observation_date, "%j"))
  # hist(sampling$observation_date, seq.Date(min(sampling$observation_date), max(sampling$observation_date), "day"))
  
  # get region hemisphere
  centroid <- st_centroid(st_as_sfc(st_bbox(ecoreg$geometry[ecoreg$eco_code==ecocode])))

  a <- nrow(sampling)   #REPORT
  if(st_coordinates(centroid)[2] > 0){
    sampling <- sampling %>%
      filter((day >= 135) & (day <= 197))
    # filter(month == 6 | (month == 5 & day >= 15) | (month == 7 & day <= 15))
  }
  if(st_coordinates(centroid)[2] < 0){
    sampling <- sampling %>%
      filter((day >= 319) | (day <= 15))
    # filter(month == 12 | (month == 11 & day >= 15) | (month == 1 & day <= 15))
  }
  report$kept_sampling_season[report$eco==ecocode] <- round((nrow(sampling) / a), digits=3)   #REPORT
  # hist(sampling$observation_date, seq.Date(min(sampling$observation_date), max(sampling$observation_date), "weeks"))
  

## CORRECTION of report ----
  # report$list_nbr[report$eco==ecocode] <- nrow(sampling)
  report$remaining_duration[report$eco==ecocode] <- nrow(subset(sampling, (duration_minutes >= 15) & (duration_minutes <=60)))
  report$remaining_score[report$eco==ecocode] <- nrow(subset(sampling, score <= quantile(unique(score),0.9) & score >= quantile(unique(score),0.1)))
  report$remaining_distance[report$eco==ecocode] <- nrow(sampling[sampling$effort_distance_km <= 2, ])
  sampling2 <- sampling %>%
    filter((duration_minutes >= 15) & (duration_minutes <=60),
           effort_distance_km <= 2,
           score <= quantile(unique(score),0.9) & score >= quantile(unique(score),0.1))
  urb <- sampling2 %>%
    filter((urb2015_1km >= 50) & (urb2015_1km <= 90) & (urb2015_3km >= 20))
  report$remaining_urb[report$eco==ecocode] <- nrow(urb)
  nonurb <- sampling2 %>%
    filter((urb2015_1km <= 1) & (urb2015_3km <= 5))
  report$remaining_nonurb[report$eco==ecocode] <- nrow(nonurb)

  report$list_filtered[report$eco==ecocode] <- nrow(sampling2)

  #save table
  write.table(sampling, file=file.path(data_dir, paste0(ecocode,"_sampling_season.txt")), sep="\t")
} 


#edit report
write.table(report, file="3_report.txt", sep="\t", quote=F,row.names = F, col.names = T ,eol="\r") #, append=T


