
                      # DATA SPLITING  & CLEANING #

# To run for each ecoregion after 1_Data_Extraction

library(auk) ;library(dplyr) ; library(sf) ;
library(rgdal); library(ggplot2) ; library(suncalc) ; 
library(lubridate) ;library(lutz)

# Crops a geographical area (bbox) for lists and records

# Removes lists with more than 2 observers
# Retains data for the summer and winter periods
# Removes night-time lists
# Retains only lists from the geographical area within the ecoregion

# Retrieves the observations associated with the lists of ecoregions saved
# Removes nocturnal and marine taxa

## SPLIT REGIONS ----

# wd <- "C:/AMonchy/scripts/eBird_Project"
# setwd(wd)
auk_set_ebd_path("data", overwrite=T)
auk_get_ebd_path()

#import filtered sampling from America
sampling <- auk_sampling(file = "data/ebd_checklistsLIGHT_america.txt", sep = "\t")
ebd <- auk_ebd(file = "D:/ebird_Data/ebd_america.txt", sep = "\t") #split continent in 2 hemispheres ?

# import ecoregions shapefile
ecoreg <- st_read(dsn = "../../SIG/Terrestrial Ecoregions of the World_WWF", layer ="wwf_terr_ecos", quiet=F)
# select REALM
neotrop <- filter(ecoreg,REALM=="NT") #180 ecoregions + "Rock and Ice"
neoarc <- filter(ecoreg,REALM=="NA") #118 ecoregions
# choose the ecoregion to focus on
ecocode <- "NA0605"
ecoreg[ecoreg$eco_code==ecocode,]

## To use it in automatic over a cluster  :
# eco <- read.table(file="data_in/ecoNT.txt", sep="\t", header=F)
# for(j in 1:nrow(eco)){ #177
#   ecocode <- eco[j,] # eco[1,]
# }

# select bounding box of the region
boundbox <- st_bbox(ecoreg[ecoreg$eco_code==ecocode,]) # neotrop[neotrop$eco_code==ecocode,]

# filter data to keep only the region selected
region_filter <- sampling %>% 
  # select lists in the boundary box of the region
  auk_bbox(bbox = boundbox)
region_filter

region_filterEBD <- ebd %>% 
  # select observations in the boundary box of the region
  auk_bbox(bbox = boundbox) %>%
  auk_complete()
region_filterEBD

# Filter sampling
data_dir <- "data"
bbox_sampling <- file.path(data_dir, paste0(ecocode,"_BBOX_sampling.txt"))
bbox_ebd <- file.path(data_dir, paste0(ecocode,"_BBOX_ebd.txt"))

# APPLY FILTERS (only run if the files don't already exist)
if (!file.exists(region_sampling)) {
  auk_filter(region_filter, file = bbox_sampling) #, filter_sampling=T, , overwrite = T)
  auk_filter(region_filterEBD, file = bbox_ebd)
}


## FILTERING & CLEANING SAMPLING DATA ----

## To run on a cluster and get a REPORT
# report <- read.table(file="report.txt",sep="\t", header=T)
# report[nrow(report)+1,"eco"] <- ecocode
# report$zone_area[report$eco==ecocode] <- unique(ecoreg$area_km2[ecoreg$eco_code==ecocode])
# report$name[report$eco==ecocode] <- unique(ecoreg$ECO_NAME[ecoreg$eco_code==ecocode])


NT0704_sampling <- read_sampling(x = bbox_sampling, sep = "\t", unique=T) #111 425
# report$list_init[report$eco==ecocode] <- length(NT0704_sampling$checklist_id) #REPORT

# FILTER according to the number of observers #
NT0704_sampling <- filter(NT0704_sampling,number_observers <= 2) # 91 507


# FILTER according to the period
# create period information
NT0704_sampling$month <- as.integer(format(NT0704_sampling$observation_date, "%m"))
NT0704_sampling$day <- as.integer(format(NT0704_sampling$observation_date, "%d"))
#filter between 15/05 - 31/08 & 15/11 & 28/02
NT0704_sampling <- NT0704_sampling %>% #11 284
  filter((month == c(1,2,6,7,8,12)) | (month == c(5,11) & day >= 15)) # 11 284
# NT0704_sampling <- NT0704_sampling %>% #11 284
#   filter((month %in% c(1,2,6,7,8,12)) | (month %in% c(5,11) & day >= 15))
#check !
min(NT0704_sampling$observation_date[NT0704_sampling$observation_date > "2021-08-31"])
min(NT0704_sampling$observation_date[NT0704_sampling$observation_date > "2020-02-29"])

# report$list_periodf[report$eco==ecocode] <- length(NT0704_sampling$checklist_id) #REPORT

# FILTER according to observation started time
# Density before filtering
# hist(hour(hms(NT0704_sampling$time_observations_started)), xlab="Hours", main="Time of the day distribution for observations")

# Prepare formated data for getSunlight function
data <- data.frame(list_id = NT0704_sampling$checklist_id, date = NT0704_sampling$observation_date, start = NT0704_sampling$time_observations_started, 
                   lat=NT0704_sampling$latitude, lon = NT0704_sampling$longitude, period=NA,
                   tz = tz_lookup_coords(lat = NT0704_sampling$latitude, lon = NT0704_sampling$longitude, method="accurate"))
# data$tz[data$tz=="America/Argentina/Rio_Gallegos; America/Punta_Arenas"] <- "America/Argentina/Rio_Gallegos"

# check if each list is in the good timespan (2h before sunrise and after sunset)
for(i in 1 : length(NT0704_sampling$checklist_id)){
  sun <- getSunlightTimes(date=data$date[i],lat=data$lat[i],lon=data$lon[i], keep = c("sunrise", "sunset"),tz = data$tz[i])
  data$period[i] <- (hms::as_hms(data$start[i]) >= hms::as_hms(sun$sunrise-hours(2))) & (hms::as_hms(data$start[i]) <= hms::as_hms(sun$sunset+hours(2)))
} #3 min for more than 11000 lists
# Select only daylist
NT0704_sampling <- NT0704_sampling %>%
  filter(checklist_id %in% data$list_id[data$period==T]) # 11 176
# Density after filtering
# hist(hour(hms(NT0704_sampling$time_observations_started)), xlab="Hours", main="Time of the day distribution for observations")

# sf_use_s2(F)
# FILTER according ecoregion area
# transform coord in geometry POINTS
sampling_in_sf <- st_as_sf(NT0704_sampling %>% select(checklist_id, longitude, latitude), 
                           coords = c("longitude", "latitude"), crs=st_crs(ecoreg))
# sampling_in_sf <- st_as_sf(NT0704_sampling[,c("checklist_id", "longitude", "latitude")],
                           # coords = c("longitude", "latitude"), crs=st_crs(ecoreg))
# crop shapefile to save only the ecoregion
NT0704_sf <- ecoreg[ecoreg$eco_code==ecocode,] #neotrop[neotrop$eco_code == ecocode,]
sampling_in_sf <- st_join(sampling_in_sf,NT0704_sf, join = st_within)
# keep only list inside the ecoregion
NT0704_sampling <- NT0704_sampling %>%
  filter(checklist_id %in% sampling_in_sf$checklist_id[sampling_in_sf$eco_code == ecocode]) # 2 110 

# report$list_regionf[report$eco==ecocode] <- length(NT0704_sampling$checklist_id) #REPORT

# # PLOT points for lists
# ggplot() +
#   geom_sf(data = ecoreg[ecoreg$eco_code==ecocode,], fill="#dbf707", color=NA, show.legend=F) + #factor(eco_code)
#   geom_sf(data= sampling_in_sf)+
#   ggtitle("Eastern Canadian Forests (NA0605)")


# FILTER according observers' experience
# import observers with at least 10 lists
observers <- read.table(file = "data/validObservers_america.txt", sep = "\t")

# check for each saved list of the eco region if one of the observer is valid
l <- character()
for(i in 1 : length(NT0704_sampling$observer_id)){
  if(is.na(NT0704_sampling$group_identifier[i]) & (NT0704_sampling$observer_id[i] %in% observers$obs_id)){
    l <- c(l,NT0704_sampling$checklist_id[i])
  }
  if(!is.na(NT0704_sampling$group_identifier[i]) & (any(unlist(strsplit(NT0704_sampling$observer_id[i], split=",")) %in% observers$obs_id))){
    l <- c(l,NT0704_sampling$checklist_id[i])
  }
}
l <- unique(l[is.na(l)==F])
# keep only list with at least one good observer
validList <- subset(NT0704_sampling, checklist_id %in% l) # 2 054


# Clean sampling Data
validList <- validList %>%
  select(checklist_id, 
         country_code, locality_id, latitude, longitude,
         observation_date,observer_id, time_observations_started,
         protocol_type, sampling_event_identifier, group_identifier,
         duration_minutes, effort_distance_km, number_observers)
# validList <- subset(validList, select= c(checklist_id, 
#          country_code, locality_id, latitude, longitude,
#          observation_date,observer_id, time_observations_started,
#          protocol_type, sampling_event_identifier, group_identifier,
#          duration_minutes, effort_distance_km, number_observers))

# Save sampling data for the ecoregion
write.table(validList, file=file.path(data_dir, paste0(ecocode, "_sampling.txt")), sep="\t")


## EXTRACTING & CLEANING OBSERVATION DATA ----

# Import observations of the boundbox
NT0704_ebd <- read_ebd(x = bbox_ebd, sep = "\t")

# report$obs_init[report$eco==ecocode] <- length(NT0704_ebd$checklist_id) #REPORT

#Pour les ENORMES ?cor?gions !
for(j in 1:length(ymax)){
  for(i in 1:(ymax[j]-ymin[j])){
    NA0403_ebd <- read_ebd(x = file.path(data_dir, paste0(ecocode,"_",j,i,"_ebd.txt")), sep = "\t")
    NA0403_ebd <-  NA0403_ebd %>%
      filter(checklist_id %in% NA0403_sampling$checklist_id)
  }
}
NA0402_SW1_ebd <- read_ebd(x = file.path(data_dir, paste0(ecocode,"_SW1_ebd.txt")), sep = "\t")
NA0402_SW2_ebd <- read_ebd(x = file.path(data_dir, paste0(ecocode,"_SW2_ebd.txt")), sep = "\t")
NA0402_MID1_ebd <- read_ebd(x = file.path(data_dir, paste0(ecocode,"_MID1_ebd.txt")), sep = "\t")
NA0402_MID2_ebd <- read_ebd(x = file.path(data_dir, paste0(ecocode,"_MID2_ebd.txt")), sep = "\t")
NA0402_MID3_ebd <- read_ebd(x = file.path(data_dir, paste0(ecocode,"_MID3_ebd.txt")), sep = "\t")
NA0402_MID4_ebd <- read_ebd(x = file.path(data_dir, paste0(ecocode,"_MID4_ebd.txt")), sep = "\t")
NA0402_MID5_ebd <- read_ebd(x = file.path(data_dir, paste0(ecocode,"_MID5_ebd.txt")), sep = "\t")
NA0402_NE_ebd <- read_ebd(x = file.path(data_dir, paste0(ecocode,"_NE_ebd.txt")), sep = "\t")

# Filter data to keep ones from saved lists
NT0704_ebd <-  NT0704_ebd %>%
  filter(checklist_id %in% l) # 52 824

# report$obs_listf[report$eco==ecocode] <- length(NT0704_ebd$checklist_id) #REPORT

#Pour les ENORMES ?cor?gions !
NA0402_sampling <- read.table(file = file.path(data_dir, paste0(ecocode,"_sampling.txt")), sep = "\t", header=T) #111 425
NA0402_SW1_ebd <-  NA0402_SW1_ebd %>%
  filter(checklist_id %in% NA0402_sampling$checklist_id)
NA0402_SW2_ebd <-  NA0402_SW2_ebd %>%
  filter(checklist_id %in% NA0402_sampling$checklist_id)
NA0402_MID1_ebd <-  NA0402_MID1_ebd %>%
  filter(checklist_id %in% NA0402_sampling$checklist_id)
NA0402_MID2_ebd <-  NA0402_MID2_ebd %>%
  filter(checklist_id %in% NA0402_sampling$checklist_id)
NA0402_MID3_ebd <-  NA0402_MID3_ebd %>%
  filter(checklist_id %in% NA0402_sampling$checklist_id)
NA0402_MID4_ebd <-  NA0402_MID4_ebd %>%
  filter(checklist_id %in% NA0402_sampling$checklist_id)
NA0402_MID5_ebd <-  NA0402_MID5_ebd %>%
  filter(checklist_id %in% NA0402_sampling$checklist_id)
NA0402_NE_ebd <-  NA0402_NE_ebd %>%
  filter(checklist_id %in% NA0402_sampling$checklist_id)

  
# Remove nocturnal and marine species #3842 species
# import list of accepted taxa and filter observations
acceptedBirds <- read.table(file = "data/acceptedTaxa.txt", sep = "\t", header=F)
NT0704_ebd <- subset(NT0704_ebd, scientific_name %in% acceptedBirds[,1]) #48 131

# report$obs_final[report$eco==ecocode] <- length(NT0704_ebd$checklist_id) #REPORT

#Pour les ENORMES ?cor?gions !
# NA0504_NE_ebd <- subset(NA0504_NE_ebd, scientific_name %in% acceptedBirds[,1])
# NA0504_MID_ebd <- subset(NA0504_MID_ebd, scientific_name %in% acceptedBirds[,1])
# NA0504_SW_ebd <- subset(NA0504_SW_ebd, scientific_name %in% acceptedBirds[,1])
NA0402_SW1_ebd <- subset(NA0402_SW1_ebd, scientific_name %in% acceptedBirds[,1])
NA0402_SW2_ebd <- subset(NA0402_SW2_ebd, scientific_name %in% acceptedBirds[,1])
NA0402_MID1_ebd <- subset(NA0402_MID1_ebd, scientific_name %in% acceptedBirds[,1])
NA0402_MID2_ebd <- subset(NA0402_MID2_ebd, scientific_name %in% acceptedBirds[,1])
NA0402_MID3_ebd <- subset(NA0402_MID3_ebd, scientific_name %in% acceptedBirds[,1])
NA0402_MID4_ebd <- subset(NA0402_MID4_ebd, scientific_name %in% acceptedBirds[,1])
NA0402_MID5_ebd <- subset(NA0402_MID5_ebd, scientific_name %in% acceptedBirds[,1])
NA0402_NE_ebd <- subset(NA0402_NE_ebd, scientific_name %in% acceptedBirds[,1])

# Clean observations data
validData <- NT0704_ebd %>%
  select(checklist_id, global_unique_identifier,
         taxonomic_order, scientific_name,observation_count,
         country_code, locality_id, latitude, longitude,
         observation_date,time_observations_started, protocol_type,
         observer_id, sampling_event_identifier, group_identifier,
         duration_minutes, effort_distance_km,
         number_observers)

# save observations data
write.table(validData, file=file.path(data_dir, paste0(ecocode, "_ebd.txt")), sep="\t")


### EDIT REPORT ###
write.table(report[report$eco==ecocode,], file="report.txt", sep="\t", quote=F,row.names = F, col.names = F, append=T,eol="\r")

### Remove BBOX files ### (pour la version automatique sur cluster)
file.remove(file.path(data_dir, paste0(ecocode,"_BBOX_sampling.txt")))
file.remove(file.path(data_dir, paste0(ecocode,"_BBOX_ebd.txt")))





# Special move to split huge ecoregions
# import ecoregions shapefile
ecoreg <- st_read(dsn = "../../SIG/Terrestrial Ecoregions of the World_WWF", layer ="wwf_terr_ecos", quiet=F)

# choose the ecoregion to focus on
ecocode <- "NA0403"
ecoreg[ecoreg$eco_code==ecocode,"geometry"]

data_dir <- "data"
bbox_ebd <- file.path(data_dir, paste0(ecocode,"_BBOX_ebd.txt"))
ebd <- auk_ebd(file = bbox_ebd, sep = "\t")

## POUR NA0504
ggplot() + 
  geom_sf(data = ecoreg[ecoreg$eco_code==ecocode,], aes(fill=factor(OBJECTID)), show.legend=F)+ # , size = 3, color = "black", fill = "cyan1") +
  scale_fill_manual(values = rainbow(12))+
  ggtitle("Appalachian mixed mesophytic forests - NA0402") + 
  scale_x_continuous(breaks=seq(-87,-74,1))+
  scale_y_continuous(breaks=seq(33,42,1))+
  coord_sf()

# select bounding box of the region
xmin <- raster::extent(ecoreg[ecoreg$eco_code==ecocode,])[1]
xmax <- raster::extent(ecoreg[ecoreg$eco_code==ecocode,])[2]
ymin <- raster::extent(ecoreg[ecoreg$eco_code==ecocode,])[3]
ymax <- raster::extent(ecoreg[ecoreg$eco_code==ecocode,])[4]

ymin <- c(33,33,34,34,34,35,36,37,37,38,40,40,40)
ymax <- c(35,36,37,37,38,38,39,40,41,42,42,42,42)
j <- 0
i <- 0
for(x in floor(xmin):(ceiling(xmax)-1)){
  j <- j+1
  i <- 1
  for(y in ymin[j]:(ymax[j]-1)){
    bbox <- st_bbox(c(xmin=x, xmax=(x+1), ymax=(y+1), ymin=y))
    print(bbox)
    region_filterEBD <- ebd %>% 
      auk_bbox(bbox = bbox) %>%
      auk_complete()
    partial_ebd <- file.path(data_dir, paste0(ecocode,"_", j,i, "_ebd.txt"))
    print(paste0(j,i))
    auk_filter(region_filterEBD, file = partial_ebd)
    i <- i+1
  }
}

# bboxSW1 <- st_bbox(c(xmin=-87, xmax=-74, ymax=33, ymin=34))
# bboxSW2 <- st_bbox(c(xmin=-88, xmax=-84, ymax=36, ymin=33.9))
# bboxMID1 <- st_bbox(c(xmin=-88, xmax=-78, ymax=37, ymin=35.9))
# bboxMID2 <- st_bbox(c(xmin=-88, xmax=-78, ymax=38, ymin=36.9))
# bboxMID3 <- st_bbox(c(xmin=-88, xmax=-78, ymax=39, ymin=37.9))
# bboxMID4 <- st_bbox(c(xmin=-88, xmax=-78, ymax=40, ymin=38.9))
# bboxMID5 <- st_bbox(c(xmin=-88, xmax=-78, ymax=41, ymin=39.9))
# bboxNE <- st_bbox(c(xmin=-84, xmax=-77, ymax=42, ymin=40.9))

# Filter sampling
data_dir <- "data"
bbox_ebd <- file.path(data_dir, paste0(ecocode,"_BBOX_ebd.txt"))

ebd <- auk_ebd(file = bbox_ebd, sep = "\t")

region_filterEBD <- ebd %>% 
  # select observations in the boundary box of the region
  auk_bbox(bbox = bboxNE) %>%
  auk_complete()
region_filterEBD

data_dir <- "data"
partial_ebd <- file.path(data_dir, paste0(ecocode,"_NE_ebd.txt"))

# APPLY FILTERS (only run if the files don't already exist)
if (!file.exists(region_sampling)) {
  auk_filter(region_filterEBD, file = partial_ebd)
}
