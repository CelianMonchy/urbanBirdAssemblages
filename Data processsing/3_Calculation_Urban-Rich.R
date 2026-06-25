
                        # CALCULATION of URBAN BUFFER & SCORE  #

# To run with sampling data of each ecoregion, after 2_Data_Split


library(raster) ; library(ggplot2); 
library(dplyr); library(data.table)

# Calculate an ‘urban’ percentage 1 km and 3 km from each list
# Calculate a score for each observer 
# based on the richness of the lists retrieved from the observations

# wd <- "C:/AMonchy/scripts/eBird_Project"
# setwd(wd)

## Import SAMPLING Data ----

data_dir <- "data/2_Splitting" # "data_in"

# Import Built-Up shapefile of 2015
built2015_all <- raster(file.path("../../SIG/Copernicus","PROBAV_LC100_global_v3.0.1_2015-base_BuiltUp-CoverFraction-layer_EPSG-4326.tif")) #data_dir
# built2019 <- raster("../../SIG/Copernicus/PROBAV_LC100_global_v3.0.1_2019-nrt_BuiltUp-CoverFraction-layer_EPSG-4326.tif")
ecoreg <- st_read(dsn = "data_in/Terrestrial Ecoregions of the World_WWF", layer ="wwf_terr_ecos", quiet=F)

## Import SAMPLING Data ---

## To use it in automatic over a cluster  :
# eco <- read.table(file=file.path(data_dir,"ecoNA.txt"), sep="\t", header=F)
# for(j in 1:nrow(eco)){ #177
#   ecocode <- eco[j,] # eco[1,]
# }
  
# select ecoregion 
ecocode <- "NT0803"

# import sampling data from this ecoregion
sampling_path <- file.path(data_dir, paste0(ecocode,"_sampling.txt")) # ,"ebird"
# if(file.exists(sampling_path)){}
  
  sampling <- read.table(file = sampling_path, sep = "\t", header=T)
  
  # keep only checklist with 1 declared observer
  sampling <- sampling %>% filter(number_observers==1)
  
  # remove checklists with more than 1 observerID
  sampling <- sampling %>% filter(nchar(observer_id) <= 12)
  length(unique(sampling$observer_id))
    

## URBAN BUFFER for SAMPLING DATA ----

# Crop larger than the ecoregion
xmin <- min(sampling$longitude)-2
xmax <- max(sampling$longitude)+2
ymin <- min(sampling$latitude)-2
ymax <- max(sampling$latitude)+2
built2015 <- crop(built2015_all, extent(c(xmin,xmax, ymin, ymax)), verbose =T)
# built2019 <- crop(built2019, extent(c(xmin,xmax, ymin, ymax)), verbose =T)

# ## PLOT
# myCol <- hcl.colors(5, "Light Grays", rev=T) # rev(heat.colors(5))
# plot(built2015,col=myCol, main="Cerrado urbanization raster", breaks=seq(0,100,20))
# plot location of sampling , pch 0 = square
# points(sampling$longitude,sampling$latitude, pch=19, cex = 0.5)

# # information about raster values distribution
# mean_built2015 <- cellStats(built2015, stat='mean')
# sd_built2015 <- cellStats(built2015, stat='sd')
# skew_built2015 <- cellStats(built2015, stat='skew')
# v_built2015 <- getValues(built2015)
# hist(v_built2015, main="Distribution du % d'urbanisation (Cerrado)", xlab="% urbain", cex.main=1, col=myCol, freq=F)

# convert sampling coords in geometry
sampling_spdf <- SpatialPointsDataFrame(cbind(sampling$longitude,sampling$latitude), proj4string=crs(built2015), sampling)

# compute average urban rate 1km & 3 km around or 5km
sampling_buffer_2015 <- cbind(raster::extract(built2015, sampling_spdf, buffer = 1000, fun=mean, df=TRUE), 
                        raster::extract(built2015, sampling_spdf, buffer = 3000, fun=mean, df=TRUE)) #3000
# get checklist ID
sampling_buffer_2015 <- sampling_buffer_2015[,c(1,2,4)]
colnames(sampling_buffer_2015) <- c("list_id", "urb2015_1km" ,"urb2015_3km")
sampling_buffer_2015$list_id <- sampling_spdf$checklist_id
sampling_buffer_2015$urb2015_1km <- round(sampling_buffer_2015$urb2015_1km, digits=3)
sampling_buffer_2015$urb2015_3km <- round(sampling_buffer_2015$urb2015_3km, digits=3)   # urban_3km, digits=3)

# MERGE avg urban rates to sampling data
sampling <- merge(sampling, sampling_buffer_2015, by.x = 'checklist_id', by.y = 'list_id')

# # ##plot distrib : 1km ##
summary(sampling$urb2015_1km)
hist(sampling$urb2015_1km, main="Distribution des listes selon leur % moyen d'urbanisation (1km)", xlab="% moyen urbain",
     col=rev(heat.colors(100)),cex.main=1, breaks=seq(0,100,1), freq=F)
abline(v=median(sampling$urb2015_1km), col="darkblue", lwd=3)

# # ##plot distrib : 3km ##
summary(sampling$urb2015_3km)
hist(sampling$urb2015_3km, main="Distribution des listes selon leur % moyen d'urbanisation (3km)", xlab="% moyen urbain",
     col=rev(heat.colors(100)),cex.main=1, breaks=seq(0,100,1), freq=F)
abline(v=median(sampling$urb2015_3km), col="darkblue", lwd=3)


# Remove lists with NA values in urban_1km (maybe in the sea water)
sampling <- sampling[is.na(sampling$urb2015_1km)==F,]

# # plot
# summary(sampling$urb2015_1km) ; summary(sampling$urb2019_1km)
# ggplot(data=sampling)+
#   geom_histogram(aes(x=urb2015_1km), fill="red") +
#   geom_histogram(aes(x=urb2019_1km), col="black", alpha=0.8)+
#   ggtitle("Distribution du %urbain")

sampling$land <- NA
sampling$land[sampling$urb2015_1km <= 1 & sampling$urb2015_3km <= 5] <- "NonUrban" # <= 5
sampling$land[sampling$urb2015_1km >= 50 & sampling$urb2015_1km <= 90 & sampling$urb2015_3km >= 20] <- "Urban"  # >= 20
sampling$land[(sampling$urb2015_1km > 1 & sampling$urb2015_1km < 60)] <- "NotClassified"

sampling$land <- as.factor(sampling$land)

# # list count in each type of landcover
# table(sampling$land)
barplot(table(sampling$land), col=rev(heat.colors(3)), ylab="# checklist", main="Repartition des classes de listes", space=0)

# ## PLOT
myCol <- rev(heat.colors(5))
plot(built2015,col=myCol, main="Southern Florida", breaks=seq(0,100,20))
# plot according to the land
points(x= sampling$longitude[sampling$land=="Urban"], y=sampling$latitude[sampling$land=="Urban"], pch=16, cex = 0.8)
points(x= sampling$longitude[sampling$land=="NonUrban"], y=sampling$latitude[sampling$land=="NonUrban"], pch=1, cex = 0.8)
points(x= sampling$longitude[sampling$land=="NotClassified"], y=sampling$latitude[sampling$land=="NotClassified"], pch=8, cex = 0.8)
legend("top",horiz=T,pch=c(1,8,16),legend=levels(sampling$land))
# 
# #zoom on one subregion
drawing <- drawExtent()
plot(crop(built2015,drawing),col=myCol, main="Croped Eastern Canadian Forest urbanization raster", breaks=seq(0,100,20))
# plot according to the land
points(x= sampling$longitude[sampling$land=="Urban"], y=sampling$latitude[sampling$land=="Urban"], pch=16, cex = 0.8)
points(x= sampling$longitude[sampling$land=="NonUrban"], y=sampling$latitude[sampling$land=="NonUrban"], pch=1, cex = 0.8)
points(x= sampling$longitude[sampling$land=="NotClassified"], y=sampling$latitude[sampling$land=="NotClassified"], pch=8, cex = 0.8)
legend("bottomleft",horiz=T,pch=c(1,8,16),legend=levels(sampling$land), cex = 0.8)

# distribution des points
hist(crop(built2015,drawing), main="Distribution du % d'urbanisation (Brasilia)", xlab="% urbain", cex.main=1, freq=F)



## GET LIST RICHNESS FROM OBSERVATIONS  ----

# import observations
ebd <- read.table(file = file.path(data_dir, paste0(ecocode,"_ebd.txt")), sep = "\t", header=T)

#Remove observations with more than one observer
ebd <- ebd %>% filter(number_observers==1)
ebd <- ebd %>% filter(nchar(observer_id) <= 12)

# VERIF 
#ensure all checklist in ebd are in sampling file
all(ebd$checklist_id %in% sampling$checklist_id)
ebd[(ebd$checklist_id %in% sampling$checklist_id)==F,]
ebd <- subset(ebd, ebd$checklist_id %in% sampling$checklist_id)

# supprimer les listes pour lesquelles il n'y a pas d'observation
length(unique(sampling$checklist_id)) - length(unique(ebd$checklist_id))
#17 listes n'ont pas d'observations
sampling <- subset(sampling, sampling$checklist_id %in% ebd$checklist_id)

### MERGE EBD info with SAMPLING EVENT ----
# count richness of lists and observed species
species <- ebd %>% 
  group_by(checklist_id)  %>% 
  summarize(observed = paste(unique(scientific_name), collapse=";"), count=n())

# richness of each list
sampling$species_nbr <- 0
# sampling$species_obs <- character()

for(i in 1:nrow(sampling)){
  sampling$species_nbr[i] <- species$count[species$checklist_id == sampling$checklist_id[i]]
  # sampling$species_obs[i] <- species$observed[species$checklist_id == sampling$checklist_id[i]]
}
# check there's not too much lists with only one obs
sampling[sampling$species_nbr==1,"duration_minutes"] #25 / 2037
unique(species$observed[species$count == 1])


## PREPARE DATA for SCORE ESTIMATION   ----

# create seasonal information
sampling$observation_date <- as.Date(sampling$observation_date)
# sampling$day <- as.numeric(format(sampling$observation_date, "%j")) 
# sampling$month <- as.numeric(format(sampling$observation_date, "%m"))
sampling$week <- as.numeric(format(sampling$observation_date, "%W"))

# convert time at which observation started in minutes
sampling$time.min <- sapply(strsplit(as.character(sampling$time_observations_started),":"),
                            function(x) {
                              x <- as.numeric(x)
                              x[1]*60+x[2]})

write.table(sampling, file="NA0605_enhanced_sampling.txt", sep="\t")


## SCORE ESTIMATION ----

# COMPUTE SCORE
if(nrow(sampling) < 100){
  if(length(unique(sampling$observer_id)) < 5){ # for really tiny tiny regions, score is constant
    sampling$score <- 1
  }
  else{ # for small region, there's no nonlinear effects
    
    gamm <- mgcv::gamm(species_nbr ~ protocol_type + duration_minutes + urb2015_1km + time.min + week,
                       random=list(observer_id= ~1), #observer is a random effect
                       data=sampling,
                       family="poisson" )
    
    # Score estimation - Random Effect
    rand_sampling <- nlme::ranef(gamm$lme)
    sampling$score <- rand_sampling[match(sampling$observer_id, rownames(rand_sampling)), "(Intercept)"]
    
  }} else{
  gamm <- mgcv::gamm(species_nbr ~ protocol_type + s(duration_minutes) + s(urb2015_1km) + s(time.min) + s(week),
                     random=list(observer_id= ~1), #observer is a random effect
                     data=sampling,
                     family="poisson" )
  
  # Score estimation - Random Effect
  rand_sampling <- nlme::ranef(gamm$lme)
  rand.observer_sampling <- as.data.frame(rand_sampling$observer_id)
  rand.observer_sampling$observer_id <- substr(rownames(rand.observer_sampling), 9, 50)
  sampling$score <- rand.observer_sampling[match(sampling$observer_id, rand.observer_sampling$observer_id), "(Intercept)"]
}

# summary about score
summary(sampling$score)
any(is.na(sampling$score)) # F
length(unique(sampling$score)) == length(unique(sampling$observer_id)) 

# WRITE table
write.table(sampling, file=file.path("data_out", paste0(ecocode,"_sampling_calculation.txt")), sep="\t")