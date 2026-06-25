
                        #  URBAN & RURAL Lists SELECTION  #


# To run with sampling each ecoregion, after 4Filtering

library(dplyr) ;library(ggplot2) ; library(sf) ; library(raster) ; library(rgl)

# Calculate the distance to the coastline for each list
# Check the elevation for each list
# Select the maximum number of pairs of the closest lists 
# (based on their Euclidean distance)

# wd <- "C:/AMonchy/scripts/eBird_Project"
# setwd(wd)

## Import coastline data ----
sig.dir <- "data_in/SIG"  # "../../SIG"
coastline <- st_read(dsn = file.path(sig.dir, "NaturalEarth"), layer ="ne_Vector_50m_coastline", quiet=F)
elevation <- raster(file.path(sig.dir,"ElevationData", "GTopo30_America.tif"))

# get files with lists remaining on 2 months
data_dir <- "data_out/4_Filtering"
lf <- list.files(path = data_dir, pattern="sampling")
data_dir2 <- "data_out/5_Selecting"

# ecocode <- "NA0302"

for(i in 7:49){ # NA404 - NA0616 
  #import sampling data
  sampling <- read.table(file = file.path(data_dir, lf[i]), sep = "\t", header=T)
  
  n_urb <- nrow(sampling[sampling$land=="Urban",])
  n_nonurb <- nrow(sampling[sampling$land=="NonUrban",])
  
  # import report
  ecocode <- unlist(strsplit(lf[i],split = "_"))[1]
  report <- read.table(file="5_report.txt",sep="\t", header=T)
  report[nrow(report)+1,"eco"] <- ecocode
  report$nlist_urb[report$eco==ecocode] <-   n_urb
  report$nlist_nonurb[report$eco==ecocode] <-   n_nonurb
  
  if(n_urb  == 0 | n_nonurb==0){
    # report$sd_30pairedlist[report$eco==ecocode] <- 0
    #edit report
    write.table(report[report$eco==ecocode,], file="5_report.txt", sep="\t", quote=F,row.names = F, col.names = F, append=T,eol="\r")
  }
  else{
    # Keep only urban and non urban lists
    sampling <- sampling %>%
      filter(land =="Urban" | land=="NonUrban")
    
    # Boxing geographic area
    xmin <- min(sampling$longitude)-2
    xmax <- max(sampling$longitude)+2
    ymin <- min(sampling$latitude)-2
    ymax <- max(sampling$latitude)+2
    
    bbox <- st_as_sfc(st_bbox(c(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), crs = 4326)) #, crs = 4326
    
    ## DISTANCE TO COASTLINE CALCULATION ----
    # sf_use_s2(F)
    coast <- st_intersection(coastline$geometry, bbox)
    lon <- as.numeric(sampling[,"longitude"])
    lat <- as.numeric(sampling[,"latitude"])
    points <- st_multipoint(matrix(c(lon,lat), nrow=nrow(sampling), byrow=F)) #st_point(cbind(lon[1],lat[1]))
    points <-  st_sfc(points, crs = 4326)
    # #plot
    # ggplot() +
    #   geom_sf(data = coast, show.legend=F)+
    #   geom_sf(data=points, color="red") +
    #   ggtitle("Lists & Coastline") +
    #   coord_sf()
    if(length(coast) == 0){
      sampling$dist2coast <- 1
      report$no_coast[report$eco==ecocode] <- "TRUE"
    } else{
      #compute
      d <-  geosphere:: dist2Line(as(as_Spatial(points),"SpatialPoints"), as_Spatial(coast)) # ID of the object nearest
      #assign distance 2 coast to lists
      d <- as.data.frame(d)
      sampling$dist2coast <- d$distance
      report$no_coast[report$eco==ecocode] <- "FALSE"
    }
    
    ## ELEVATION EXTRACTION ----
    elev <- crop(elevation, extent(c(xmin,xmax, ymin, ymax)), verbose =T)
    sampling_spdf <- SpatialPointsDataFrame(cbind(sampling$longitude,sampling$latitude), proj4string=crs(elev), data=sampling)
    sampling_elev <- raster::extract(x=elev, y=sampling_spdf, df=TRUE)
    
    colnames(sampling_elev) <- c("list_id", "elevation")
    sampling_elev$list_id <- sampling_spdf$checklist_id
    # set NA values to 0
    # nrow(sampling_elev[is.na(sampling_elev$elevation),])
    sampling_elev$elevation[is.na(sampling_elev$elevation)] <- 1
    
    # MERGE elevation to sampling data
    sampling <- merge(sampling, sampling_elev, by.x = 'checklist_id', by.y = 'list_id')
    
    # ## DISPLAY scatter3d ##
    # sampling$dist2coast <- round(sampling$dist2coast/1000, digits=3)
    # plot3d(x=sampling$dist2coast[sampling$land=="Urban"], y=sampling$elevation[sampling$land=="Urban"],
    #        z=sampling$duration_minutes[sampling$land=="Urban"], size=5, col="red",
    #        xlab="Distance To Coastline", ylab="Elevation", zlab="Duration")
    # points3d(x=sampling$dist2coast[sampling$land=="NotClassified"], y=sampling$elevation[sampling$land=="NotClassified"],
    #          z=sampling$duration_minutes[sampling$land=="NotClassified"],size=5, col="lightblue")
    # points3d(x=sampling$dist2coast[sampling$land=="NonUrban"], y=sampling$elevation[sampling$land=="NonUrban"],
    #          z=sampling$duration_minutes[sampling$land=="NonUrban"], size=5, col="black")
    #
    # points3d(x=sampling$dist2coast[c(76,239)], y=sampling$elevation[c(76,239)], z=sampling$duration_minutes[c(76,239)], size=5, col="blue")
    
    
    ## VECTOR TRANSFORMATION ----
    b <- sampling[,c("checklist_id","land","duration_minutes", "dist2coast","elevation")] %>%
      # filter(land=="Urban"| land=="NonUrban") %>%
      mutate(dist2coast = log(dist2coast))
    urb <- b[b$land=="Urban",]
    nonurb <- b[b$land=="NonUrban",]
    
    ## Create matrix distance ----
    e <- data.frame()
    c <- 1
    for(j in 1:nrow(urb)){
      for(k in 1:nrow(nonurb)){
        e[c,1] <- urb$checklist_id[j] # $urb
        e[c,2] <- nonurb$checklist_id[k] # $nonurb
        # e[c,3] <- apply(cbind(urb[j,3:5],nonurb[k,3:5]), 1, function(x){dist(matrix(x,nrow=2, byrow=T), method="euclidean")}) # $dist
        e[c,3] <- sqrt((urb$duration_minutes[j]-nonurb$duration_minutes[k])^2 + (urb$dist2coast[j]-nonurb$dist2coast[k])^2 + (urb$elevation[j]-nonurb$elevation[k])^2)
        c <- c+1
      }
    }
    colnames(e) <- c("urb", "nonurb", "dist")
    e2 <- e
    ## PAIRWISE ----
    pairs <- data.frame()
    for(j in 1:min(nrow(urb), nrow(nonurb))){
      pairs <- rbind(pairs, e2[e2$dist==min(e2$dist),][1,])
      e2 <- subset(e2, e2$urb != pairs[j,1] & e2$nonurb != pairs[j,2])
    }
    # Ecart-type (standard deviation)
    report$sd_20pairedlist[report$eco==ecocode] <- sd(pairs$dist[1:20]) # until the 20th pair
    
    # #plot3D
    # urb2 <- sampling[sampling$checklist_id %in% pairs2$urb,]
    # urb3 <- sampling[(sampling$checklist_id %in% pairs2$urb)==F & sampling$land=="Urban",]
    # nonurb2 <- sampling[sampling$checklist_id %in% pairs2$nonurb,]
    # plot3d(x=log(urb2$dist2coast), y=urb2$elevation, z=urb2$duration_minutes, 
    #        size=5, xlab="LOG(Distance To Coastline)", ylab="Elevation", zlab="Duration", col="red")
    # points3d(x=log(nonurb2$dist2coast), y=nonurb2$elevation, z=nonurb2$duration_minutes, size=5)
    # points3d(x=log(urb3$dist2coast), y=urb3$elevation, z=urb3$duration_minutes, size=5, col="gray70")
    
    #save sampling table
    write.table(pairs, file=file.path(data_dir2, paste0(ecocode,"_paired_sampling.txt")), sep="\t", row.names = F, quote=F)
    
    #edit report
    write.table(report[report$eco==ecocode,], file="5_report.txt", sep="\t", quote=F,row.names = F, col.names = F, append=T,eol="\r")
  }
}

# #edit report
# write.table(report, file="5_report.txt", sep="\t", quote=F,row.names = F, col.names = T ,eol="\r") #, append=T

