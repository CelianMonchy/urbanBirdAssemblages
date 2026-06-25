
                        #  DATA EXTRACTION  #


# Extracts data for the Americas from the global dataset 
# for the specified period (2015–2021),
# using the "stationary" and "traveling" protocols.
# Applies an initial set of filters: duration <= 5 hours, distance <= 5 km
# Collects observers with at least 10 lists across the entire continent


library(auk) ;library(dplyr)


## IMPORT after downloading ---- 
# (to run once at the very beginning)

# wd <- "C:/AMonchy/scripts/eBird_Project" # replace by your own working directory
# setwd(wd)
# path where are stored unzipped raw data files (EBD + Sampling Event Data)
auk_set_ebd_path("D:/...", overwrite=T)

# import EBD and sampling
ebd <- auk_ebd("ebd_relJan-2022.txt", file_sampling = "ebd_sampling_relJan-2022.txt")

# import a file with country codes for America
countrycode <- read.table(file = "ISO3166-1-Alpha2_Code Country_AMERICA.txt", header=T, sep="\t")


## SET PRIMARY FILTERS ----
ebd_filters <- ebd %>%
  # american countries
  auk_country(country = countrycode$CODE)
  # year between 2015 and 2021
  auk_year(year = seq(2015,2021,1)) %>%
  # restrict to the standard traveling and stationary count protocols
  auk_protocol(protocol = c("Stationary", "Traveling")) %>%
  auk_complete()
ebd_filters

# create output files (in WD)
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}
f_ebd <- file.path(data_dir, "ebd_america.txt")
f_sampling <- file.path(data_dir, "ebd_checklists_america.txt")

# APPLY FILTERS (only run if the files don't already exist)
if (!file.exists(f_ebd)) {
  auk_filter(ebd_filters, file = f_ebd, file_sampling = f_sampling)
}
# Transfer data from AMERICA to external storage



## FILTERING sampling from America ----

setwd(wd)
auk_set_ebd_path(paste0(wd,"/data"), overwrite=T) # "D:/ebird_Data"
auk_get_ebd_path()

sampling <- auk_sampling(file = "ebd_checklistsLIGHT_america.txt", sep = "\t") # "ebd_checklists_america.txt"

# SET Additional filters
sampling_filters <- sampling %>% 
  # lists between 15min and 5 h
  auk_duration(duration = c(15, 5*60)) %>%
  # lists shorter than 5 km
  auk_distance(distance = c(0,5)) %>%
  auk_complete()
sampling_filters

# EXTRACT observers only
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}
f_sampling <- file.path(data_dir, "ebd_checklistsLIGHT_america.txt")
# APPLY FILTERS (only run if the files don't already exist)
if (!file.exists(f_sampling)) {
  auk_filter(sampling_filters, file = f_sampling) #, filter_sampling=T)
}



## GET observers' number of lists ----

# Keep only observers data 
auk_filter(sampling_filters, file = file.path(data_dir, "observer_america.txt"), filter_sampling=T,
           keep = "observer id") #c("observer id", "sampling event identifier", "group identifier"))

# IMPORT created file with only observers
observers <- read_sampling(x = obs_sampling, sep = "\t", unique=T)
head(observers)

# Select good observers (10 lists or more in America)
obs1 <- data.frame(obs_id = unlist(strsplit(observers$observer_id, split=",")))
obs2 <- obs1 %>%
  group_by(obs_id) %>%
  summarize(n_list = n()) %>%
  arrange(desc(n_list))
obs3 <- obs2 %>% filter(n_list >= 10)
# dim(obs2)[1] - dim(obs3)[1] #285455 observers are removed (71%)

#Transform format observers
obs3 <- sub("obs", "", obs3$obs_id)
obs3 <- as.integer(obs3)

# keep valid observers list
write.table(obs3, file=file.path(data_dir, "validObservers_america.txt"), sep="\t")
