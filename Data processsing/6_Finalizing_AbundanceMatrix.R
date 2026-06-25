
                                    #  ASSEMBLAGE COMPOSITION  #


library(dplyr) ; library(data.table) ; library(ggpubr) ; library(ggplot2)
                                                         
# Collecting species by list
# List of species (species richness)
# Counting the number of occurrences
# Design of the final dataset: a 2-column matrix 
#   showing the frequency of each species in urban and non-urban lists
# (number of occurrences of the species in that ecoregion / 
# total number of occurrences of all species in that ecoregion)                                                     

# wd <- "C:/AMonchy/scripts/eBird_Project"
# setwd(wd)
                                                        
data_dir4 <- "data/4_Filtering" # "data_out"
data_dir5 <- "data/5_Selecting"
data_dir6 <- "data/6_Finalizing"
  
# # For a specific ecoregion
# ecocode <- "NT1315"
# #import data
# sampling <- read.table(file = file.path(data_dir,"4_Filtering",paste0(ecocode,"_sampling_final.txt")), sep = "\t", header=T)
# pairs <- read.table(file = file.path(data_dir,"5_Selecting",paste0(ecocode,"_paired_sampling.txt")), sep = "\t", header=T)
# ebd <- fread(file = file.path(data_dir,"4_Filtering",paste0(ecocode,"_ebd_final.txt"))) 
# 

# For every NT ecoregions
lf_pairs <- list.files(path = data_dir5, pattern="^NT\\d+\\_paired")
# lf_sampling <- list.files(path = file.path(data_dir,"4_Filtering"), pattern="^NT\\d+\\_sampling") # select every NT sampling files
# lf_ebd <- list.files(path = file.path(data_dir,"4_Filtering"), pattern="^NT\\d+\\_ebd") #_final.txt$

# For every NA ecoregions
lf_pairs <- list.files(path = data_dir5, pattern="^NA\\d+\\_paired")

for(i in 1:length(lf_pairs)){
  # import data
  ecocode <- unlist(strsplit(lf_pairs[i],split = "_"))[1]

  pairs <- read.table(file = file.path(data_dir5,lf_pairs[i]), sep = "\t", header=T)
  if(nrow(pairs) < 20){
      file.copy(from= file.path(data_dir5,lf_pairs[i]), to=file.path(data_dir5,"tooSMALL")) #, overwrite=F, recursive=F, copy.mode = F, copy.date = F)
      file.remove(file.path(data_dir5,lf_pairs[i]))
  } else{
  
  # sampling <- read.table(file = file.path(data_dir4, paste0(ecocode,"_sampling_final.txt")), sep = "\t", header=T)
  ebd <- fread(file = file.path(data_dir4, paste0(ecocode,"_ebd_final.txt")))
  
  ## SET THRESHOLD and KEEP LIST OF INTERESTS ----
  thresh <- 20
  paired_sampling <- pairs[1:thresh,]
  # u_sampl <- sampling %>%                                  #URBAN
  #   filter(checklist_id %in% paired_sampling$urb)
  # nu_sampl <- sampling %>%                                 #NOT URBAN
  #   filter(checklist_id %in% paired_sampling$nonurb)
  # left_sampl <- sampling %>%                               #Not ATTRIBUTED and leaving URBAN or NONURBAN
  #   filter((checklist_id %in% paired_sampling$nonurb)==F & (checklist_id %in% paired_sampling$urb)==F)
  
  ## MERGE EBD info with SAMPLING EVENT ----
  # get observations from remaining lists
  u_ebd <- ebd %>%                                        #URBAN
    filter(checklist_id %in% paired_sampling$urb) #u_sampl$checklist_id
  nu_ebd <- ebd %>%                                       #NOT URBAN
    filter(checklist_id %in% paired_sampling$nonurb)

  ## Keeping observations associated to the 40 remaining lists ----
  ### URBAN ----
  u_ebd <- u_ebd %>% # merge_ebd
    dplyr::select(checklist_id, taxonomic_order, scientific_name, observation_count, 
                  longitude, latitude, observation_date, time_observations_started, protocol_type, observer_id) #,
                  # duration_minutes, effort_distance_km, score, urb2015_1km, urb2015_3km, land)
  ### NON-URBAN ----
  nu_ebd <- nu_ebd %>%
    dplyr::select(checklist_id, taxonomic_order, scientific_name, observation_count, 
                  longitude, latitude, observation_date, time_observations_started, protocol_type, observer_id) #,
                  # duration_minutes, effort_distance_km, score, urb2015_1km, urb2015_3km, land)
  
  #write final urban & non urban observations
  write.table(u_ebd, file=file.path(data_dir6, paste0(ecocode,"_urb_obs.txt")), sep="\t", row.names = F, quote=F)
  write.table(nu_ebd, file=file.path(data_dir6, paste0(ecocode,"_NONurb_obs.txt")), sep="\t", row.names = F, quote=F)
}} #end of else & for()
