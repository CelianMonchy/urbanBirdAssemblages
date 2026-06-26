
                            # COMPUTING ECOLOGICAL DISTANCES #

                                          

library(dplyr)                               

# Construction of the frequency matrix per community
# Ecoregional diversity in local_div
# Computes distance matrices between urban and rural communities

# wd <- "C:/AMonchy/scripts/eBird_Project"
# setwd(wd)                                             

#import data
data_dir6 <- "data/6_Finalizing" #/NA_20"
lf <- list.files(path = data_dir6, pattern="NONurb")


## DISTANCE COMPUTATION FUNCTIONS ----
# Distance euclidienne : euclidean <- function(a, b) sqrt(sum((a - b)^2))
# Distance de Jaccard : jaccard <- 1 -  function(a, b) {intersection = length(intersect(a, b))
# union = length(a) + length(b) - intersection
# return (intersection/union)  }
# Dissimilarité de Bray - Curtis : 1 - sum(apply(df, 2, function(x) abs(max(x)-min(x)))) / sum(rowSums(df))

### Euclidienne ----
euclidean <- function(a, b){
  sqrt( sum((a - b)^2, na.rm=T))
}

euclidean(community[,2],community[,3]) 
local_div$alpha_dist[1]
vegan::vegdist(t(community[,c(2,3)]), "euclidean", na.rm=T)
# dist(t(community[,2:3]), "euclidean")


### Jaccard ----
jaccard <- function(a, b) {
  intersection <-  length(intersect(which(a!=0), which(b!=0)))
  union <- length(which(a!=0)) + length(which(b!=0)) - intersection
  return (1 - intersection/union)
}

jaccard(community[,2],community[,3])
local_div$jaccard_dist[1]
vegan::vegdist(t(community[which(!is.na(community[,2])),2:3]), "jaccard", na.rm=T, binary=T)


### BrayCurtis ----
brayCurtis <- function(a,b) {
  bray <- (2 * sum(apply(cbind(a,b), 1,min), na.rm=T)) / sum(colSums(cbind(a,b), na.rm=T))
  return (1 - bray)
}

brayCurtis(community[,2], community[,3])
vegan::vegdist(t(community[,2:3]), "bray", na.rm=T)



## PREPARING DATA for LOCAL DIVERSITY ----
community <- data.frame()
local_div <- as.data.frame(matrix(nrow=length(lf))) 


for(i in 1:length(lf)){ # 11:13         # c("NT0710_urb", "NT0710_nonurb", "NT0803_urb","NT0803_nonurb")
  # ecocode <- "NA0401"
  ecocode <- unlist(strsplit(lf[i],split = "_"))[1]
  
  # import des observation des 20 paires de listes conservées (avec 6_Finalizing)
  urb_obs <- read.table(file = file.path(data_dir6, paste0(ecocode, "_urb_obs.txt")), sep = "\t", header=T)
  nonurb_obs<- read.table(file = file.path(data_dir6, paste0(ecocode, "_NONurb_obs.txt")), sep = "\t", header=T)
  
  
  # get communities for the ecoregion i
  urb <- urb_obs %>%             # table(u_ebd$scientific_name)
    group_by(scientific_name) %>%
    summarize(count=n(), freq=(n()/20)) #, abundance=sum(as.numeric(observation_count)))
  nonurb <- nonurb_obs %>%
    group_by(scientific_name) %>%
    summarize(count=n(), freq=(n()/20)) #, abundance=sum(as.numeric(observation_count)))
  
  
  # merge communities of the ecoregion i with the other ones
  if(i == 1){
    community <- data.frame(species=sort(union(urb$scientific_name, nonurb$scientific_name)))
    community[,2] <- urb$freq[match(community$species, urb$scientific_name)]
    community[,3] <- nonurb$freq[match(community$species, nonurb$scientific_name)]
    colnames(community)[2:3] <- c(paste0(ecocode,"_urb"), paste0(ecocode,"_nonurb"))
    community[is.na(community[,2]),2] <- 0
    community[is.na(community[,3]),3] <- 0
  }else {
    comm <- data.frame(species=sort(union(urb$scientific_name, nonurb$scientific_name)))
    comm[,2] <- urb$freq[match(comm$species, urb$scientific_name)]
    comm[,3] <- nonurb$freq[match(comm$species, nonurb$scientific_name)]
    colnames(comm)[2:3] <- c(paste0(ecocode,"_urb"), paste0(ecocode,"_nonurb"))
    comm[is.na(comm[,2]),2] <- 0
    comm[is.na(comm[,3]),3] <- 0
    
    community <- merge(community, comm, by="species",all=T)
  }
  
  local_div$eco[i] <- ecocode
  
  # Equitabilité #
  local_div$shannon_urb[i] <- (- sum((urb$freq / sum(urb$freq)) * log2(urb$freq / sum(urb$freq)))) / log2(length(unique(urb$scientific_name)))
  local_div$simpson_urb[i] <- 1 / sum(urb$freq^2)
  # rel <- sum(nonurb$count)
  local_div$shannon_nonurb[i] <- (- sum((nonurb$freq / sum(nonurb$freq)) * log2(nonurb$freq / sum(nonurb$freq)))) / log2(length(unique(nonurb$scientific_name)))
  local_div$simpson_nonurb[i] <- 1 / sum(nonurb$freq^2)
  # 1 - sum((urb$count/rel)^2) #moins fiable statistiquement
  # sum(nonurb$count * (nonurb$count-1)) / (sum(nonurb$count) * (sum(nonurb$count)-1)) #pour les petits échantillons
  
  # Richesse #
  local_div$n_urb[i] <- length(unique(urb$scientific_name))
  local_div$n_nonurb[i] <- length(unique(nonurb$scientific_name))
  local_div$n_inter[i] <- length(intersect(nonurb$scientific_name, urb$scientific_name))
  
  # Densité #
  local_div$dens_urb[i] <- sum(urb$count)
  local_div$dens_nonurb[i] <- sum(nonurb$count)
  
  # Dissimilarité #
  # Distance euclidienne 
  local_div$alpha_dist[i] <- euclidean(community[,i*2], community[,i*2+1])
    # sqrt(sum((community[,i*2]-community[,i*2+1])^2, na.rm=T)) # sqrt(sum((map$urb-map$nonurb)^2, na.rm=T))
  #Distance de Jaccard (quali)
  local_div$jaccard_dist[i] <- jaccard(community[,i*2], community[,i*2+1])
    # 1 - (local_div$n_inter[i] / (local_div$n_urb[i] + local_div$n_nonurb[i] - local_div$n_inter[i]))
  # Distance de Bray-Curtis ~ 1 - % Similarité
  local_div$BrayC_dist[i] <- brayCurtis(community[,i*2], community[,i*2+1])

} #end of for()

# write table
local_div <- local_div[,-1]

write.table(local_div, file="Analyses/local_diversity.txt", sep="\t", quote=F,row.names = F)
write.table(s, file="Analyses/communities_20pairs.txt", sep="\t", quote=F,row.names = F)


## PREPARING DATA for ECO-REGIONAL DIVERSITY ----

### RURAL assemblage ----
nonurb_comm <- community[, seq(1,2*length(lf)+2,2)]
rownames(nonurb_comm) <- nonurb_comm[,1]
nonurb_comm <- nonurb_comm[,-1]

### URBAN assemblage ----
urb_comm <- community[, c(1,seq(2,2*length(lf),2))]
rownames(urb_comm) <- urb_comm[,1]
urb_comm <- urb_comm[,-1]


## EUCLIDIENNE ----

### Between RURAL assemblages ----
eucl_NONurb <- matrix(nrow=length(lf), ncol=length(lf))
ecocodes <- character()
for(i in 1:ncol(nonurb_comm)){ 
  ecocodes <- c(ecocodes, unlist(strsplit(lf[i],split = "_"))[1])
  nonurb_comm[is.na(nonurb_comm[i]),i] <- 0
  # se remplit par ligne
  for(j in 1:ncol(nonurb_comm)){
    # eucl_NONurb[i,j] <- sqrt(sum( (nonurb_comm[,i]-nonurb_comm[,j]) ^2, na.rm=T))
    # eucl_NONurb[j,i] <- sqrt(sum( (nonurb_comm[,i]-nonurb_comm[,j]) ^2, na.rm=T))
    eucl_NONurb[i,j] <- euclidean(nonurb_comm[,i], nonurb_comm[,j])
    eucl_NONurb[j,i] <- euclidean(nonurb_comm[,i], nonurb_comm[,j])
  }
}
rownames(eucl_NONurb) <- ecocodes
colnames(eucl_NONurb) <- ecocodes
# eucl_NONurb <- as.matrix(dist(t(nonurb_comm), method="euclidean"))
# eucl_NONurb <- as.matrix(vegan::vegdist(t(nonurb_comm), "euclidean"))


### Between URBAN assemblages ----
eucl_urb <- matrix(nrow=length(lf), ncol=length(lf))
for(i in 1:ncol(urb_comm)){ 
  urb_comm[is.na(urb_comm[i]),i] <- 0
  # se remplit par ligne
  for(j in 1:ncol(urb_comm)){
    # eucl_urb[i,j] <- sqrt(sum( (urb_comm[,i]-urb_comm[,j]) ^2, na.rm=T))
    # eucl_urb[j,i] <- sqrt(sum( (urb_comm[,i]-urb_comm[,j]) ^2, na.rm=T))
    eucl_urb[i,j] <- euclidean(urb_comm[,i], urb_comm[,j])
    eucl_urb[j,i] <- euclidean(urb_comm[,i], urb_comm[,j])
  }
}
rownames(eucl_urb) <- ecocodes
colnames(eucl_urb) <- ecocodes



## JACCARD ----

### Between RURAL assemblages ----
jacc_NONurb <- matrix(nrow=length(lf), ncol=length(lf))
# nonurb_comm <- community[, seq(1,2*length(lf)+2,2)]
ecocodes <- character()
for(i in 1:length(lf)){ 
  ecocodes <- c(ecocodes, unlist(strsplit(lf[i],split = "_"))[1])
  # nonurb_comm[is.na(nonurb_comm[i]),i] <- 0
  # comm_i <- nonurb_comm[nonurb_comm[,i+1]!=0 & !is.na(nonurb_comm[,i+1]),c(1,i+1)] # nonurb_comm[!is.na(nonurb_comm[,i+1]),c(1,i+1)]
  # se remplit par ligne
  for(j in 1:length(lf)){ 
    # comm_j <- nonurb_comm[nonurb_comm[,j+1]!=0 & !is.na(nonurb_comm[,j+1]),c(1,j+1)] # nonurb_comm[nonurb_comm[,j]!=0,c(1,j+1)]
    # jacc_NONurb[i,j] <- 1 - (length(intersect(comm_i[,1], comm_j[,1])) /
    #                            (nrow(comm_i) + nrow(comm_j) - length(intersect(comm_i[,1], comm_j[,1]))))
    # jacc_NONurb[j,i] <- 1 - (length(intersect(comm_i[,1], comm_j[,1])) /
    #                            (nrow(comm_i) + nrow(comm_j) - length(intersect(comm_i[,1], comm_j[,1]))))
    jacc_NONurb[i,j] <- jaccard(nonurb_comm[,i], nonurb_comm[,j])
    jacc_NONurb[j,i] <- jaccard(nonurb_comm[,i], nonurb_comm[,j])
  }
}
rownames(jacc_NONurb) <- ecocodes
colnames(jacc_NONurb) <- ecocodes

### Between URBAN assemblages ----
jacc_urb <- matrix(nrow=length(lf), ncol=length(lf))

for(i in 1:length(lf)){ 
  urb_comm[is.na(urb_comm[i]),i] <- 0
  # se remplit par ligne
  for(j in 1:length(lf)){ 
    jacc_urb[i,j] <- jaccard(urb_comm[,i], urb_comm[,j])
    jacc_urb[j,i] <- jaccard(urb_comm[,i], urb_comm[,j])
  }
}
rownames(jacc_urb) <- ecocodes
colnames(jacc_urb) <- ecocodes



## BRAY-CURTIS ----

### Between RURAL assemblages ----
brayc_NONurb <- matrix(nrow=length(lf), ncol=length(lf))
# ecocodes <- character()
for(i in 1:ncol(nonurb_comm)){ 
  # ecocodes <- c(ecocodes, unlist(strsplit(lf[i],split = "_"))[1])
  nonurb_comm[is.na(nonurb_comm[i]),i] <- 0
  # se remplit par ligne
  for(j in 1:ncol(nonurb_comm)){
    brayc_NONurb[i,j] <- brayCurtis(nonurb_comm[,i], nonurb_comm[,j])
    brayc_NONurb[j,i] <- brayCurtis(nonurb_comm[,i], nonurb_comm[,j])
    # brayc_NONurb[i,j] <- vegan::vegdist(t(nonurb_comm[,c(i,j)]), "bray", na.rm=T)
    # brayc_NONurb[j,i] <- vegan::vegdist(t(nonurb_comm[,c(i,j)]), "bray", na.rm=T)
  }
}
rownames(brayc_NONurb) <- ecocodes
colnames(brayc_NONurb) <- ecocodes

### Between URBAN assemblages ----
brayc_urb <- matrix(nrow=length(lf), ncol=length(lf))
for(i in 1:ncol(urb_comm)){ 
  urb_comm[is.na(urb_comm[i]),i] <- 0
  # se remplit par ligne
  for(j in 1:ncol(urb_comm)){
    brayc_urb[i,j] <- brayCurtis(urb_comm[,i], urb_comm[,j])
    brayc_urb[j,i] <- brayCurtis(urb_comm[,i], urb_comm[,j])
  }
}
rownames(brayc_urb) <- ecocodes
colnames(brayc_urb) <- ecocodes


## SAVE distance matrices
write.table(eucl_urb, file="Analyses/distanceMatrix_urban.txt", sep="\t", quote=F)
write.table(eucl_NONurb, file="Analyses/distanceMatrix_NONurban.txt", sep="\t", quote=F)
write.table(jacc_urb, file="Analyses/JaccardMatrix_urban.txt", sep="\t", quote=F)
write.table(jacc_NONurb, file="Analyses/JaccardMatrix_NONurban.txt", sep="\t", quote=F)
write.table(brayc_urb, file="Analyses/BrayCurtisMatrix_urban.txt", sep="\t", quote=F)
write.table(brayc_NONurb, file="Analyses/BrayCurtisMatrix_NONurban.txt", sep="\t", quote=F)
