# A. Overlap in species composition 
# between urban and rural assemblages of the same region

library(tidyverse) ; library(sf) ; library(ggplot2) ; library(mgcv)                            


# wd <- "C:/AMonchy/scripts/eBird_Project"
# setwd(wd)                                             

#import data
data_dir6 <- "data/6_Finalizing" #/NA_20"
lf <- list.files(path = data_dir6, pattern="NONurb")

local_div <- read.table("Analyses/local_diversity.txt", sep = "\t", header=T)
community <- read.table("Analyses/communities_20pairs.txt", sep = "\t", header=T)

ecoreg <- st_read(dsn = "../../SIG/Terrestrial Ecoregions of the World_WWF", layer ="wwf_terr_ecos", quiet=F) #../../SIG
neo <- subset(ecoreg, REALM %in% c("NA", "NT")) # "NT"

local_div$realm <- neo$REALM[match(local_div$eco, neo$eco_code)]

## Venn Diagram ----
grid::grid.newpage()                                        
VennDiagram::draw.pairwise.venn(area1=round(mean(local_div$n_urb[local_div$realm=="NT"])), area2=round(mean(local_div$n_nonurb[local_div$realm=="NT"])), 
                                cross.area=round(mean(local_div$n_inter[local_div$realm=="NT"])),
                                fill=c("#fc8d62", "#66c2a5"),col=NA, alpha=c(0.8,1), cex=2, fontfamily ="sans") #, category=c("URBAIN","NON-URBAIN")

VennDiagram::draw.pairwise.venn(area1=local_div$n_urb[6], area2=local_div$n_nonurb[6], cross.area=local_div$n_inter[6] ,fill=c("#fc8d62", "#66c2a5"),
                                col=NA, alpha=c(0.8,1), cex=2, fontfamily ="sans", inverted=T)


## Metrics ----

# Standard deviation = ecart-type
sqrt((sum((local_div$n_urb[54:69]-mean(local_div$n_urb[54:69]))^2))/(69-54+1-1))
sd(local_div$n_urb[54:69])
# # Standard error
# sd(local_div$n_urb[54:69])/sqrt(69-54+1-1)
# # Variance
# var(local_div$n_urb[54:69])
# sd(local_div$n_urb[54:69])^2

# Strictly urban species
long <- numeric()
for(i in 54:69){ #1:53
  long <- c(long,
            length(which(!is.na(community[,i*2]) & community[,(i*2+1)]==0)))
}
mean(long) ; sd(long)