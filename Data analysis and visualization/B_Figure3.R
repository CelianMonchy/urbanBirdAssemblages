# B. pecies richness of rural versus urban assemblages 
# of the same region, across regions

library(tidyverse) ; library(ggplot2)                           

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

## Plot ----
ggplot() + # avec couleurs
  # geom_abline(intercept=0, slope=1, linetype=2,linewidth=0.5, col="gray70") +
  geom_smooth(data=local_div , aes(x=n_nonurb, y=n_urb, col=as.factor(realm)), 
              method="lm", fullrange=F, se=T, linewidth=1.2, alpha=0.4) +
  geom_point(data=local_div , aes(x=n_nonurb, y=n_urb, shape=as.factor(realm), 
                                  col=as.factor(realm)), size=2) + # alpha=as.factor(realm)
  scale_shape_manual(values=c(1,19)) +
  # scale_alpha_manual(values=c(0.8,1))+
  scale_color_manual(values=c("#64496f", "#1f042a"))+ 
  labs(y=NULL, x=NULL) +
  theme_light() + theme(legend.position = "none", 
                        axis.text=element_text(size=10), axis.title=element_text(size=15)) +
  ylim(0, max(local_div$n_urb)) + xlim(0, max(local_div$n_nonurb))

## Statistical metrics ----
lm_geo <- lm(n_urb ~ n_nonurb, local_div[54:69,]) #[1:53,]
summary(lm_geo)
cor.test(x=local_div$n_nonurb[1:53], y=local_div$n_urb[1:53], method="spearman")
cor.test(x=local_div$n_nonurb[54:69], y=local_div$n_urb[54:69], method="spearman")
