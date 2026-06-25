# D. Dissimilarity between pairs of rural versus pairs of urban assemblages, across regions

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

datNA <- read.table("Analyses/betaDiversity_NA.txt", sep = "\t", header=T)
datNT <- read.table("Analyses/betaDiversity_NT.txt", sep = "\t", header=T)
dat2 <- rbind(datNA, datNT)
dat2$realm <- c(rep("NA",nrow(datNA)), rep("NT", nrow(datNT)))


## JACCARD ----
jacc3 <- ggplot()+
  geom_abline(intercept=0, slope=1, linetype=2,linewidth=0.5, col="gray70") +
  geom_point(data=dat2, aes(x=J_NONurb, y=J_urb, shape=as.factor(realm),
                            col=as.factor(realm), alpha=as.factor(realm)), size=2) +
  geom_smooth(data=dat2, aes(x=J_NONurb, y=J_urb, col=as.factor(realm)),
              method="lm", fullrange=F, linewidth=1.2, se=T, alpha=0.4)+
  scale_alpha_manual(values=c(0.7,0.9)) + # (0.7,1)
  scale_shape_manual(values=c(1,19))+
  scale_color_manual(values=c("#64496f", "#1f042a"))+ # #b186af"
  # scale_linetype_manual(values=c("dashed","solid"))+
  labs(x=NULL, y=NULL) +
  xlim(0.2,1) + ylim(0.2,1)+
  theme_light() + theme(legend.position = "none",
                        axis.text=element_text(size=10), axis.title=element_text(size=15))

jacc <- ggplot()+
  geom_point(data=dat2, aes(x=J_NONurb, y=J_urb, shape=as.factor(realm), 
                            col=as.factor(realm), alpha=as.factor(realm)), size=1.5) + 
  geom_smooth(data=dat2, aes(x=J_NONurb, y=J_urb, col=as.factor(realm)),
              method="lm", fullrange=F, linewidth=1.2, se=T, alpha=0.4)+ 
  scale_alpha_manual(values=c(0.8,1)) +
  scale_shape_manual(values=c(1,19))+
  scale_color_manual(values=c("gray50", "black"))+ # c("black","gray50")
  # scale_linetype_manual(values=c("dashed","solid"))+
  labs(x=NULL, y=NULL) +
  xlim(0.2,1) + ylim(0.2,1)+
  theme_minimal() + theme(legend.position = "none",
                          axis.text=element_text(size=13), axis.title=element_text(size=15))


jacc2 <- ggplot()+
  geom_point(data=dat2, aes(x=J_NONurb, y=J_urb, shape=as.factor(realm), 
                            col=as.factor(realm), alpha=as.factor(realm)), size=3) + 
  geom_smooth(data=dat2, aes(x=J_NONurb, y=J_urb, col=as.factor(realm)),
              method="lm", fullrange=F, linewidth=1, se=T, alpha=0.2)+ 
  scale_alpha_manual(values=c(0.5,1)) +
  scale_shape_manual(values=c(1,19))+
  scale_color_manual(values=c("#640D5F","#FFB200"))+
  labs(x="Dissimilarity between rural communities", y="Dissimilarity between urban communities") +
  theme_light() + theme(legend.position = "none",
                        axis.text=element_text(size=13), axis.title=element_text(size=15))


### Stats ----
# J_NONurb_adj <- datNA$J_NONurb-1
# lmJNA <-lm(formula= datNA$J_urb~J_NONurb_adj)
lmJNA <-lm(formula= J_urb~J_NONurb, data=datNA)
summary(lmJNA)
# car::linearHypothesis(lmJNA, "J_NONurb=1")
# lmtest::coeftest(lmJNA, vcov. = vcov(lmJNA), rhs=c(NA,0.78))
# test de pente différente de 1
coefs <- as.data.frame(summary(lmJNA)$coefficients)
t_val <- (coefs$Estimate[2]-1) / coefs$`Std. Error`[2]
2 * pt(abs(t_val), df=df.residual(lmJNA), lower.tail = F) # p_val
# test de corrélation
cor.test(x=datNA$J_NONurb,y=datNA$J_urb, method="spearman")

lmJNT <-lm(formula= J_urb~J_NONurb, data=datNT)
summary(lmJNT)
coefs <- as.data.frame(summary(lmJNT)$coefficients)
t_val <- (coefs$Estimate[2]-1) / coefs$`Std. Error`[2]
2 * pt(abs(t_val), df=df.residual(lmJNT), lower.tail = F) # p_val
cor.test(x=datNT$J_NONurb,y=datNT$J_urb, method="spearman")

## Bray-Curtis ----
bc3 <- ggplot()+
  geom_abline(intercept=0, slope=1, linetype=2,linewidth=0.5, col="gray70") +
  geom_point(data=dat2, aes(x=BC_NONurb, y=BC_urb, shape=as.factor(realm), 
                            col=as.factor(realm), alpha=as.factor(realm)), size=2) + 
  geom_smooth(data=dat2, aes(x=BC_NONurb, y=BC_urb, col=as.factor(realm)),
              method="lm", fullrange=F, linewidth=1.2, se=T, alpha=0.4)+
  scale_alpha_manual(values=c(0.7,0.9)) +
  scale_shape_manual(values=c(1,19))+
  scale_color_manual(values=c("#64496f", "#1f042a"))+ # #b186af"
  # scale_linetype_manual(values=c("dashed","solid"))+
  labs(x=NULL, y=NULL) +
  xlim(0.2,1) + ylim(0.2,1)+
  theme_light() + theme(legend.position = "none",
                        axis.text=element_text(size=10), axis.title=element_text(size=15))

bc <- ggplot()+
  geom_point(data=dat2, aes(x=BC_NONurb, y=BC_urb, shape=as.factor(realm), 
                            col=as.factor(realm), alpha=as.factor(realm)), size=1.5) + 
  geom_smooth(data=dat2, aes(x=BC_NONurb, y=BC_urb, col=as.factor(realm)),
              method="lm", fullrange=F, linewidth=0.9, se=T, alpha=0.4)+ 
  geom_abline(intercept=0, slope=1, linetype=2,linewidth=0.5) +
  scale_alpha_manual(values=c(0.8,1)) +
  scale_shape_manual(values=c(1,19))+
  scale_color_manual(values=c("gray50", "black"))+ # c("black","gray50")
  # scale_linetype_manual(values=c("dashed","solid"))+
  labs(x=NULL, y=NULL) +
  xlim(0.2,1) + ylim(0.2,1)+
  theme_minimal() + theme(legend.position = "none",
                          axis.text=element_text(size=13), axis.title=element_text(size=15))
bc2 <- ggplot()+
  geom_point(data=dat2, aes(x=BC_NONurb, y=BC_urb, shape=as.factor(realm), 
                            col=as.factor(realm), alpha=as.factor(realm)), size=3) + 
  geom_smooth(data=dat2, aes(x=BC_NONurb, y=BC_urb, col=as.factor(realm)),
              method="lm", fullrange=F, linewidth=1, se=T, alpha=0.2)+ 
  scale_alpha_manual(values=c(0.5,1)) +
  scale_shape_manual(values=c(1,19))+
  scale_color_manual(values=c("#640D5F","#FFB200"))+ 
  labs(x="Dissimilarity between rural communities", y="Dissimilarity between urban communities") +
  xlim(0.2,1) + ylim(0.2,1)+
  theme_light() + theme(legend.position = "none",
                        axis.text=element_text(size=13), axis.title=element_text(size=15))


### Stats ----
lmDNA <-lm(formula= BC_urb~BC_NONurb, data=datNA)
summary(lmDNA)
coefs <- as.data.frame(summary(lmDNA)$coefficients)
t_val <- (coefs$Estimate[2]-1) / coefs$`Std. Error`[2]
2 * pt(abs(t_val), df=df.residual(lmDNA), lower.tail = F) # p_val
cor.test(x=datNA$BC_NONurb,y=datNA$BC_urb, method="spearman")

lmDNT <-lm(formula= BC_urb~BC_NONurb, data=datNT)
summary(lmDNT)
coefs <- as.data.frame(summary(lmDNT)$coefficients)
t_val <- (coefs$Estimate[2]-1) / coefs$`Std. Error`[2]
2 * pt(abs(t_val), df=df.residual(lmDNT), lower.tail = F) # p_val
cor.test(x=datNT$BC_NONurb,y=datNT$BC_urb, method="spearman")


fig5 <- cowplot::plot_grid(jacc3, bc3,  ncol=2, hjust = -0.5, vjust = -0.5)
ggsave("./Fig5.svg", fig5, "svg")