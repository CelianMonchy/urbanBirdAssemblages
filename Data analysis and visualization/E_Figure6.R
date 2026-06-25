# E. Relationship between geographic distance 
# and dissimilarity of pairs of rural versus pairs of urban assemblages, across regions

library(tidyverse) ; library(ggplot2) ; library(cowplot)  ; library(mgcv)                        

# wd <- "C:/AMonchy/scripts/eBird_Project"
# setwd(wd)                                             

#import data
datNA <- read.table("Analyses/betaDiversity_NA.txt", sep = "\t", header=T)
datNT <- read.table("Analyses/betaDiversity_NT.txt", sep = "\t", header=T)
dat2 <- rbind(datNA, datNT)
dat2$realm <- c(rep("NA",nrow(datNA)), rep("NT", nrow(datNT)))


## Fit model ----
lo <- loess(J_NONurb~D_km, data=datNA)
lo$s # qualité de l'ajustement aux données
lo$enp # proportion de données utilisées pour modéliser la relation
new_data <- data.frame(D_km = seq(min(datNA$D_km), max(datNA$D_km),length.out = 100))
predictions <- predict(lo, newdata = new_data, type = "response", se= TRUE)


## tests stat ----
wilcox.test(datNT$J_NONurb, datNT$J_urb, "greater")
wilcox.test(datNA$J_NONurb, datNA$J_urb, "greater")
wilcox.test(datNT$BC_NONurb, datNT$BC_urb, "greater")
wilcox.test(datNA$BC_NONurb, datNA$BC_urb, "greater")


### NEARCTIC ----
NA_test <- data.frame(D_km=rep(datNA$D_km,2),
                      jacc=c(datNA$J_urb, datNA$J_NONurb),
                      BC=c(datNA$BC_urb, datNA$BC_NONurb),
                      comm=as.factor(rep(c("urban","rural"),each=nrow(datNA))))
levels(NA_test$comm)

# modele_log <- aov(jacc~comm +log(D_km), data=NA_test)
# shapiro.test(residuals(modele_log))
modele_jaccNA <- mgcv::gam(jacc~comm + s(D_km, bs="cs", sp=100), data=NA_test)
modele_bcNA <- mgcv::gam(BC~comm + s(D_km, bs="cs", sp=100), data=NA_test)
summary.gam(modele_jaccNA)
summary(modele_bcNA)
mgcv::k.check(modele_jaccNA) ; mgcv::k.check(modele_bcNA)
mgcv::concurvity(modele_jaccNA)
anova.gam(modele_bcNA)
par(mfrow = c(2, 2))
mgcv::gam.check(modele_jaccNA)


### NEOTROPICS ----
NT_test <- data.frame(D_km=rep(datNT$D_km,2),
                      jacc=c(datNT$J_urb, datNT$J_NONurb),
                      BC=c(datNT$BC_urb, datNT$BC_NONurb),
                      comm=as.factor(rep(c("urban","rural"),each=nrow(datNT))))
# NT_test$D_km_seuil <- ifelse(NT_test$D_km>3000, NT_test$D_km-3000,0)
# NT_test <- NT_test %>%
#   mutate(D_km_seuil = ifelse(D_km>2500, D_km,0),
#          D_km = ifelse(D_km<=2500, D_km,0))
lm_jaccNT <- glm(jacc~comm +D_km+D_km_seuil, data=NT_test)
# levels(NT_test$comm)

modele_jaccNT <- mgcv::gam(jacc~comm + s(D_km, bs="cs", sp=100), data=NT_test) # family = tw(link = "log")) # by=comm
modele_bcNT <- mgcv::gam(BC~comm + s(D_km, bs="cs", sp=100), data=NT_test) #s(log(D_km), k=20), method="REML", family=quasibinomial)
summary(modele_jaccNT)
summary(modele_bcNT)
mgcv::k.check(modele_jaccNT) ; mgcv::k.check(modele_bcNT) 
mgcv::concurvity(modele_jaccNT) ; mgcv::concurvity(modele_bcNT)

par(mfrow = c(2, 2))
mgcv::gam.check(modele_jaccNT)
mgcv::gam.check(modele_bcNT)

par(mfrow = c(4, 2), mar = c(2, 4, 1, 1))
mgcv::plot.gam(modele_jaccNA, all.terms = T, residuals = F, seWithMean = 2)
plot(modele_bcNA, all.terms = T, residuals = F, seWithMean = 2)
mgcv::plot.gam(modele_jaccNT, all.terms = T, residuals = F, seWithMean = 2)
plot(modele_bcNT, all.terms = T, residuals = F, seWithMean = 2)


newdata <- NT_test[,c(1,4)]
newdata$jacc_pred <- mgcv::predict.gam(modele_jaccNT, newdata=newdata)
newdata$bc_pred <- mgcv::predict.gam(modele_bcNT, newdata=newdata)
ggplot()+
  geom_point(data=newdata, aes(x=D_km, y=jacc_pred, col=comm)) +
  geom_smooth(data=NT_test, aes(x=D_km, y=jacc, col=comm), method="gam", fullrange=T, alpha=0.3) +
  scale_color_manual(values=c("#66c2a5", "#fc8d62")) +
  theme_light()
wilcox.test(newdata$jacc_pred[newdata$comm=="rural"], newdata$jacc_pred[newdata$comm=="urban"],
            "greater")
ggplot()+
  geom_point(data=newdata, aes(x=D_km, y=bc_pred, col=comm)) +
  geom_smooth(data=NT_test, aes(x=D_km, y=BC, col=comm), method="gam", fullrange=T, alpha=0.3) +
  scale_color_manual(values=c("#66c2a5", "#fc8d62")) +
  theme_light()
wilcox.test(newdata$bc_pred[newdata$comm=="rural"], newdata$bc_pred[newdata$comm=="urban"],
            "greater")


## Plots ----

### Jaccard ----
NA_jacc <- ggplot(NA_test, aes(x=D_km, y=jacc)) +
  geom_point(aes(col=comm), alpha=0.1, size=2) + 
  geom_smooth(aes(col=comm), method="gam", fullrange=T, alpha=0.3) + 
  scale_color_manual(values=c("#66c2a5", "#fc8d62")) +
  labs(x=NULL, y=NULL) + ylim(0.25,1) + 
  theme_light() + theme(legend.position = "none", axis.text=element_text(size=10)) +
  scale_x_continuous(breaks=seq(0,5000,1000))

NT_jacc <- ggplot(NT_test, aes(x=D_km, y=jacc)) +
  geom_point(aes(col=comm), alpha=0.2, size=2) + #aes(x=D_km, y=J_NONurb)
  # geom_point(aes(x=D_km, y=J_urb), col="#fc8d62", alpha=0.3) +
  geom_smooth(aes(col=comm), method="gam", fullrange=T, alpha=0.3) + #formula=y~s(x)
  # geom_smooth(aes(x=D_km, y=J_urb), method="gam", formula=y~s(x, k=9), col="#fc8d62", fullrange=T, alpha=0.3) +
  scale_color_manual(values=c("#66c2a5", "#fc8d62")) +
  labs(x=NULL, y=NULL) + ylim(0.25,1) + #xlim(0,7400) +
  theme_light() + theme(legend.position = "none", axis.text=element_text(size=10)) +
  scale_x_continuous(breaks=seq(0,7400,1000))


### Bray-Curtis ----
NA_bray <- ggplot(NA_test, aes(x=D_km, y=BC)) +
  geom_point(aes(col=comm), alpha=0.1, size=2) + 
  geom_smooth(aes(col=comm), method="gam", fullrange=T, alpha=0.3) + # formula=y~s(x, sp=2)
  scale_color_manual(values=c("#66c2a5", "#fc8d62")) +
  labs(x=NULL, y=NULL) + ylim(0,1) + 
  theme_light() + theme(legend.position = "none", axis.text=element_text(size=10)) +
  scale_x_continuous(breaks=seq(0,5000,1000))

NT_bray <- ggplot(NT_test, aes(x=D_km, y=BC)) +
  geom_point(aes(col=comm), alpha=0.2, size=2) + 
  geom_smooth(aes(col=comm), method="gam", fullrange=T, alpha=0.3) + #formula=y~s(x)
  scale_color_manual(values=c("#66c2a5", "#fc8d62")) +
  labs(x=NULL, y=NULL) + ylim(0,1) + 
  theme_light() + theme(legend.position = "none", axis.text=element_text(size=10)) +
  scale_x_continuous(breaks=seq(0,7400,1000))

cowplot::plot_grid(NA_jacc, NT_jacc,NA_bray, NT_bray,  ncol=2, hjust = -0.5, vjust = -0.5) #NA_jacc, NT_jacc,