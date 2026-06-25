# C. Relationship between species’ abundances in rural assemblages 
# and their occurrence in the corresponding urban assemblages 

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


## Analysis ----
i=1
pv <- data.frame(Inter=numeric(), rur=numeric()) #numeric()
odds_ratios <- data.frame(Inter=numeric(), rur=numeric())# numeric()
pred_presU <- numeric()
se_presU <- numeric()
n_urb <- numeric()
n_rur <- numeric()
for(i in 1:length(lf)){
  p <- na.omit(ifelse(community[,i*2]>0, 1,0))
  n_urb <- c(n_urb, length(which(community[,i*2]!=0)))
  n_rur <- c(n_rur, length(which(community[,i*2+1]!=0)))
  rur <- na.omit(community[,i*2+1])*20
  reglog <- glm(p~rur, family="binomial")
  # summary(reglog)
  odds_ratios <- rbind(odds_ratios, exp(coef(reglog)))
  pred <- predict(reglog,
                  newdata = data.frame(rur = seq(0,20, length.out = 100)),
                  type = "response", se.fit=T)
  # pred <- fitted(reglog, se.fit=T)
  pred_presU <- rbind(pred_presU, pred$fit)
  se_presU <- rbind(se_presU, pred$'se.fit')
  pv <- rbind(pv, broom::tidy(reglog)$p.value)
  # CI <- rbind(CI, exp(confint(reglog, parm = "rur")))
}
OR_pv <- data.frame(eco=local_div$eco, inter_OR=odds_ratios[,1], inter_pv=pv[,1], 
                    rur_OR=odds_ratios[,2], rur_pv=pv[,2])
OR_pv <- cbind(OR_pv, pred_presU, se_presU)

# plot(x=1:69, y=(n_rur-n_urb))
# abline(a=0, b=0, col="blue")

# cowplot::plot_grid(
#   ggplot() + # Nearctics
#     geom_histogram(data=OR_pv[1:53,], aes(rur_OR),binwidth=0.05, fill="grey50")+ #binwidth =0.1
#     geom_histogram(data=OR_pv[1:53,] %>% filter(rur_pv<0.05), aes(rur_OR), fill="blue", binwidth=0.05, alpha=0.4)+ #binwidth =0.1
#     geom_vline(aes(xintercept = 1), linetype=2) +
#     labs(x="Odd-Ratio for rural frequence") +
#     theme_light() + theme(legend.position = "none",
#                           axis.text=element_text(size=10), axis.title=element_text(size=12)) +
#     scale_y_continuous(breaks=0:10, minor_breaks = NULL), #+ xlim(0.7,1.7),
#   # scale_x_continuous(breaks=seq(0.8,1.6,0.2), minor_breaks = seq(0.8,1.6,0.1)),
#   ggplot() + # Neotropics
#     geom_histogram(data=OR_pv[54:69,],aes(rur_OR), binwidth=0.05, fill="grey50")+ # binwidth=0.1,
#     geom_histogram(data=OR_pv[54:69,] %>% filter(rur_pv<0.05), aes(rur_OR), fill="blue", binwidth=0.05, alpha=0.4)+ #, binwidth=0.1
#     geom_vline(aes(xintercept = 1), linetype=2) +
#     labs(x="Odd-Ratio for rural frequence") +
#     theme_light() + theme(legend.position = "none",
#                           axis.text=element_text(size=10), axis.title=element_text(size=12)) +
#     scale_y_continuous(breaks=0:3, minor_breaks = NULL) #+ xlim(0.7,1.7)
#   # scale_x_continuous(breaks=round(seq(-0.2,0.8,0.1), digits=1), minor_breaks = seq(-0.2,0.8,0.05)) #seq(-0.2,0.6,0.05))
# )

### logistic model ----
logPred <- OR_pv[,-(106:205)] %>% # [,-(2:5)]
  # rename_with(~paste0("se",.),-(1:105)) %>%
  # filter(str_starts(eco, "NA")) %>%
  pivot_longer(cols='1':'100', values_to = "predicted", names_to="x") %>% # ,names_to="eco"
  # pivot_longer(cols=-(1:5), names_to = c("predicted", "se"), names_sep = "_") %>%
  mutate(x=rep(seq(0,20, length.out = 100),69), .before=predicted)

se_col <- OR_pv[,-(2:105)] %>%
  pivot_longer(cols='1':'100', values_to = "se") %>%
  select(se)

logPredNA <- logPred %>%
  bind_cols(se_col) %>%
  filter(str_starts(eco, "NA"))
logPredNT <- logPred %>%
  bind_cols(se_col) %>%
  filter(str_starts(eco, "NT"))

## Metrics ----
# Positive association
nrow(OR_pv[OR_pv$eco %in% OR_pv$eco[1:53] & OR_pv$rur_OR>1,])/53
nrow(OR_pv[OR_pv$eco %in% OR_pv$eco[54:69] & OR_pv$rur_OR>1,])/16
# Significant
nrow(OR_pv[OR_pv$eco %in% OR_pv$eco[1:53] & OR_pv$rur_pv<0.05,]) /53
nrow(OR_pv[OR_pv$eco %in% OR_pv$eco[54:69] & OR_pv$rur_pv<0.05 & OR_pv$rur_OR>1,]) /16
# Negative association 
nrow(OR_pv[OR_pv$eco %in% OR_pv$eco[1:53] & OR_pv$rur_OR<1,])/53
nrow(OR_pv[OR_pv$eco %in% OR_pv$eco[54:69] & OR_pv$rur_OR<1,])/16

## PLOT Figure 4 ----
cowplot::plot_grid(
  ggplot() +
    geom_ribbon(data=logPredNA %>% filter(rur_pv>0.05), aes(x=x,ymin=predicted-0.5*se, ymax=predicted+0.5*se, group=eco), fill="#e1e0dd", alpha=0.4) +
    geom_ribbon(data=logPredNA %>% filter(rur_pv<0.05), aes(x=x,ymin=predicted-0.5*se, ymax=predicted+0.5*se, group=eco), fill= "#9daddf", alpha=0.4) +
    geom_line(data=logPredNA %>% filter(rur_pv>0.05), aes(x=x,y=predicted, group=eco), col="#878377", linewidth=1, alpha=0.6) +
    geom_line(data=logPredNA %>% filter(rur_pv<0.05), aes(x=x,y=predicted, group=eco), col= "#3a5cbe" , linewidth=1, alpha=0.6) +
    scale_y_continuous(breaks=seq(0,1,0.2), minor_breaks = seq(0,1,0.1)) +
    labs(x="Species frequency in rural lists", 
         y="Predicted probability to find the species in the urban community") +
    theme_light() + theme(legend.position = "none",
                          axis.text=element_text(size=10), axis.title=element_text(size=12)),
  ggplot() +
    geom_ribbon(data=logPredNT %>% filter(rur_pv>0.05), aes(x=x,ymin=predicted-0.5*se, ymax=predicted+0.5*se, group=eco), fill="#e1e0dd", alpha=0.4) +
    geom_ribbon(data=logPredNT %>% filter(rur_pv<0.05), aes(x=x,ymin=predicted-0.5*se, ymax=predicted+0.5*se, group=eco), fill="#9daddf", alpha=0.4) +
    geom_line(data=logPredNT %>% filter(rur_pv>0.05), aes(x=x,y=predicted, group=eco), col="#878377", linewidth=1, alpha=0.6) +
    geom_line(data=logPredNT %>% filter(rur_pv<0.05), aes(x=x,y=predicted, group=eco), col="#3a5cbe", linewidth=01, alpha=0.6) +
    scale_y_continuous(breaks=seq(0,1,0.2), minor_breaks = seq(0,1,0.1)) +
    labs(x="Species frequency in rural lists", 
         y="Predicted probability to find the species in the urban community") +
    theme_light() + theme(legend.position = "none",
                          axis.text=element_text(size=10), axis.title=element_text(size=12))
)


## Figure Supp 8 ----
# # rho computation
# community[community==0] <- NA # si on ne veut que les espèces en commun
cor <- numeric()
pv <- numeric()
pvL <-  numeric()
for(i in 1:length(lf)){
  cor <- c(cor, cor.test(x=community[,i*2], y=community[,i*2+1], method="spearman", use="complete.obs", exact=F)$estimate)
  pv <- c(pv, cor.test(x=community[,i*2], y=community[,i*2+1], method="spearman", use="complete.obs", exact=F, "greater")$p.value)
  pvL <- c(pvL, cor.test(x=community[,i*2], y=community[,i*2+1], method="spearman", use="complete.obs", exact=F, "less")$p.value)
}
cor_pv <- data.frame(eco=local_div$eco, cor,pv, pvL)

# stats
cor_pvNA <- cor_pv[1:53,]
cor_pvNT <- cor_pv[54:69,]
## positive
nrow(cor_pvNA[cor_pvNA$cor>0,]) / 53
nrow(cor_pvNT[cor_pvNT$cor>0,]) / 16
## significant
nrow(cor_pvNA[cor_pvNA$cor>0 & cor_pvNA$pv<0.05,]) / 53
nrow(cor_pvNT[cor_pvNT$cor>0 & cor_pvNT$pv<0.05,]) / 16
## negative
nrow(cor_pvNA[cor_pvNA$cor<0,]) / 53
nrow(cor_pvNT[cor_pvNT$cor<0,]) / 16

cowplot::plot_grid(
  ggplot() + # Nearctics
    geom_histogram(data=cor_pv[1:53,], aes(cor), binwidth =0.05, fill="grey50")+ #binwidth =0.05
    geom_histogram(data=cor_pv[1:53,] %>% filter(pv<0.05), aes(cor), binwidth =0.05, fill="blue", alpha=0.4)+ #, col="blue")+
    geom_histogram(data=cor_pv[1:53,] %>% filter(pvL<0.05), aes(cor), binwidth =0.05, fill="red", alpha=0.5)+
    labs(x="Spearman Rho Rank Correlation") +
    theme_light() + theme(legend.position = "none",
                          axis.text=element_text(size=13), axis.title=element_text(size=15))+
    scale_y_continuous(breaks=0:8, minor_breaks = NULL) +
    scale_x_continuous(breaks=seq(-0.2,0.8,0.1), minor_breaks = seq(-0.2,0.8,0.05)), #seq(-0.2,0.6,0.05))
  # scale_x_continuous(breaks=seq(-0.2,0.6,0.1), minor_breaks = seq(-0.2,0.6,0.05)), #seq(-0.2,0.6,0.05))
  ggplot() + # Neotropics
    geom_histogram(data=cor_pv[54:69,], aes(cor), binwidth=0.05, fill="grey50")+ # bins=20
    geom_histogram(data=cor_pv[54:69,] %>% filter(pv<0.05), aes(cor), binwidth=0.05, fill="blue", alpha=0.4)+ #, col="blue")+
    geom_histogram(data=cor_pv[54:69,] %>% filter(pvL<0.05), aes(cor), binwidth=0.05, fill="red", alpha=0.4)+
    labs(x="Spearman Rho Rank Correlation") +
    theme_light() + theme(legend.position = "none",
                          axis.text=element_text(size=13), axis.title=element_text(size=15)) +
    scale_y_continuous(breaks=0:4, minor_breaks = NULL) +
    scale_x_continuous(breaks=round(seq(-0.2,0.8,0.1), digits=1), minor_breaks = seq(-0.2,0.8,0.05)) #seq(-0.2,0.6,0.05))
  # scale_x_continuous(breaks=round(seq(-0.3,0.6,0.1), digits=1), minor_breaks = seq(-0.25,0.6,0.05)) #seq(-0.2,0.6,0.05))
)
