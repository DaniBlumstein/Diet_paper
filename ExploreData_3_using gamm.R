##############################
###  2/05/21  E Linder Analysis
###  3/21/21  using gamm
###############################

load("diet_all.Rdata")
summary(diet_all)

# Using it as a data frame - use a short name for convenience
dd <- as.data.frame(diet_all);   rm(diet_all)

#  Creating nominal categorical variables (Sex,experiment )
index <- c(1,8)
for (i in index) dd[,i] <- as.factor(dd[,i])
summary(dd)
#   dd[1:20,] ; tail(dd)

## make continuous time variable
library(timeDate)
dd$doy <- dayOfYear(as.timeDate(dd$StartDate))
dd$ctime <- dd$doy + dd$time_in_S / (24*60*60)
summary(dd$ctime)

## make time in day (values between 0 and 1) ; scale may be better for gam
dd$time_in_D <- dd$time_in_S /(24 * 60 * 60)
summary(dd$time_in_D)


#  Plotting the daily cycle:
par(mfrow=c(1,2))
with(dd, plot(time_in_D, EE,main = "Circadian Rhythm: Energy Expenditure"))
with(dd, plot(time_in_D, H2Omg,main = "Circadian Rhythm: Water Retention"))
with(dd, plot(time_in_D, RQ))

# Plotting the daily cycle by experiment
library(ggplot2)
ggplot(dd, aes(x = time_in_D, y = EE)) +
  geom_point()+ 
  facet_wrap(~experiment)
ggplot(dd, aes(x = time_in_D, y = H2Omg)) +
  geom_point()+ 
  facet_wrap(~experiment)
ggplot(dd, aes(x = time_in_D, y = RQ)) +
  geom_point()+ 
  facet_wrap(~experiment)

# Correct the mistake of animal 6
dd$Animal_ID[dd$Animal_ID == "6" & dd$doy > 100] <- "6a"
unique(dd$Animal_ID)
table(dd$Animal_ID)   #  now looks good!

# make Animal_ID a factor:
dd$ID <- as.factor(dd$Animal_ID)
#  table(dd$ID)

# There are 8 groups of experiments with the following start days:
#   51  57  70  75 231 234 259 263 
#    7   7   7   7   7   7   7   6

## group animals with adjacent experiment start date together

dd$startexp <- 0  # initializing

ind <- levels(dd$ID)
for (i in 1:length(ind) ){  # i = 1
  startEXP <- min(dd$doy[dd$ID == ind[i] ],na.rm=T)
  dd$startexp[dd$ID == ind[i] ] <- startEXP
}
table(dd$startexp)

# printing out the design
design <- matrix("m",nrow=1, ncol=4)
colnames(design) <- c("Sex","experiment","StartDate","ID")

design <- subset(dd,select=c("Sex","experiment","StartDate","ID"),subset = (dd$ID == ind[1]))[1,]
for (i in 2:length(ind) ){ # i=2
  ddsub <- subset(dd,select=c("Sex","experiment","StartDate","ID"),subset = (dd$ID == ind[i]))[1,]
  design <- rbind(design, as.vector(ddsub[1,]) )
}

design[order(design$StartDate),]   #  this prints it



# create "time in experiment"  in units of day
dd$ctime.exp <- dd$ctime - dd$startexp
    #   hist(dd$ctime.exp)   #  looks good; units are days

 


## Plotting the time traces:
##  use EE,  H2Omg, RQ

# Color selection for colorblind folks
cbp1 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00")

library(ggplot2)
ggplot(data = dd, aes(ctime,H2Omg)) +
  geom_line(aes(colour = ID),show.legend = F) +
  scale_colour_manual(values=rep(cbp2,8)) +
  facet_wrap(~startexp, scales = "free_x")
           
#  RQ has some outliers in the 2nd and 7th group of experiments
#  H2Omg  has some aberrant traces

## make startexp categorical
dd$startexp <- as.factor(dd$startexp)
levs <- levels(dd$startexp)

# Additional color pallets,  not used
# library(RColorBrewer)
# library(wesanderson)

ddd <- subset(dd,startexp == levs[1])
ggplot(data = ddd, aes(ctime,H2Omg)) +
  geom_line(aes(color=ID)) +
  scale_colour_manual(values=cbp2)
  
#  scale_color_brewer(palette="Set1")
 


##################################################################
### Using Mixed Effects gam 
##################################################################
library(mgcv)
## note:  startexp == batches

library(MASS)
M1 <- glmmPQL(H2Omg ~ experiment*Sex, random = list(startexp= ~1), data = dd, family = gaussian)
  # not the best model

#########################################
### Now include smooth terms using gamm
#########################################

M2 <- gamm(H2Omg ~ experiment+Sex + s(ctime.exp,k=3) + s(time_in_D, bs="cc") + s(EE) + s(RQ), data = dd,
           random = list(startexp = ~1, ID = ~1|startexp)  )
summary(M2$gam)  
#  summary(M2$lme)   #  detailed report about component standard deviations

par(mfrow=c(2,2),mar = c(4,4,1,1),oma=c(0,0,3,0))
plot(M2$gam,scale = 0,cex = .7,col = "red", residuals = T,shade = T, shade.col = "blue")
title("Smooths for H2Omg",outer=T)

M3 <- gamm(log(EE) ~ experiment+Sex + s(ctime.exp,k=3) + s(time_in_D, bs="cc") + s(H2Omg) + s(RQ), data = dd,
           random = list(startexp = ~1, ID = ~1|startexp)  )
summary(M3$gam)  
#  summary(M3$lme)   #  detailed report about component standard deviations

par(mfrow=c(2,2),mar = c(4,4,1,1),oma=c(0,0,3,0))
plot(M3$gam,scale = 0,cex = .7,col = "red", residuals = T,shade = T, shade.col = "blue")
title("Smooths for log EE",outer=T)

M4 <- gamm(log(RQ) ~ experiment+Sex + s(ctime.exp,k=3) + s(time_in_D, bs="cc") + s(H2Omg) + s(log(EE)), data = dd,
           random = list(startexp = ~1, ID = ~1|startexp)  )

s(time_in_D, by = Sex, bs = "cc") + 
  s(time_in_D, by = experiment, bs= "cc")

summary(M4$gam)  
#  summary(M4$lme)   #  detailed report about component standard deviations

par(mfrow=c(2,2),mar = c(4,4,1,1),oma=c(0,0,3,0))
plot(M4$gam,scale = 0,cex = .7,col = "red", residuals = T,shade = T, shade.col = "blue")
title("Smooths for log RQ",outer=T)

## This includes smooth terms grouped by experiment and by Sex

M5 <- gamm(log(RQ) ~ experiment+Sex + s(ctime.exp,k=3) + s(time_in_D, bs="cc") +
    + s(time_in_D, by = Sex, bs = "cc") + s(time_in_D, by = experiment, bs= "cc")
    + s(H2Omg) + s(log(EE)), data = dd, random = list(startexp = ~1, ID = ~1|startexp)  )

summary(M5$gam)  
#  summary(M4$lme)   #  detailed report about component standard deviations

par(mfrow=c(2,2),mar = c(4,4,1,1),oma=c(0,0,3,0))
plot(M5$gam,scale = 0,cex = .7,col = "red", residuals = T,shade = T, shade.col = "blue")
title("Smooths for log RQ",outer=T)


  
 