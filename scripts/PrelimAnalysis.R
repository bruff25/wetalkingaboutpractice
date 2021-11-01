####Preliminary Data Analysis####
library(dplyr) #"select" gets masked by EnvCpt
library(zoo)
library(png) #saving plots

setwd("C:/Users/eoruf/Dropbox/Aquaculture/Australia/Preliminary Data Analysis")


##SOUTH AUSTRALIA DATA
dataSA <- read.csv("C:/Users/eoruf/Dropbox/Aquaculture/Australia/Preliminary Data Analysis/SAData.csv")

tunaSA <- dataSA %>%
  dplyr::select(Year, Tuna)

musselSA <- dataSA %>%
  dplyr::select(Year, Mussel) %>%
  dplyr::filter(Mussel != "NA")

abaloneSA <- dataSA %>%
  dplyr::select(Year, Abalone) %>%
  dplyr::filter(Abalone != "NA")

finfishSA <- dataSA %>%
  dplyr::select(Year, Finfish) %>%
  dplyr::filter(Finfish != "NA")

##TASMANIA DATA
dataTAS <- read.csv("C:/Users/eoruf/Dropbox/Aquaculture/Australia/Preliminary Data Analysis/TASData.csv")

abaloneTAS <- dataTAS %>%
  dplyr::select(Year, Abalone) %>%
  dplyr::filter(Abalone != "NA") %>%
  dplyr::filter(Abalone != 16)

musselTAS <- dataTAS %>%
  dplyr::select(Year, Blue.Mussel)

salmonTAS <- dataTAS %>%
  dplyr::select(Year, Salmonids)

#calculate moving averages

dataroll <- data %>%
  mutate(Tuna3prev = zoo::rollmean(Tuna, 3,fill = NA)) %>%
  mutate(Oyster3prev = zoo::rollmean(Oysters, 3, fill = NA)) %>%
  mutate(Mussel3prev = zoo::rollmean(Mussel, 3, fill = NA)) %>%
  mutate(Finfish3prev = zoo::rollmean(Finfish, 3, fill = NA))

#############################
#changepoint attempts

#######EnvCpt#######
library(EnvCpt)

##Tasmania##

#mussels
fit_musselTAS <- envcpt(musselTAS$Blue.Mussel)
plot(fit_musselTAS)
fit_musselTAS$trendcpt@cpts #looking at changepoints for slope
fit_musselTAS$meancpt@cpts #looking at changepoints for means

plot(musselTAS)
rect(2000,100000,2004,1100000,col=grey(0.9),border=F)
lines(musselTAS)
abline(v = c(1997, 2003), col = "red")
abline(v = c(1998, 1999, 2000, 2001), col = "green")



#abalone
fit_abaloneTAS <- envcpt(abaloneTAS$Abalone)
plot(fit_abaloneTAS)
fit_abaloneTAS$trendcpt@cpts
fit_abaloneTAS$meancpt@cpts

plot(abaloneTAS)
rect(2001,500,2004,220000,col=grey(0.9),border=F)
lines(abaloneTAS)
abline(v = c(2000, 2008), col = "red")
abline(v = c(1998, 1999, 2000, 2001), col = "green", lty = 2)



#salmon
fit_salmonTAS <- envcpt(salmonTAS$Salmonids)
plot(fit_salmonTAS)
fit_salmonTAS$trendcpt@cpts
fit_salmonTAS$meancpt@cpts

plot(salmonTAS)
rect(2000,1000000,2001,52000000,col=grey(0.9),border=F)
lines(salmonTAS)
abline(v = c(2004), col = "red")
abline(v = c(1998, 1999, 2005), col = "green")

##South Australia##

#tuna
fit_tunaSA <- envcpt(tunaSA$Tuna)
plot(fit_tunaSA)
fit_tunaSA$trendcpt@cpts
fit_tunaSA$meancpt@cpts

plot(tunaSA)
#rect(2000,1000000,2001,52000000,col=grey(0.9),border=F)
lines(tunaSA)
abline(v = c(1999), col = "red")
abline(v = c(2007, 2011), col = "green")

#mussels
fit_musselSA <- envcpt(musselSA$Mussel)
plot(fit_musselSA)
fit_musselSA$trendcpt@cpts
fit_musselSA$meancpt@cpts

plot(musselSA)
rect(2007,250,2011,2100,col=grey(0.9),border=F)
rect(2014, 250, 2015, 2100, col = grey(0.9), border = F)
lines(musselSA)
abline(v = c(2005), col = "red")
abline(v = c(2005, 2007, 2008, 2012), col = "green", lty = 2)

#abalone
fit_abaloneSA <- envcpt(abaloneSA$Abalone)
plot(fit_abaloneSA)
fit_abaloneSA$trendcpt@cpts
fit_abaloneSA$meancpt@cpts

plot(abaloneSA)
rect(2000,1000000,2001,52000000,col=grey(0.9),border=F)
lines(abaloneSA)
#abline(v = c(2002, 2012), col = "red")
abline(v = c(2005, 2006, 2011), col = "green")

#finfish
##dataset is too short to run with envcpt()


#mcp
library(mcp)
library(rjags)


model = list(Blue.Mussel~1 + Year, ~1 + Year, ~1 + Year) # 3 linear segments
fit_mcp = mcp(model, musselTAS)

summary(fit_mcp)


#####segmented#####
library(segmented)

##South Australia

#tuna
fit_lmTunaSA <- lm(Tuna ~ 1 + Year, data = tunaSA)  # intercept-only model
fit_segmentedTunaSA <- segmented(fit_lmTunaSA, seg.Z = ~Year, npsi = 2)  # Two change points along x

summary(fit_segmentedTunaSA)

plot(fit_segmentedTunaSA)
points(tunaSA)
lines.segmented(fit_segmentedTunaSA)
abline(v = c(2007, 2011), col = "green")
abline(v = c(1999.139, 2009), col = "red")
title("Tuna S. Australia")
#points.segmented(fit_segmentedTunaSA)

#mussel
fit_lmMusselSA <- lm(Mussel ~ 1 + Year, data = musselSA)
fit_segmentedMusselSA <- segmented(fit_lmMusselSA, seg.Z = ~Year, npsi = 2)

summary(fit_segmentedMusselSA)

plot(fit_segmentedMusselSA)
#rect(2007,250,2011,1800,col=grey(0.9),border=F)
#rect(2014, 250, 2015, 1800, col = grey(0.9), border = F)
points(musselSA)
lines.segmented(fit_segmentedMusselSA)
abline(v = c(2005, 2006.193), col = "red")
abline(v = c(2005, 2007, 2008, 2012), col = "green", lty = 2)
title("Mussel S. Australia")

#abalone
fit_lmAbaloneSA <- lm(Abalone ~ 1 + Year, data = abaloneSA)
fit_segmentedAbaloneSA <- segmented(fit_lmAbaloneSA, seg.Z = ~Year, npsi = 2)

summary(fit_segmentedAbaloneSA)

plot(fit_segmentedAbaloneSA)
#rect(2007,250,2011,1800,col=grey(0.9),border=F)
#rect(2014, 250, 2015, 1800, col = grey(0.9), border = F)
points(abaloneSA)
lines.segmented(fit_segmentedAbaloneSA)
abline(v = c(2008.866, 2010.603), col = "red")
abline(v = c(2005, 2006, 2011), col = "green", lty = 2)
title("Abalone S. Australia")

##Tasmania

#salmon
fit_lmSalmonTAS <- lm(Salmonids ~ 1 + Year, data = salmonTAS)
fit_segmentedSalmonTAS <- segmented(fit_lmSalmonTAS, seg.Z = ~Year, npsi = 1)

summary(fit_segmentedSalmonTAS)

plot(fit_segmentedSalmonTAS)
points(salmonTAS)
lines.segmented(fit_segmentedSalmonTAS)
abline(v = c(2004), col = "red")
abline(v = c(1998, 1999, 2005), col = "green")
title("Salmon Tasmania")


#mussel

fit_lmMusselTAS <- lm(Blue.Mussel ~ 1 + Year, data = musselTAS)
fit_segmentedMusselTAS <- segmented(fit_lmMusselTAS, seg.Z = ~Year, npsi = 2)

summary(fit_segmentedMusselTAS)

plot(fit_segmentedMusselTAS)
#rect(2000,100000,2004,1100000,col=grey(0.9),border=F)
points(musselTAS)
lines.segmented(fit_segmentedMusselTAS)
abline(v = c(2001.356, 2008), col = "red")
abline(v = c(1998, 1999, 2000, 2001), col = "green")
title("Mussel Tasmania")

#abalone

fit_lmAbaloneTAS <- lm(Abalone ~ 1 + Year, data = abaloneTAS)
fit_segmentedAbaloneTAS <- segmented(fit_lmAbaloneTAS, seg.Z = ~Year, npsi = 2)

summary(fit_segmentedAbaloneTAS)

plot(fit_segmentedAbaloneTAS)
points(abaloneTAS)
lines.segmented(fit_segmentedAbaloneTAS)
abline(v = c(2001.863, 2007.655), col = "red")
abline(v = c(1998, 1999, 2000, 2001), col = "green")
title("Abalone Tasmania")

#plotting segmented

