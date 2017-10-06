#Point-framing data and LAI data for a site in Alaska. 
#Point-framing is just a measure of species richness from the number of times a pin dropped from the top of the canopy hits a certain spp.
#LAI measurements are from an leaf area index meter.

pfdata <- read.csv("/Users/elizabethfortin12/Documents/ND First Year/Meetings with Adrian/Fert Project/WorkingData/Point_Framing_Data_2016.csv", header = TRUE)
laidata <- read.csv("/Users/elizabethfortin12/Documents/ND First Year/Meetings with Adrian/Fert Project/WorkingData/LAI.output.csv", header = FALSE)
colnames(laidata) <- c("Site","Block","Treatment","LAI2016","LAI2017")

library(plyr)
pftot <- ddply(pfdata, c("SITE","Treatment"), summarize, Betula=sum(Betula), Tussock=sum(Tussock), Forb=sum(Forb), Total=sum(Total))
colnames(pftot) <- c("Site","Treatment", "Betula","Tussock","Forb","Total")
perc.cov <- ddply(pftot, c("Site", "Treatment"), summarize, Betula = (Betula/Total)*100, Tussock = (Tussock/Total)*100, Forb = (Forb/Total)*100)

laitot <- ddply(laidata, c("Site", "Treatment"), summarize, LAI2016=mean(LAI2016), LAI2017=mean(LAI2017))

pc.lai <- merge(perc.cov, laitot, by = c("Site","Treatment"))
write.table(pc.lai, file = "pc.lai.txt")

pclai <- read.table("/Users/elizabethfortin12/Documents/ND First Year/Biocomputing/R_Programming/pc.lai.txt")

library(ggplot2)
library(grid)
library(gridExtra)
library(reshape2)

pclai.melt <- melt(pclai, id.vars=c('Site','Treatment','LAI2016','LAI2017'))
colnames(pclai.melt) <- c("Site","Treatment","LAI2016","LAI2017","GrowthForm","PercentCover")

a <- ggplot(data = pclai.melt, aes(x=LAI2016, y=PercentCover)) + theme_classic() + 
  geom_point(aes(colour=as.factor(GrowthForm))) +
  stat_smooth(method = 'lm') +
  theme(legend.position = "none") +
  scale_color_manual(values = c("red", "green", "blue")) +
  labs(x="LAI 2016", y="Percent Cover %")
a

b <- ggplot(data = pclai.melt, aes(x=LAI2017, y=PercentCover)) + theme_classic() + 
  geom_point(aes(colour=as.factor(GrowthForm))) +
  stat_smooth(method = 'lm') +
  theme(axis.title.y=element_blank(), legend.title = element_blank(), legend.position = c(0.2,0.9)) +
  scale_color_manual(values = c("red", "green", "blue")) +
  xlab("LAI 2017") 

b

grid.arrange(a, b, ncol = 2)
