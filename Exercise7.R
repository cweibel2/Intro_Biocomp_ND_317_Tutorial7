# Question 1

#Uploading data
setwd("~/Desktop/Biocomputing/Week6")
fasta=scan(file = "Lecture11.fasta.txt",what=character())

# Using Stuart's code to get my matrix values becuase my code 
# is still kind of weird

# pre-allocate matrix to store sequence information
summ=matrix(NA,length(fasta)/2,4)

# loop through lines of fasta and process each record
for(i in 1:length(fasta)){
  # if-else to figure out if we have a recordID line or sequence line
  if(grepl(">",fasta[i])){
    summ[((i+1)/2),1]=substr(fasta[i],2,nchar(fasta[i])) # could use counter instead of complicated row calculation
  }else{
    seqLength=nchar(fasta[i])
    
    numG=nchar(fasta[i])-nchar(gsub("G","",fasta[i]))
    numC=nchar(fasta[i])-nchar(gsub("C","",fasta[i]))
    if(seqLength<=14){
      Tm=2*(numG+numC)+2*seqLength
    }else{
      Tm=-9999
    }
    
    summ[i/2,2]=seqLength
    summ[i/2,3]=round((numG+numC)/seqLength*100,1)
    summ[i/2,4]=Tm
  }
}

seqSumm=data.frame(sequenceID=summ[,1],sequenceLength=as.numeric(summ[,2]),percentGC=as.numeric(summ[,3]),meltingTemp=as.numeric(summ[,4]))

#### Graph 1 - Histogram of sequence lengths

a = ggplot(data = seqSumm, aes(x = sequenceLength))
a + geom_histogram() + theme_classic() + stat_bin(binwidth = 1) + 
  xlab("Sequence Length") + ylab("Count") + 
  ggtitle("Histogram of Sequence Length")

#### Graph 2 - Histogram of %GC Content

b  = ggplot(data = seqSumm, aes(x = percentGC))
b + geom_histogram() + theme_classic() + xlab("% GC Content") + 
  ylab("Count") + ggtitle("Histogram of %GC Content")



#Question 2
#Point-framing data and LAI data for a site in Alaska. 
#Point-framing is just a measure of species richness from the number of times a pin dropped from the top of the canopy hits a certain spp.
#LAI measurements are from a leaf area index meter.

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


#Question 3
data <- read.csv("/Users/elizabethfortin12/Documents/ND First Year/Biocomputing/R_Programming/Intro_Biocomp_ND_317_Tutorial7/data.txt")

ddply(data, c("region"), summarize, mean=mean(observations))

d <- ggplot(data = data)
d + geom_bar(aes(x = as.factor(region), y = observations), stat = "summary",
            fun.y = "mean", fill = "black", color = "black") + theme_classic() +
            xlab("Region") + ylab("Mean Observation") + ggtitle("Mean Observations in Each Region")

a <- ggplot(data = data)
a + theme_classic() + geom_jitter(aes(x=as.factor(region),y=observations), alpha=0.1) + labs(x='Region', y='Observations') + ggtitle("Observations in Each Region")
#Yes because the bar plot compares the averages of each region, 
#while the geom_jitter plot gives you an idea of the average and 
#the spread because it shows each individual data point
