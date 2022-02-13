setwd("~")
LavieRaw <-read.csv("Lavie2data.csv", header=TRUE)

###  Mark trials in which no response was made, or responses were too fast/slow
LavieRaw$Drop <-0

LavieRaw$Drop[LavieRaw$array.RT <100 | is.null(LavieRaw$array.RESP)] <-1

  #check that it worked correctly
verify <-data.frame(LavieRaw[LavieRaw$Drop ==1,])
verify2 <-data.frame(LavieRaw[LavieRaw$array.RT<100,])

  #count 0,1...So we can calculate percentage of removed trials
library(plyr)
getCount <-count(LavieRaw$Drop)
    #1 0 44218
    #2 1   710
getCount[2,2]/(getCount[1,2] + getCount[2,2])
    #.0158

#Delete bad trials
Lavie.No.Bad <-data.frame(LavieRaw[LavieRaw$Drop != 1,])

count(Lavie.No.Bad$Drop)
  #1 0 44218

####REMOVE RT for inaccurate trials
Lavie.No.Bad$Trim[Lavie.No.Bad$array.ACC==1] <-Lavie.No.Bad$array.RT[Lavie.No.Bad$array.ACC==1]




#Save Work
write.csv(Lavie.No.Bad, "LavieGoodTrials.csv", row.names=FALSE)

#aggregate for analysis
Lavie.Agg <-aggregate(list(RT = Lavie.No.Bad$Trim, ACC = Lavie.No.Bad$array.ACC),
                      by= list(subject = Lavie.No.Bad$subject, 
                               array.type = Lavie.No.Bad$loadLevel,
                               compat = Lavie.No.Bad$trial_type),
                      FUN = mean, na.rm=TRUE)

###Make Factors
Lavie.Agg$array.type <-as.factor(Lavie.Agg$array.type)
  with(Lavie.Agg, tapply(RT, array.type, mean))
    ###0 = No-Line, 1= Line
Lavie.Agg$array.type <-factor(Lavie.Agg$array.type, labels = c("No-Line", "Line"))


####Working Memory Capacity
Lavie.Agg$WMC[Lavie.Agg$subject < 1400] <-0
Lavie.Agg$WMC[Lavie.Agg$subject > 1399] <-1
  ####Add Lablels
Lavie.Agg$WMC <-factor(Lavie.Agg$WMC, labels = c("Low_WMC", "High_WMC"))

Lavie.Agg$subject <-as.factor(Lavie.Agg$subject)
  
  #Score verify
with(Lavie.Agg, tapply(RT, list(WMC, array.type), mean))

####Transform ACC into Error
Lavie.Agg$Err <- (1-Lavie.Agg$ACC)


  #Score verify
with(Lavie.Agg, tapply(Err, list(WMC, array.type), mean))



##################Save that data
setwd("~")
write.csv(Lavie.Agg, "LavieAgg.csv", row.names=FALSE)






######Mixed ANOVA
LAVIEanova <-aov(RT ~ array.type * compat * WMC + Error(subject/(array.type*compat)),
                 data=Lavie.Agg)
summary(LAVIEanova)


LAVIEerr <-aov(Err~ array.type * compat * WMC + Error(subject/(array.type*compat)),
               data=Lavie.Agg)
summary(LAVIEerr)


library(psych)
describeBy(Lavie.Agg$RT, list(Lavie.Agg$WMC, Lavie.Agg$array.type, Lavie.Agg$compat), na.rm=TRUE, type=2)



#############################################
#Figure

Lfig <-read.csv("LavieAgg.csv", header=TRUE)

###get rid of neutral
Lfig2<-Lfig[Lfig$compat!="neut",] 

Lfig2$compat <-as.factor(Lfig2$compat)
Lfig2$array.type <-as.factor(Lfig2$array.type)

library(psych)
XYXY<-describeBy(Lfig2$RT,
           group = Lfig2$array.type : Lfig2$compat,
           mat = TRUE,
           type = 2,
           digits = 0)

XYXY$ArrayType <-sub("[:].*", "", XYXY$group1)    ####take this portion and replace with ""
XYXY$Compatability <-sub(".*[:]", "", XYXY$group1)    



library(ggplot2)
ggplot(XYXY, aes(fill=Compatability, y=mean, x=ArrayType))+
  geom_bar(position="dodge", stat="identity", width = .5)+
  theme_classic()+
  geom_errorbar(aes (ymin = mean - se, ymax = mean + se, width = .2),
                position = position_dodge(0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600))+
  ylab("Response Time")








