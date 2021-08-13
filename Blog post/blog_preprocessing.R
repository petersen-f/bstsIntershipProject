data<-read.csv("ESMdata.csv")#, header = F)


library(bsts)
library(dplyr)
library(lubridate)
library(tidyverse)
dat <- data[,c(2,6,9,11:26,41)]



dat$activity <- as.factor(data[,"act_what1"])
dat$person <- as.factor(data[,"soc_who1"])

# only those until relapse
#dat<-dat[1:934,] 


positive_items <- c("mood_relaxed","mood_satisfi","mood_enthus","mood_cheerf","mood_strong","pat_concent")
negative_items <- c("mood_down","mood_lonely","mood_anxious","mood_suspic","mood_guilty","mood_doubt","pat_worry","phy_tired")
restless_items <- c("mood_irritat","pat_restl","pat_agitate")
# some cleaming for the factorial variables

dat$activity <- recode(dat$activity,
                  "0" = "nothing"
                  ,"1" = "resting"
                  ,"10"= "work/studies"
                  #",20" = "housekeeping/shopping"
                  ,"21"= "caring for others"
                  #",26"" = "medical care"
                  ,"27" = "taking care of oneself"
                  #",41" = "sports"
                  ,"43"= "active relaxation"
                  ,"45"= "passive relaxation"
                  #",47" = "chatting/texting/facebook etc"
                  #",49" = "chilling"
                  ,"51"= "talking"
                  ,"60"= "eating/drinking"
                  ,"88"= "traveling"
                  ,"89"= "other")

#levels(dat$who)
dat$person <- recode(dat$person,
                  "0"= "nobody"
                  ,"10" = "partner"
                  #,"17" = "family resident"
                  ,"19" = "roommates"
                  #,"27" = "family non-resident"
                  ,"29" = "family living at other places"
                  ,"30" = "friends"
                  ,"40" = "colleagues"
                  ,"49" = "acquaintances"
                  ,"50" = "strangers/others")


dat <- dat[complete.cases(dat),]

# feeling down, lonely and anxious were reported on a scale from -3 to 3 and must be recoded to 1 to 7


dat[,c("mood_down","mood_lonely","mood_anxious")] <- dat[,c("mood_down","mood_lonely","mood_anxious")] +4


# compound na and pa scores

dat$negative_mood <- rowMeans(dat[negative_items])
dat$positive_mood <- rowMeans(dat[,c(positive_items)])


# create restlessness compound and invert restlessness to make it negative/give it positive correlation with othe restlessness variables
dat$restlessness_compound <- rowMeans(cbind(dat[restless_items]))

dat$overall_mood <- rowMeans(cbind(dat[negative_items],8-dat[positive_items]))


#dat$lag_act <- lag(dat$act)
#dat$lag_who <- lag(dat$who)


dat1 <- dat[,c("date","negative_mood","positive_mood","pat_restl","restlessness_compound","overall_mood","activity","person")]

write.csv(dat1,file = "esm_cleaned.csv")





# data until short after relapse
dat$date <- dmy(dat$date)
dat$resptime_e <- hms(dat$resptime_e)
dat$date_time <- with(dat, date + resptime_e)
dat_relapse <- dat[1:934,] 

write.csv(dat_relapse,file = "esm_restlessness.csv")


# aggregate to halfday

dat_halfday <- dat[1:934,] 

dat_halfday$day_half <-  ifelse(dat_halfday$beepno<6,1,2)

dat_halfday <- dat_halfday %>% group_by(date,day_half) %>% 
                summarise(restlessness_compound=mean(restlessness_compound), restlessness = mean(pat_restl)) %>% 
                complete(date,day_half=1:2)

# if so data for first half of day it is substituted with NA values


dat_halfday$restlessness_compound_log <- log(dat_halfday$restlessness_compound)
dat_halfday$restlessness_log <- log(dat_halfday$restlessness)
write.csv(dat_halfday,file = "esm_halfday.csv")

x=dat_halfday$restlessness
ggplot(NULL) +
  geom_histogram(aes(x=x,y=..density..),binwidth = 0.3) +
  geom_density(aes(x=x),alpha=.2, fill="#FF6666")+ theme_classic()



# aggregate to daily

dat_daily <- dat[1:934,] 

dat_daily <- dat_daily %>% group_by(date) %>% summarise(restlessness_compound=mean(restlessness_compound), restlessness = mean(pat_restl))

dat_daily$restlessness_compound_log <- log(dat_daily$restlessness_compound)
dat_daily$restlessness_log <- log(dat_daily$restlessness)
write.csv(dat_daily,file = "esm_daily.csv")

