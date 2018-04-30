# 3/16 need to finish the clean up process afterwards
# need ot get Steve and Bobbie switched
# query is for only LTC agencies, need to see if Burt and Gary are getting in there

#Add 730 as the max if has not been visited in > 2 years

#Badger 
# CRM>AgencyLocations>Agency Locations - BadgerReps save as badger.csv
# CRM>Activities>Future Activities - Badger save as future.csv
library(tidyverse)
library(lubridate)

tdy <- Sys.Date()
DateFilter <- difftime(tdy, '2012-12-25')-1
TwoYearsAgo <- floor_date(tdy -740, "week") #2 years ago and the previous Sunday (no appts on Sundays)
data.location <- "C:/Users/eyankowski/Documents/EFY/_Projects/BadgerMapping/Data"
tdyCHR <- format.Date(tdy,"%m_%d_%y")
FilterNumber <- tdy - TwoYearsAgo

setwd(data.location)
badger <- read_csv("badger.csv", col_types = cols(`Last CM Packet Date` = col_date(format = "%m/%d/%Y"), 
 `Last Sample Cooler Date` = col_date(format = "%m/%d/%Y"), 
 `Last Stop By Date` = col_date(format = "%m/%d/%Y"), 
 `Last Visit Date` = col_date(format = "%m/%d/%Y")))

future <- read_csv("future.csv", col_types = cols(`Activity Status` = col_skip(), 
 `Activity Type` = col_skip(), `Badger Mapping Id (Regarding)` = col_skip(), 
 `Main Agency (Regarding)` = col_skip(), `Recurring Instance Type` = col_skip(), 
 `Start Date` = col_date(format = "%m/%d/%Y %H:%M %p"),
 `State (Regarding)` = col_skip(), Subject = col_skip()))

nextappt <- future %>% arrange(Regarding,`Start Date`) %>%
  group_by(Regarding) %>%
  top_n(-1) #this is the next appointment date

badgerUPDATED <- badger %>% 
  replace_na(list(`Last Visit Date` = TwoYearsAgo,`Last Stop By Date` = TwoYearsAgo,
                  `Last Visit Type`='None', `Last CM Packet Date`=TwoYearsAgo, 
                  `Last Sample Cooler Date`=TwoYearsAgo, `Address 2`=" ")) %>%
  mutate(`Last Visit Type`=replace(`Last Visit Type`, 
        `Last Visit Type`=='In Person','In Service Presentation'),
         LastLiveVisit=if_else(`Last Visit Date`>`Last Stop By Date`,
        `Last Visit Date`,`Last Stop By Date`)) %>%
  mutate(DaysSinceLastLive=tdy-LastLiveVisit)

colnames(nextappt)<-c("Name","Future Appt")

badger2 <- left_join(badgerUPDATED,nextappt, by="Name") %>%
  mutate(`Visit Scheduled`= if_else(is.na(`Future Appt`),"No Visit Scheduled","Visit Scheduled"))

#write function to replace all Dec 25, 2012 with " "
badger2$`Last Visit Date` <- as.character(badger2$`Last Visit Date`)
badger2$`Last Visit Date`[badger2$`Last Visit Date`==TwoYearsAgo] <- " "

badger2$`Last Stop By Date` <- as.character(badger2$`Last Stop By Date`)
badger2$`Last Stop By Date`[badger2$`Last Stop By Date`==TwoYearsAgo] <- " "

badger2$`Last CM Packet Date` <- as.character(badger2$`Last CM Packet Date`)
badger2$`Last CM Packet Date`[badger2$`Last CM Packet Date`==TwoYearsAgo] <- " "

badger2$`Last Sample Cooler Date` <- as.character(badger2$`Last Sample Cooler Date`)
badger2$`Last Sample Cooler Date`[badger2$`Last Sample Cooler Date`==TwoYearsAgo] <- " "

badger2$LastLiveVisit <- as.character(badger2$LastLiveVisit)
badger2$LastLiveVisit[badger2$LastLiveVisit==TwoYearsAgo] <- " "

badger2$`Future Appt` <- as.character(badger2$`Future Appt`)
badger2$`Future Appt`[is.na(badger2$`Future Appt`)]<- " "

badger3 <- badger2 %>% mutate(DaysSinceLastLive=if_else(DaysSinceLastLive>=FilterNumber,FilterNumber,
                              DaysSinceLastLive))

######################################
## create the folder on my computer ##
######################################
output.location <- "C:/Users/eyankowski/Documents/EFY/_Projects/BadgerMapping/Output"
output.location <- paste(output.location,tdyCHR, sep = "/")
dir.create(output.location,showWarnings = TRUE, recursive = FALSE, mode = "0777")
#####################################################

##Output Folder
setwd(output.location)
badger_output <- badger3[,c(1:3,5:16,18,20,19,4)]
colnames(badger_output)[1] <- "Badger Mapping ID"
colnames(badger_output)[6] <- "Address"
colnames(badger_output)[7] <- "Suite"
colnames(badger_output)[19] <- "Territory Manager"
write.csv(badger_output,sprintf("PurFoodsUpdate_%s.csv",tdy),row.names=FALSE)


###############################
#CleanUp
# find the files that you want
listoffiles <- list.files(data.location, "\\.csv$",full.names = T)
# copy the files to the new folder
file.copy(listoffiles, output.location)
# remove original files
file.remove(listoffiles)

##Graphics
MonthOfLastVisit <- badger %>% 
  mutate(MO=floor_date(`Last Visit Date`,"month")) %>%
  replace_na(list(MO='2000-01-01'))%>%
  group_by(MO) %>%
  summarize(ByMonth=n())%>%
  arrange(MO)

ggplot(MonthOfLastVisit, aes(MO,ByMonth))+
  geom_bar(stat="identity")
