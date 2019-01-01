library(gdata)
library(tidyverse)
library(stringr)
library(stringi)
library(data.table)
library(dplyr)
library(statsr)
library(reshape)
library(Hmisc)#correlation matrix
library(plyr) #count the frequency
library(readxl)
library(splitstackshape)
library(statsr)
library(lubridate)

####################################
###    Part 1 -- Add variables   ###
####################################


setwd('/yaohanl/2017--Patent data/patent_UW/cleaned_data')
file_list <- list.files("/yaohanl/2017--Patent data/patent_UW/cleaned_data",
                        pattern='*.csv')


pharmacy <- do.call("rbind",lapply(file_list,
                                  FUN=function(files){fread(files, header = T, fill = F, encoding = "UTF-8") #read.csv(files) 
                                  }
)
)

# chose the patent applications under pharmacy category
patents$pharmacy <- ifelse(is.na(patents$main_IPC), -999, 
  ifelse(grepl("A61K", patents$main_IPC) & !grepl("A61K8", patents$main_IPC), 1, 0))


pharmacy <- patents[patents$pharmacy == 1, ]

##drop chinese chacters
pharmacy <- pharmacy[, -c(16,21)]

pharmacy <- as.data.frame(pharmacy)


#********************************************#
#********************************************#
##        1 -- Add legal period             ##
##      V1: 1985.04.01 -- 1992.12.31        ##
##      V2: 1993.01.01 -- 2001.06.30        ##
##      V3: 2001.07.01 -- 2009.09.30        ##
##      V4: 2009.10.01 -- 2011.03.13        ##
##      V5: 2011.03.14 -- 2017.12.31        ##
#********************************************#
#********************************************#
# V1 as refersnce group
pharmacy$app_date <- as.Date(pharmacy$app_date)
pharmacy <- pharmacy %>%
  mutate(patent_law_V2 = ifelse(pharmacy$app_date %within% interval(ymd("1993-01-01"), ymd("2001-06-30")), 1, 0), 
         patent_law_V3 = ifelse(pharmacy$app_date %within% interval(ymd("2001-07-01"), ymd("2009-09-30")), 1, 0), 
         patent_law_V4 = ifelse(pharmacy$app_date %within% interval(ymd("2009-10-01"), ymd("2011-03-13")), 1, 0),
         patent_law_V5 = ifelse(pharmacy$app_date %within% interval(ymd("2011-03-14"), ymd("2017-12-31")), 1, 0))

#*****************************************************#
#*****************************************************#
## 2 -- Add control variables IPC_one and claim_one  ##
#*****************************************************#
#*****************************************************#
pharmacy$IPC_one <- ifelse(pharmacy$IPCcount == 1, 1, 0)
pharmacy$total_claims_one <- ifelse(pharmacy$total_claims == 1, 1, 0)

#*****************************************************#
#*****************************************************#
##     3 -- Add earlier publication dummy            ##
#*****************************************************#
#*****************************************************#

#check for earlier publication
pharmacy$app_date <- as.Date(pharmacy$app_date,format = "%Y-%m-%d")
##### early_pub contains NA which because of the NA pub_date_c
##### modify the NULL pub_date_c as pub_date, because those patents are not nondisclosure patents
pharmacy$pub_date <- as.Date(pharmacy$pub_date,format = "%Y.%m.%d")
pharmacy$pub_date_c <- as.Date(pharmacy$pub_date_c,format = "%Y-%m-%d")
pharmacy$app_pub_dur <- as.duration(pharmacy$app_date %--% pharmacy$pub_date_c)/dyears(1)*12
pharmacy$app_pub_dur <- ifelse(is.na(pharmacy$app_pub_dur), as.duration(pharmacy$app_date %--% pharmacy$pub_date)/dyears(1)*12, pharmacy$app_pub_dur)


pharmacy <- pharmacy %>%
  mutate(early_pub = ifelse(app_pub_dur < 18, 1, 0))


#**********************************************************#
#**********************************************************#
##     4 -- Add year of each event                        ##
##     5 -- Add duration of each event                    ##
#**********************************************************#
#**********************************************************#
# put IV at the first several columns in the dataframe and then put the app_year at the end of the column
#keep the year of each event date
pharmacy$substantive_review_year <- str_sub(pharmacy$substantive_review_c, 1, 4)
pharmacy$app_withdraw_year <- str_sub(pharmacy$app_withdraw_c, 1, 4)
pharmacy$reject_year <- str_sub(pharmacy$reject_c, 1, 4)
pharmacy$grant_year <- str_sub(pharmacy$grant_c, 1, 4)


pharmacy$app_withdraw_c <- as.Date(pharmacy$app_withdraw_c,format = "%Y-%m-%d")
pharmacy$reject_c <- as.Date(pharmacy$reject_c,format = "%Y-%m-%d")
pharmacy$grant_c <- as.Date(pharmacy$grant_c,format = "%Y-%m-%d")
pharmacy$withdraw_duration <- as.duration(pharmacy$app_date %--% pharmacy$app_withdraw_c)/dyears(1)*12
pharmacy$refusal_duration <- as.duration(pharmacy$app_date %--% pharmacy5$reject_c)/dyears(1)*12
pharmacy$grant_duration <- as.duration(pharmacy$app_date %--% pharmacy$grant_c)/dyears(1)*12
pharmacy$subview_reject_dur <- as.duration(pharmacy$substantive_review_c %--% pharmacy$reject_c)/dyears(1)*12
pharmacy$subview_grant_dur <- as.duration(pharmacy$substantive_review_c %--% pharmacy$grant_c)/dyears(1)*12
pharmacy$subview_withdraw_dur <- as.duration(pharmacy$substantive_review_c %--% pharmacy$app_withdraw_c)/dyears(1)*12



#**********************************************************#
#**********************************************************#
##     6 -- Add PCT application dummy                     ##
#**********************************************************#
#**********************************************************#
pharmacy$PCT_app <- ifelse(grepl("PCT", pharmacy$Int_filing), 1, 0)


#************************************************#
#************************************************#
##     7 -- Add event dummies                   ##
#************************************************#
#************************************************#
pharmacy <- pharmacy %>%
  mutate(event_withdraw = ifelse(event == 1, 1, 0), 
         event_refusal = ifelse(event == 2, 1, 0), 
         event_grant = ifelse(event == 3, 1, 0))

#************************************************#
#************************************************#
##     8 -- Remove duplicates                   ##
#************************************************#
#************************************************#
## remove duplicate by application number. 
## Because some of the patents data have multiple records, for example, one is for application file, the other is for granted file. 
## But the legal status are unique
## The rule for removing duplication is keep unique record by app_num
pharmacy <- as.data.frame(pharmacy)

pharmacy2 <- pharmacy[!duplicated(pharmacy$app_num), ]

## drop all the raw data which contain Chinese character
pharmacy3 <- pharmacy2[, -c(2, 5:6, 9:10, 12:14, 19:21, 28:29, 54)]

pharmacy2 <- pharmacy %>%
  mutate(type3_other = ifelse((type1_company == 1)|(type2_university == 1), 0, 1))

write.csv(pharmacy2, file = '/yaohanl/2017--Patent data/patent_UW/cleaned_data/pharmacy.csv')

