library(gdata)
library(tidyverse)
library(stringr)
library(stringi)
library(data.table)
library(dplyr)
library(statsr)
library(doBy)
library(ggplot2)
library(reshape)
library(Hmisc)#correlation matrix
library(corrplot)
library(survival)
library(eha)
library(cmprsk)
library(plyr) #count the frequency
#library(qpcR)
library(lattice)


#### Read all the data from the folder and combine ####
setwd('/yaohanl/2017--Patent data/patent_UW/cleaned_data')
file_list <- list.files("/yaohanl/2017--Patent data/patent_UW/cleaned_data",
                        pattern='*.csv')


patents <- do.call("rbind",lapply(file_list,
                                  FUN=function(files){read_csv(files)
                                  }
)
)


patents <- patents[, -c(1)]

patents <- as.data.frame(patents)

table(patents$legal_period, patents$industry_IPC, patents$event)
table(patents$legal_period, patents$industry_IPC)
table(pharmacy$PCT_app, pharmacy$app_year)
table(patents$Chinese, patents$app_year)
table(patents$total_claims, patents$app_year)
table(mean(patents$grant_duration, na.rm = T), patents$app_year)


d <- patents %>%
  group_by(app_year) %>%
  summarise_at(vars(-event), funs(mean(., na.rm=TRUE)))

#****************************************#
#****************************************#
##          1 -- Visualization          ##
#****************************************#
#****************************************#
#****************************************#
#****************************************#
##     1.1 -- Frequency by years        ##
#****************************************#
#****************************************#

frequency1 <- count(patents, "app_year")
frequency2 <- count(patents, "substantive_review_year")
frequency3 <- count(patents, "app_withdraw_year")
# Since there is no applicant_withdraw, grant, or reject data in year 1985, so we need to add an empty column to keep it constant
newrow <- c(1985, 0)
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

frequency3_c <- insertRow(frequency3, newrow, 1)
#Insert one row before year 1986
frequency4 <- count(patents, "reject_year")
frequency4_c <- insertRow(frequency4, newrow, 1)
frequency5 <- count(patents, "grant_year")
frequency5_c <- insertRow(frequency5, newrow, 1)


frequency <- qpcR:::cbind.na(frequency1, frequency2, frequency3_c, frequency4_c, frequency5_c)
frequency <- drop_na(frequency, grant_year)

colnames(frequency)[2] <- "application"
colnames(frequency)[4] <- "substantive_review"
colnames(frequency)[6] <- "applicant_withdraw"
colnames(frequency)[8] <- "reject"
colnames(frequency)[10] <- "grant"

Years <- seq(1985, 2017)
frequency <- qpcR:::cbind.na(frequency, Years)
frequency <- as.data.frame(frequency)


frequency_c <- frequency %>%
  mutate(Application = ifelse(frequency$Years == frequency$app_year, frequency$application, NA),
         Substantive_review =  ifelse(Years == substantive_review_year, substantive_review, NA),
         Applicant_withdraw = ifelse(Years == app_withdraw_year, applicant_withdraw, NA),
         Reject =  ifelse(Years == reject_year, reject, NA),
         Grant =  ifelse(Years == grant_year, grant, NA))

frequency_tb <- frequency_c[c(11:16)]
  

melt_tb <- melt(frequency_tb, id="Years")  # convert to long format
ggplot(melt_tb, aes(x=Years, y=value, colour=variable, shape=variable)) + 
  # use shape to change the point type and when shape = variable point shape will be changed based on variables
  geom_line(linetype = 1) +
  geom_point(size=2.5) + 
  theme_bw() +
  scale_shape(solid = T) +
  scale_x_continuous(breaks=seq(from=1985,to=2017,by=2)) +
  scale_y_continuous(breaks=seq(from=0,to=max(melt_tb$value, na.rm = T)+150000, by=150000))


#****************************************#
#****************************************#
##         1.2 -- IPC Group             ##
#****************************************#
#****************************************#
patents$IPC_group <- str_sub(patents$main_IPC, 1, 1)
table(patents$app_year, patents$IPC_group)

ggplot(patents, aes(x=app_year)) + 
  geom_histogram(binwidth = 0.3, aes(fill = IPC_group)) + 
  scale_x_continuous(breaks=seq(from=1985,to=2017,by=5)) +
  scale_y_continuous(breaks=seq(from=0,to=2430690, by=50000))


#****************************************************#
#****************************************************#
##       1.3 -- Industry Classification             ##
#****************************************************#
#****************************************************#
table(patents$app_year, patents$industry_IPC)
table(patents$app_withdraw_year, patents$industry_IPC)
table(patents$reject_year, patents$industry_IPC)
table(patents$grant_year, patents$industry_IPC)


#********************************************************#
#********************************************************#
##   1.4 -- Statistic in different legal period         ##
#********************************************************#
#********************************************************#
table(patents$event, patents$legal_period)
pharmacy$legal_period <- as.factor(pharmacy$legal_period)
levels(pharmacy$legal_period) <- c("legal_0", "legal_1", "legal_2", "l3gal_3")
table(patents$event, patents$app_year)


#****************************************#
#****************************************#
##       2 -- Correlation Matrix        ##
#****************************************#
#****************************************#
Cor <- as.data.frame(patents[, c(104:106, 12, 14, 16:19, 21, 24:25, 27:28, 30, 94:95, 103)])
Cor$Earlier_publication <- ifelse(is.na(Cor$Earlier_publication), 0, Cor$Earlier_publication) 
summary(Cor) 

write.csv(Cor, file = "/yaohanl/2017--Patent data/patent_UW/cleaned_data/correlation_dataset.csv")
#correlation matrix

correlation <- rcorr(as.matrix(Cor))



#***********************************************#
#***********************************************#
##        3 -- Generate the model dataset      ##
#***********************************************#
#***********************************************#
#put all the variable together
model <- as.data.frame(patents[, c(12:21, 24:25, 27:30, 52:55, 57:92, 94:95, 103, 50:51, 47, 23)])
model$Earlier_publication <- ifelse(is.na(model$Earlier_publication), 0, model$Earlier_publication) 
summary(model)

#seperate the model dataset into different baches
legal_origin <- model[model$legal_period == 0, ]
legal_V1 <- model[model$legal_period == 1, ]
legal_V2 <- model[model$legal_period == 2, ]
legal_V3 <- model[model$legal_period == 3, ]
legal_V3_1 <- legal_V3[(legal_V3$app_year >=2009 & legal_V3$app_year <= 2013), ]
legal_V3_2 <- legal_V3[(legal_V3$app_year >=2014 & legal_V3$app_year <= 2017), ]
## 2014 intelectual property court esteblished in 3 main cities

## check the histogram for count variables
hist(legal_origin$IPCcount)
hist(legal_origin$total_applicant)
hist(legal_origin$total_claims)
hist(legal_origin$total_inventers)


legal_origin$IPCcount <- log(legal_origin$IPCcount)
legal_origin$total_applicant <- log(legal_origin$total_applicant)
legal_origin$total_claims <- log(legal_origin$total_claims)
legal_origin$total_inventers <- log(legal_origin$total_inventers)


