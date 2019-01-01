# Load libraries ----------------------------------------------------------

library(gdata)
library(readxl)
library(tidyverse)
library(stringr)
library(stringi)
library(data.table)
library(dplyr)
library(splitstackshape)
library(statsr)
library(lubridate)
library(ineq)

### Set system language as Chinese so that the Chinese characters in the data can be shown ###
Sys.getlocale()
Sys.setlocale(category = "LC_ALL", locale = "chs")

getwd()
#### Read all the data from the folder and combine ####
setwd('/yaohanl/2017--Patent data/patent_UW/data')
file_list <- list.files('/yaohanl/2017--Patent data/patent_UW/data',
                        pattern='*.xls')


patents <- do.call("rbind",lapply(file_list,
                                  FUN=function(files){read_excel(files, col_types = "text")
                                  }
)
)

#### rename the variables ####
names(patents) <- c("app_num", "title", "main_IPC", "IPC", "applicant", "inventer",
                    "pub_date", "pub_num", "agency", "agent", "app_date", "address",
                    "priority", "nation_province_code", "abstract", "main_claim", "Int_filing",
                    "Int_pub", "entry_date", "initial_app_num", "claim",
                    "legal_status", "patent_status", "type")

#### check and remove duplications
## check by app_num
patents %>%
  group_by(app_num) %>%
  count(sort=TRUE) %>%
  head(5)

## remove duplications
patents <- unique(patents) 

#*************************************************************#
#*************************************************************#
#1-- Parse out IPC and get the number of totalIPC and         #
#                    uniqueIPC for each patent                #
#*************************************************************#
#*************************************************************#
# We combine pre and post 2006. The code is to deal with the complications in original IPC column
## Split IPC's and extract the first 4 charaters

patents2 <- cSplit(patents, "IPC", sep = "//|//\\(|\\([A-Z]{1}", drop = F, fixed = F) # split using "//" or "//(" or "(+ one letter" as delimiter

#patents2 <- cSplit(patents2, "IPC_1", sep = ";", drop = F) 

# We are only interested in the stuff before the above delimiters
## Note, depending on how many elements parsed out from "IPC", the second step of parsing might be applied on "IPC_01" or "IPC_1"
if ("IPC_01" %in% names(patents2)) {
    patents2 <- cSplit(patents2, "IPC_01", sep = ";", drop = F)
  } else {patents2 <- cSplit(patents2, "IPC_1", sep= ";", drop = F)}


patents2 <- as.data.frame(patents2)

## Find out the main IPC group (i.e, A - H) a IPC belongs to
patents2$main_IPC_group <- str_sub(patents2$main_IPC, 1, 1)

## Find out all IPC subgroups a patent belongs to
str4 <- function(x){str_sub(x, 1,4)} # define a function to extract the first 4 characters

if ("IPC_01" %in% names(patents2)) {
  IPC <- grep("IPC_01_", names(patents2), value = TRUE)
} else {IPC <- grep("IPC_1_", names(patents2), value = TRUE)}

patents2[IPC] <- lapply(patents2[IPC],str4)

## get rid of special chacraters (∴) in columns "IPC_1_"
IPC_clean_f <- function(x){
  ifelse(is.na(x), x,
         ifelse(grepl("[A-Z]{1}[0-9]{2}[A-Z]{1}", x), x, NA))
  
}

patents2[IPC] <- lapply(patents2[IPC], IPC_clean_f)

# change names of splitted IPC's to make them consistent as post 2006 data. 
# UPDATE: since we eventually drop the splited IPC's, it might not be necessary to change names
# if ("IPC_01" %in% names(patents2)) {
#  IPC2 <- gsub("IPC_01_", "IPCs_", IPC)
#} else {IPC2 <- gsub("IPC_1_", "IPCs_", IPC)} # use "IPCs_" as prefix in case the result of the first Csplit has IPC_01,
                                              # i.e., avoid two "IPC_01"
#setnames(patents2, old = IPC, new = IPC2)

patents2 <- as.data.frame(patents2)

# calculate the Gini-Simpson index of diversity of IPC's to measure technological diversity of a patent
##  The larger the number, the more diverse
get.GS.index <- function(x) {
  x <- factor(x)
  return(1-sum(prop.table(table(x))^2))
  }

patents2$tech_div <- apply(patents2[IPC], 1, get.GS.index)


# total number of IPC's
patents2$IPCcount <- apply(patents2[IPC], 1, function(x) sum(!is.na(x)))

# create a dummy to indicate the cases with only one IPC code, note that the Gini index for those cases are 0!!
patents2$SingleIPC <- ifelse(patents2$IPCcount == 1, 1, 0)


# Define a function to count unique non-NA elements (i.e., IPC's)
Non_NA_unique <- function(x){
  if (sum(is.na(x)) !=  0 ) {    ## if there is "NA", "NA" as a unique value needs to be excluded
    length(unique(x))-1
  } else {
    length(unique(x))            ## if there is no "NA"
  }
}
patents2$IPCcount_unique <- apply(patents2[IPC], 1, Non_NA_unique)

#***********************************************#
#***********************************************#
#2--get the number of applicants for each patent#
#***********************************************#
#***********************************************#

# Parse the applicant column and reset the data as a dataframe
patents2 <- cSplit(patents2, "applicant", ";", drop=F)
patents2 <- as.data.frame(patents2)

#count the total number of applicants for each patent.
applicant <- grep("applicant_", names(patents2), value = TRUE)
patents2$total_applicant <- apply(patents2[applicant], 1, function(x) sum(!is.na(x)))

# Create a dummy variable: collaboration - if the applicants > 1,which is regarded as a 
# collabration, return 1, if total applicants <= 1,return 0. Note this is cross-organizational
# collaboration. Some patents might have multiple inventors, but if they are all in the same
# org. that is not collaboration according to this definition. 
patents2$collaboration_applicant <- ifelse(patents2$total_applicant > 1, 1, 0) # create a dummy variable indicating whether the patent is collaborated
as.factor(patents2$collaboration) # encode the variable "collaboration" as a factor (categorical variable)

#*****************************************************************#
#*****************************************************************#
# 3-- determine the type of the applicants,                       #
# "company" = 1, "university" = 2, "individual" = 3, "other" = 0  #
#*****************************************************************#
#*****************************************************************#

# create an empty matrix
app_type <- matrix(NA, nrow=nrow(patents2), ncol=length(applicant)) 

# rename the column names of the empty matrix
app_type_names <- applicant
app_type_names <- gsub("applicant_", "app_type_", app_type_names) 
colnames(app_type) <- app_type_names

# Create a new dataset by appending the empty matrix to the current dataset (i.e., patents2)
patents3 <- as.data.frame(cbind(patents2, app_type))

# Define a function to categorize applicants (1 = company, 2 = research, 3 = individual, 0 = other)
app_type_f <- function(x, y){
  x = as.character(x)
  y <- ifelse (is.na(x), x, 
              ifelse(grepl("公司|株式会社", x), 1,
                    ifelse(grepl("研究|大学|学院", x), 2,
                              ifelse(nchar(x) <= 3|grepl("·", x), 3, 0
                                     )))) 
     
}

# Apply the function on patents3 to fill the appended applicant matrix
patents3[app_type_names] <- lapply(patents3[applicant], app_type_f, y = patents3[app_type_names])
patents3[app_type_names] <- lapply(patents3[app_type_names], as.character)

# Create 3 dummy variables to indicate if applicants contain company, university or individual
# Note that it is possible that more than one of these variables are 1
patents3$type1_company <- 0
patents3$type2_university <- 0
patents3$type3_individual <- 0

for (i in 1:nrow(patents3)) {
  if (is.element(1, patents3[i, app_type_names])) {patents3$type1_company[i] <- 1} 
  if (is.element(2, patents3[i, app_type_names])) {patents3$type2_university[i] <- 1}
  if (is.element(3, patents3[i, app_type_names])) {patents3$type3_individual[i] <- 1}
}
patents3[app_type_names] <- lapply(patents3[app_type_names], as.factor) # setting these variables as factor has to be after 

## some frquency tables ##
table(patents3$collaboration)
table(patents3$app_type_01)
table(patents3$type1_company)
table(patents3$type2_university)
table(patents3$type3_individual)


#****************************************************#
#****************************************************#
#  4--count the number for inventer for each patent  #
#****************************************************#
#****************************************************#

# Parse the inventor column and reset the data as a dataframe
patents4 <- cSplit(patents3, "inventer", ";", drop=F)
patents4 <- as.data.frame(patents4)

#count the total number of inventors for each patent.
inventer_vec <- grep("inventer_", names(patents4), value = TRUE)
patents4$total_inventers <- apply(patents4[inventer_vec], 1, function(x) sum(!is.na(x)))
patents4$collaboration_inventer <- ifelse(patents4$total_inventers >=2, 1, 0)


#*********************************#
#*********************************#
#  5--Agency and Agents           #
#*********************************#
#*********************************#

# Whether an agency is used
patents5 <- patents4
patents5$if_agency <- lapply(patents5$agency, function(x)length(which(!is.na(x)))) 
# create a dummy variable.
#Note if only which() is used, NA's in agency would be integer(0)
#(which has a length of 0), not a value of 0

# Count the number of agents
patents5 <- cSplit(patents5, "agent", ";", drop=F)
patents5 <- as.data.frame(patents5)
agent_vec <- grep("agent_", names(patents5), value = TRUE)
patents5$total_agents <- apply(patents5[agent_vec], 1, function(x)sum(!is.na(x)))


#************************************************#
#************************************************#
# 6--count the number of claims for each patent  #
#************************************************#
#************************************************#

### Note that the number of claims can be underestimated, because in the original data,
### some claims are not followed by a period "。"
patents6 <- cSplit(patents5, "claim", sep = "(。[0-9]+、)|(。[0-9]+，)|(。[0-9]+.)|(。-->[0-9]+)|(。([0-9]+))|(。[[0-9]+])", drop=F)
patents6 <- as.data.frame(patents6)
claim_vec <- grep("claim_", names(patents6), value = TRUE)

###### NOTE: There are cases where claim is empty ######
## (1) For these cases, we cannot simply assume there is only one claim, so we treat total_claim as missing
## and replace it with the median number of claims of all patent applications of that year. The reason we use 
## median rather than average is because the distribution of total_claim is rather skewed, with a few cases 
## having more than 400 claims. 
## (2) However, if their main claim is not numbered, very likely there is only one claim. 
## (3) When main claim is missing, total_claims is NA. 

patents6$total_claims_temp <- ifelse(!is.na(patents6$claim), apply(patents6[claim_vec], 1, function(x) sum(!is.na(x))), NA)

patents6$app_year <- str_sub(patents$app_date, 1, 4)

patents6$total_claims <- ifelse(is.na(patents6$main_claim), NA,
                                ifelse(!is.na(patents6$claim), patents6$total_claims_temp, 
                                ifelse(is.na(patents6$claim) & !is.na(patents6$main_claim) & grepl("^1、|^1.", patents6$main_claim),
                                       ave(patents6$total_claims_temp, patents6$app_year, FUN = function(x) median(x, na.rm = T)), 1)))

table(patents6$total_claims, exclude = NULL) # NOTE: total_claims2 can still be NA, when main_claim is NA

patents6$paid_claims <- ifelse(patents6$total_claims > 10, 1, 0)

#*********$$$$$$$$*********%%%%%%%%**********@@@@@@@***********#
#*********$$$$$$$$*********%%%%%%%%**********@@@@@@@***********#
### Drop splitted claims_, inventer_, agent_, app_type_,     ###
### applicant_, total_claims_temp                            ###
#*********$$$$$$$$*********%%%%%%%%**********@@@@@@@***********#
#*********$$$$$$$$*********%%%%%%%%**********@@@@@@@***********#

patents7 <- patents6[, -grep("claim_|inventer_|agent_|app_type_|total_claims_temp|applicant_", colnames(patents6))]

#****************************************************************************************#
#****************************************************************************************#
# 7--patent status for each patent (at the end of observation period, i.e., 2017-12-31), #
#                             无效=2， 有效=1， 在审=3                                   #
#****************************************************************************************#
#****************************************************************************************#
patents8 <- patents7
patentstatus <- c("有效" = 1, "无效" = 2,"在审" = 3)
patents8$patent_status_n <- patentstatus[patents8$patent_status] # match 1, 2, 3 with "有效"， "无效"，"在审" in "patent_status"


#**************************************************************#
#**************************************************************#
# 8--nation_province_code, whether it is chinese patent or not #
#**************************************************************#
#**************************************************************#

## NOTE: Chinese patents include applications from HK(81), MO(82), and TW(71). They are not membership "countries"
## of PCT or Paris Convention. They can directly file applications in Mainland China, or they can also choose to file
## applications through PCT. Similarly, Chinese applicants can also file through PCT. Therefore, the variables "Chinese" and
## "PCT_app" are not mutually exclusive.

patents9 <- cSplit(patents8, "nation_province_code", ";", drop=F)
patents9 <- as.data.frame(patents9)

check.integer <- function(x){
  !grepl("[^[:digit:]]", format(x, scientific = F)) 
} # regex "[^[:digit:]]" matches all characters not digits 0-9, note the "!" (negate) before grepl


patents9$Chinese <- as.numeric(check.integer(patents9$nation_province_code_2))

#******************************************#
#******************************************#
#  9 -- priority & Divisional application  #
#        国内/国外优先权，分案申请         #
#******************************************#
#******************************************#

# 国内优先权：专利国内申请优先权，是指专利国内申请优先权人将自己的发明创造第一次在中国提出专利申请，
# 在规定的专利权期内，又以同一主题的发明创造向中国提出专利申请，依法所享有的优先权。有可能是研发进行
# 到一个阶段，先申请专利保护起来。国内优先权主要是便于优先权期间内技术方案的增加，为不同种类专利间的
# 转换提供条件

# 国外优先权：申请人自发明或者实用新型在外国第一次提出专利申请之日起十二个月内，或者自外观设计在
# 外国第一次提出专利申请之日起六个月内，又在中国就相同主题提出专利申请的，依照该外国同中国签订的
# 协议或者共同参加的国际条约，或者依照相互承认优先权的原则，可以享有优先权。有国外优先权的专利在
# 进入实质审查阶段专利年龄有可能很长，超出自申请日的3年，但我们仍然用原始的app_date，因为我们要
# 研究专利技术特征对专利授权的影响。

# 分案申请: 申请不符合单一要求时，审查员可要求分案申请，通常在专利授予前两个月左右。分案申请专利
# 有一个新的专利号，分案后母专利在数据库里就看不到了。即使是国外专利在分案后也不再有int_filing,
# int_pub, entry_date。分案申请可以是申请人主动提出，也可能是专利审查员要求申请人提出。
patents9$priority_d <- ifelse(!is.na(patents9$priority), 1, 0)
patents9$divisional_app <- ifelse(patents9$initial_app_num != 0, 1, 0) #“分案原申请号”不为空的专利是分案申请专利

#***********************************************#
#***********************************************#
# 10 --legal status -- 7 types of legal status  #
#***********************************************#
#***********************************************#

### parse out various legal status and the associated time 
patents10 <- cSplit(patents9, "legal_status", ";", drop = F)
legal_status_vec <- grep("legal_status_", names(patents10), value = TRUE)
patents10 <- cSplit(patents10, legal_status_vec, "#", drop = T)
legal_status_vec <- grep("legal_status_", names(patents10), value = TRUE) # update legal_status_vec

# create an empty matrix to store recoded legal status types ONLY (the dates do not change)
legal_status_matrix <- matrix(NA, nrow=nrow(patents10), ncol=length(legal_status_vec)) 
# name the collum names of the empty matrix
colnames(legal_status_matrix) <- paste0("legal_type_", 1:length(legal_status_vec))

# Create a new dataset by appending the empty matrix to the current dataset (i.e., patents2)
patents11 <- as.data.frame(cbind(patents10, legal_status_matrix))

legal_status_type <- grep("legal_type_", names(patents11), value = T)


# Define a function to recode legal status types
legal_status_f <- function(x, y) {
  x = as.character(x)
  y <- ifelse(is.na(x), -999,
       ifelse(grepl("实质审查的生效|实质审查请求的生效|实质审查请求已生效的专利申请|实质审查请求", x), 1,
       ifelse(grepl("发明专利申请公布后的视为撤回|发明专利申请公布后的撤回|专利申请的视为撤回|被视为撤回的申请|视为撤回的专利申请|申请的撤回|撤回的专利申请", x), 2,
       ifelse(grepl("发明专利申请公布后的驳回|驳回申请决定|专利申请的驳回|驳回的专利申请", x), 3,
       ifelse(grepl("专利申请或者专利权的恢复", x), 4,
       ifelse(grepl("授权|保密专利权授予", x), 5,
       ifelse(grepl("专利权的视为放弃|被视为放弃专利权的申请", x), 6,## These cases means the right has been granted, but the applicants didn't accept it (i.e., did not pay). Therefore, they should be regarded as being granted
       ifelse(grepl("专利权的终止|专利权的主动放弃", x), 7, 
       ifelse(grepl("^公开$", x), 8,
       ifelse(grepl("审定", x), 9,
       ifelse(grepl("[0-9]{4}.[0-9]{2}.[0-9]{2}",x), x, 88)))))))))))
  
}

patents11[legal_status_type] <- lapply(patents11[legal_status_vec], FUN = legal_status_f, y = patents11[legal_status_type])

patents12 <- as.data.frame(patents11)

#**********************************************#
#**********************************************#
#  11 --find the dates for each legal status   #
#**********************************************#
#**********************************************#

ind = as.numeric(ncol(patents10)) #set the start column of the following code
patents12$substantive_review<-NA #creat variable dummies to store data
patents12$app_withdraw <- NA
patents12$reject <- NA
patents12$recover <- NA
patents12$decision_made <- NA
patents12$grant <- NA
patents12$waive <- NA   ### waive means give up obtaining patent right, the applicant receives grant notification, but chooses not to follow up
patents12$terminate <- NA
patents12$pub_date_c <- NA ## calculated pub_date (date for 公开 in legal status). The original pub_date can be the date of 审定 or grant before 2006

n_col <- NROW(legal_status_type)/2 # Note the list is displayed as a column, we need to transpose it.
patents12 <- data.frame(lapply(patents12, as.character), stringsAsFactors=FALSE)
n_rows = nrow(patents12)

for (i in c(1:n_rows)){
  for (j in c(1:n_col)){ #change the number of the total columns of legal status without dummy
    if (!is.na(patents12[i,ind+j*2])){
      if (patents12[i,ind+j*2] == 1 & is.na(patents12$substantive_review[i])){ # we only use the first substantive_review date in the raw data
        patents12$substantive_review[i]<-patents12[i,ind+j*2-1]
      }
      if (patents12[i,ind+j*2] == 2){
        patents12$app_withdraw[i] <- patents12[i,ind+j*2-1]
      }
      if (patents12[i,ind+j*2] == 3){
        patents12$reject[i] <- patents12[i,ind+j*2-1]
      }
      if (patents12[i,ind+j*2] == 4){
        patents12$recover[i] <- patents12[i,ind+j*2-1]
      }
      if (patents12[i,ind+j*2] == 5 & is.na(patents12$grant[i])){
        patents12$grant[i] <- patents12[i,ind+j*2-1]
      }
      if (patents12[i,ind+j*2] == 6){
        patents12$waive[i] <- patents12[i,ind+j*2-1]
      }
      if (patents12[i,ind+j*2] == 7){
        patents12$terminate[i] <- patents12[i,ind+j*2-1]
      }
      if (patents12[i,ind+j*2] == 8){
        patents12$pub_date_c[i] <- patents12[i,ind+j*2-1]
      }
      if (patents12[i,ind+j*2] == 9){
        patents12$decision_made[i] <- patents12[i,ind+j*2-1]
      }
    }
  }
}

patents13 <- as.data.frame(patents12)

#### CHECK the data logic ######
check_unreasonable <- patents13[!is.na(patents13$grant) & !is.na(patents13$waive),] # these obs. are not reasonable, but should not affect our analysis
check_waive <- patents13[!is.na(patents13$substantive_review) & is.na(patents13$grant) & !is.na(patents13$waive),] # These obs. should be recoded. See the explanation of status 6

patents13$substantive_review <- as.Date(patents13$substantive_review, format = "%Y.%m.%d")
patents13$app_withdraw <- as.Date(patents13$app_withdraw, format = "%Y.%m.%d")
patents13$reject <- as.Date(patents13$reject, format = "%Y.%m.%d")
patents13$recover <- as.Date(patents13$recover, format = "%Y.%m.%d")
patents13$grant <- as.Date(patents13$grant, format = "%Y.%m.%d")
patents13$waive <- as.Date(patents13$waive, format = "%Y.%m.%d")
patents13$terminate <- as.Date(patents13$terminate, format = "%Y.%m.%d")
patents13$pub_date_c <- as.Date(patents13$pub_date_c, format = "%Y.%m.%d")
patents13$app_date <- as.Date(patents13$app_date, format = "%Y.%m.%d")
patents13$decision_made <- as.Date(patents13$decision_made, format = "%Y.%m.%d")

## Recode of grant date 1: if a patent has a waive date, but no grant date, no decision made date we regard it as a 
##           patent qualified for patent right, therefore, we need to consider it as granted, the grant date is roughtly  
##           two months before the waive date, according to China's Intellectual Property Law (i.e., 2 months without
##           paying the fee, the patents is deemed as waiving the right)
## Recode of grant date 2: if a patent has a decision made (审定) date and a terminate date OR a waive date, but no grant
##           date, we use the the decision_made date as the grant date.
## Recode of grant date 3: if a patent has no grant date, no decision_made date, no waive date, but a terminate date, 
##           there are three cases: 
##        a) there is a substantive review date, then the grant date is set to 12 months after the
##           substantive review date (as it's said on the internet that it normally takes 6-18 months after substantive
##           review for a patent to be granted). 
##        b) there is no substantive review date, but has a publication date, the grant date is set to 12 months after  
##           the publication date (i.e., pub_date_c), because if substantive review date is missing, we set it as
##           publication date. So here, it's essentially the same as a) option above.
##        c) there is no substnative review date, no publication date, then the grant date is set to 30 months 
##           [18 months (imputed durationfrom app_date to substantive review)
##           + 12months (imputed duration from substantive review to grant)] after application date(i.e., app_date)

patents13 <- patents13 %>%
  mutate(grant_c = 
        ifelse(!is.na(grant), grant,
        ifelse(is.na(grant) & !is.na(waive) & is.na(decision_made), waive %m-% months(2), 
        ifelse(is.na(grant) & !is.na(waive) & !is.na(decision_made), decision_made,
        ifelse(is.na(grant) & !is.na(terminate) & !is.na(decision_made), decision_made, 
        ifelse(is.na(grant) & is.na(decision_made) & is.na(waive) & !is.na(terminate) & !is.na(substantive_review), substantive_review %m+% months(12),
        ifelse(is.na(grant) & is.na(decision_made) & is.na(waive) & !is.na(terminate) & is.na(substantive_review) & !is.na(pub_date_c), pub_date_c %m+% months(12),
        ifelse(is.na(grant) & is.na(decision_made) & is.na(waive) & !is.na(terminate) & is.na(substantive_review) & is.na(pub_date_c), app_date %m+% months(30), NA))))))))

patents13$grant_c <- as.Date(patents13$grant_c, origin = "1970-01-01") 

## Recode of reject date: 
## there is a decision-made date, but no reject date, no grant_c date, no waive date, and no termination date, 
## and the patent status is 无效, the patent was rejected on the decision-made date. 
patents13 <- patents13 %>%
  mutate(reject_c =
        ifelse(!is.na(reject), reject,
        ifelse(is.na(reject) & is.na(grant_c) & is.na(waive) & is.na(terminate) & !is.na(decision_made) & patent_status_n == 2, decision_made, NA)))

patents13$reject_c <- as.Date(patents13$reject_c, origin = "1970-01-01") 

## Recode of appllication withdrawl date:
## Note: App withdrawl can happen before or after substantive review
## We only want app-withdraw cases with substantive review date!! 
patents13$subrev_withdraw_dur <- as.duration(patents13$substantive_review %--% patents13$app_withdraw)/dyears(1)*12
patents13 <- patents13 %>%
  mutate(app_withdraw_c = 
           ifelse(!is.na(substantive_review) & !is.na(app_withdraw) & subrev_withdraw_dur > 0, app_withdraw, NA))

### The relationship between the three events - grant, reject and withdraw
## grant can override reject and withdraw, reject can override withdraw, three cannot exist simultaneously.

patents13$reject_c <- ifelse(!is.na(patents13$grant_c) & !is.na(patents13$reject_c), NA, patents13$reject_c)
patents13$app_withdraw_c <- ifelse(!is.na(patents13$grant_c) & !is.na(patents13$app_withdraw_c), NA, 
                                   ifelse(is.na(patents13$grant_c) & !is.na(patents13$reject_c) & !is.na(patents13$app_withdraw_c), NA, patents13$app_withdraw_c))

patents13$reject_c <- as.Date(patents13$reject_c, origin = "1970-01-01") 
patents13$app_withdraw_c <- as.Date(patents13$app_withdraw_c, origin = "1970-01-01")


## Recode of substantive_review date 1: Deal with cases with grant_c or reject date, but no substantive review date.
## we impute the substantive review date as following: 
## 1) if there is a publication date in legal status and it is earlier than grant/reject date,
##    use publication date (i.e., pub_date_c, Note, not the pub_date in the raw data, as it can mean 
##    different things in early times)
## 2) if the publication date is later than grant/reject date, we set the substantive review date as 18 months 
##    after the app_date, as that is the mid-point of the period (i.e., 3 years) in which applicants can apply 
##    for substantive review. 
## 3) if no publication date, we also use app_date + 18months as the substantive review date.  
## Recode of substantive_review date 2: If the patent status is "在审", there's no substantive review date, but a publication
## date, substantive review date is set to pub_date_c. If no publicaion date, substantive review date is set to
## 18 months after the application date.
patents13$pub_grant_dur <- as.duration(patents13$pub_date_c %--% patents13$grant_c)/dyears(1)*12
patents13$pub_reject_dur <- as.duration(patents13$pub_date_c %--% patents13$reject_c)/dyears(1)*12
patents13 <- patents13 %>%
  mutate(substantive_review_c = 
        ifelse(!is.na(substantive_review), substantive_review,
        ifelse(is.na(substantive_review) & !is.na(grant_c) & !is.na(pub_date_c) & pub_grant_dur > 0, pub_date_c, 
        ifelse(is.na(substantive_review) & !is.na(reject_c) & !is.na(pub_date_c) & pub_reject_dur > 0, pub_date_c, 
        ifelse(is.na(substantive_review) & !is.na(grant_c) & !is.na(pub_date_c) & pub_grant_dur <= 0, app_date %m+% months(18),
        ifelse(is.na(substantive_review) & !is.na(reject_c) & !is.na(pub_date_c) & pub_reject_dur <= 0, app_date %m+% months(18),
        ifelse(is.na(substantive_review) & !is.na(grant_c) & is.na(pub_date_c), app_date %m+% months(18), 
        ifelse(is.na(substantive_review) & !is.na(reject_c)& is.na(pub_date_c), app_date %m+% months(18), 
        ifelse(is.na(substantive_review) & patent_status_n == 3 & !is.na(pub_date_c) , pub_date_c, 
        ifelse(is.na(substantive_review) & patent_status_n == 3 & is.na(pub_date_c), app_date %m+% months(18), NA))))))))))

patents13$substantive_review_c <- as.Date(patents13$substantive_review_c, origin = "1970-01-01")

#****************************************#
#****************************************#
##            12 -- Events              ##
#****************************************#
#****************************************#

## Construct Competing End Events: 0 = right censored, 4 = application does not get into reiew process
## Events: 1 = application withdraw (not paying exam fee 3 years after substantive review; applicant's behavior), 
## 2 = reject, 3 = grant
## application withdraw cannot coexist with grant or reject, if it does, grant or reject can override withdraw.

patents13$event <- ifelse(is.na(patents13$substantive_review_c), 4,
                   ifelse(!is.na(patents13$grant_c), 3, 
                   ifelse(!is.na(patents13$reject_c), 2,
                   ifelse(!is.na(patents13$app_withdraw_c), 1, 0))))

table(patents13$event)

## Find out the end event date and the right censored date (the observation period ends on 2017-12-31)
patents13$event_date <- ifelse(patents13$event == 1, patents13$app_withdraw_c,
                        ifelse(patents13$event == 2, patents13$reject_c,
                        ifelse(patents13$event ==3, patents13$grant_c, 
                        ifelse(patents13$event == 0, as.Date("2017-12-31", origin = "1970-01-01"), NA))))

patents13$event_date <- as.Date(patents13$event_date, origin = "1970-01-01") 

## If substantive_review_c is later than the event date (grant_c, reject_c, app_withdraw), or the same
## such observations are recoded as left censored, as it is difficult to make sense of them.
patents13$start_end_dur <- as.duration(patents13$substantive_review_c %--% patents13$event_date)/dyears(1)*12
patents13$event <- ifelse(!is.na(patents13$start_end_dur) & patents13$start_end_dur <= 0, 4, patents13$event)
table(patents13$event)

#*********$$$$$$$$*********%%%%%%%%**********@@@@@@@***********#
#*********$$$$$$$$*********%%%%%%%%**********@@@@@@@***********#
#*********$$$$$$$$*********%%%%%%%%**********@@@@@@@***********#
###  Drop splitted IPC_, IPCs_, legal_status_, legal_type    ###     
#*********$$$$$$$$*********%%%%%%%%**********@@@@@@@***********#
#*********$$$$$$$$*********%%%%%%%%**********@@@@@@@***********#
#*********$$$$$$$$*********%%%%%%%%**********@@@@@@@***********#
patents14 <- patents13[, -grep("IPC_|legal_type_|legal_status_", colnames(patents13))] 

#****************************************#
#****************************************#
##            13 -- Durations           ##
#****************************************#
#****************************************#

patents14$app_date <- as.Date(patents14$app_date,format = "%Y.%m.%d")
patents14$substantive_review_c <- as.Date(patents14$substantive_review_c,format = "%Y.%m.%d")
patents14$age_at_subreview <- as.duration(patents14$app_date %--% patents14$substantive_review_c)/dyears(1)*12
patents14$age_at_event <- as.duration(patents14$app_date %--% patents14$event_date)/dyears(1)*12



#**************************************************************#
#**************************************************************#
##  14 -- Convert all the data type to fit model requirement  ##
#**************************************************************#
#**************************************************************#
patents15 <- as.data.frame(patents14)
patents15$collaboration_applicant <- as.factor(patents15$collaboration_applicant) 
# encode the variable "collaboration" as a factor (categorical variable)
#levels(patents15$collaboration_applicant) <- c("no", "yes")
#change numeric data into factor with "0 = no; 1 = yes"
patents15$type1_company <- as.factor(patents15$type1_company)
#levels(patent15$type1_company) <- c("no", "yes")
patents15$type2_university <- as.factor(patents15$type2_university)
#levels(patents15$type2_university) <- c("no", "yes")
patents15$type3_individual <- as.factor(patents15$type3_individual)
#levels(patents15$type3_individual) <- c("no", "yes")
patents15$collaboration_inventer <- as.factor(patents15$collaboration_inventer) 
#levels(patents15$collaboration_inventer) <- c("no", "yes")
patents15$if_agency <- as.factor(patents15$if_agency)
#levels(patents15$if_agency) <- c("no", "yes")
patents15$paid_claims <- as.factor(patents15$paid_claims)
#levels(patents15$paid_claim) <- c("no", "yes")
patents15$patent_status_n <- as.factor(patents15$patent_status_n) # match 1, 2, 3 with "有效"， "无效"，"在审" in "patent_status"
#levels(patents15$patent_status_n) <- c("valid", "invalid", "under_examine")
patents15$Chinese <- as.factor(patents15$Chinese)
#levels(patents15$Chinese) <- c(0, 1)
patents15$event <- as.factor(patents15$event)
#levels(patents15$converted_events) <- c("Censored", "App_withdraw", "Rejected", "Qualified_granted", "Under_examine")
#patents15[, c(25:27,32,35:36)] <- sapply(patents15[, c(25:27,32,34:35)], as.numeric)
patents15$priority_d <- as.factor(patents15$priority_d)
patents15$divisional_app <- as.factor(patents15$divisional_app)

###################################################################
###################################################################
###################################################################
###################################################################

### Check the Data ####
### All the following should be empty #####!!!!
check_a <- patents15[!is.na(patents15$waive) & is.na(patents15$substantive_review_c),]
check_b <- patents15[!is.na(patents15$grant_c) & is.na(patents15$substantive_review_c),]
check_c <- patents15[!is.na(patents15$reject_c) & is.na(patents15$substantive_review_c),]
check_d <- patents15[!is.na(patents15$substantive_review_c) & !is.na(patents15$event_date),]
check_d <- check_d[check_d$event !=4 & (as.duration(check_d$substantive_review_c %--% check_d$event_date)/dyears(1)*12 <= 0),]
check_e <- patents15[patents15$event != 4 & patents15$start_end_dur <=0,]
check_f <- patents15[patents15$event == 0 & patents15$event_date != "2017-12-31",]
check_g <- patents15[patents15$event != 4 & is.na(patents15$substantive_review_c),]
check_h <- patents15[patents15$event == 4 & !is.na(patents15$substantive_review_c),] # this is possible as we recoded event for start_end_dur <= 0 cases.
check_i <- patents15[!is.na(patents13$substantive_review_c) & !is.na(patents13$app_withdraw) & !is.na(patents13$grant_c) & patents15$event == 1,]
check_j <- patents15[!is.na(patents13$substantive_review_c) & !is.na(patents13$app_withdraw) & !is.na(patents13$reject_c) & patents15$event == 1,]
check_k <- patents15[is.na(patents15$substantive_review_c) & patents15$event !=4, ]
check_l <- patents15[is.na(patents15$grant_c) & !is.na(patents15$waive),]
check_m <- patents15[!is.na(patents15$waive) & is.na(patents15$grant_c),]
check_n <- patents15[patents15$event !=4 & is.na(patents15$substantive_review_c),]
check_o <- patents15[!is.na(patents15$terminate) & patents15$event == 0, ]
check_p <- patents15[patents15$event == 0 & !is.na(patents15$decision_made), ]
check_q <- patents15[!is.na(patents15$grant_c) & !is.na(patents15$reject_c),]
check_r <- patents15[!is.na(patents15$grant_c) & !is.na(patents15$app_withdraw_c),]
check_s <- patents15[!is.na(patents15$reject_c) & !is.na(patents15$app_withdraw_c),]
check_t <- patents15[!is.na(patents15$age_at_subreview) & !is.na(patents15$age_at_event),]
check_t <- check_t[check_t$age_at_subreview < 0 | check_t$age_at_event < 0, ]

#*********$$$$$$$$*********%%%%%%%%**********@@@@@@@***********#
#*********$$$$$$$$*********%%%%%%%%**********@@@@@@@***********#
#*********$$$$$$$$*********%%%%%%%%**********@@@@@@@***********#
###     !!!!! DROP ALL LEFT CENSORED OBSERVATIONS !!!!!      ###
#*********$$$$$$$$*********%%%%%%%%**********@@@@@@@***********#
#*********$$$$$$$$*********%%%%%%%%**********@@@@@@@***********#
#*********$$$$$$$$*********%%%%%%%%**********@@@@@@@***********#

patents16 <- patents15[patents15$event != 4,]
write.csv(patents16,file = '/yaohanl/2017--Patent data/patent_UW/cleaned_data/cleaned_data.csv')
