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
#Kaplan Meier plot 
library(GGally)
library(timereg)#survival analysis
library(readxl)
library(lubridate)

pharmacy <- fread("/2017--Patent data/patent_UW/cleaned_data/pharmacy.csv", header = T, fill = F, encoding = "UTF-8")

## generate new variables
#PCT_app exclude Chinese app
#pharmacy$Chinese <- ifelse((pharmacy$PCT_app==1) & (pharmacy$Chinese == 1), 0, pharmacy$Chinese)

pharmacy$PCT_app <- ifelse((pharmacy$PCT_app==1) & (pharmacy$Chinese == 1), 0, pharmacy$PCT_app)
pharmacy$Paris <- ifelse((pharmacy$PCT_app == 0)& (pharmacy$Chinese == 0), 1, 0)
pharmacy$Chinese <- ifelse((pharmacy$PCT_app==0)&(pharmacy$Paris==0), 1, 0)
## paris: 8743, PCT: 37727, paris after joining PCT (1994.1.1): 7461
pharmacy$CN_priority <- ifelse((pharmacy$Chinese == 1) & (pharmacy$priority_d == 1), 1, 0)
#pharmacy$five_year_plan <- ifelse(pharmacy$app_date >= as.Date("2011.03.14", format = "%Y.%m.%d"), 1, 0)
## after the 12th five year plan: 175260
pharmacy$company_academic <- pharmacy$type1_company * pharmacy$type2_university

# add annual application 
frequency <- as.data.frame(table(pharmacy$app_year))
colnames(frequency) <- c("app_year", "annual_app")
pharmacy <- merge(frequency, pharmacy, by = "app_year")

pharmacy$total_inventer_one <- ifelse(pharmacy$total_inventers == 1, 1, 0)
pharmacy$total_applicant_one <- ifelse(pharmacy$total_applicant == 1, 1, 0)

pharmacy$Ln_total_inventers <- log(pharmacy$total_inventers)

pharmacy$Ln_tech_div <- pharmacy$tech_div
pharmacy$Sq_tech_div <- (pharmacy$tech_div)^2

pharmacy$tech_paid <- pharmacy$tech_div*pharmacy$paid_claims
pharmacy$tech_2_paid <- pharmacy$Sq_tech_div*pharmacy$paid_claims


pharmacy$IPCcount <- log(pharmacy$IPCcount)
pharmacy$annual_app <- log(pharmacy$annual_app)


#pharmacy$claims_20_lntech_div <- pharmacy$claims_20 * pharmacy$Ln_tech_div
#pharmacy$claims_20_sqtech_div <- pharmacy$claims_20 * pharmacy$Sq_tech_div

pharmacy$patent_law_V4 <- ifelse(pharmacy$app_date >= as.Date("2009.10.01", format = "%Y.%m.%d") & pharmacy$app_date < as.Date("2011.03.14", format = "%Y.%m.%d"), 1, 0)
pharmacy$patent_law_V5 <- ifelse(pharmacy$app_date >= as.Date("2011.03.14", format = "%Y.%m.%d"), 1, 0)


pharmacy$event_age <- pharmacy$age_at_event - pharmacy$age_at_subreview
pharmacy$cause <- pharmacy$event

################################################
## Non-linear inventers and linear inventers ###
################################################

comp_F_1 <- comp.risk(Event(event_age, cause) ~ const(Ln_total_inventers) + const(Sq_total_inventers) + const(total_inventer_one) + 
                         const(Ln_tech_div) + const(Sq_tech_div) + 
                         const(IPCcount) + const(total_claims) +
                         const(SingleIPC) + 
                         const(company_academic) + const(total_applicant_one) + 
                         const(PCT_app) + const(Paris) + 
                         const(if_agency) + const(CN_priority) + const(early_pub) + 
                         const(annual_app) + 
                         const(divisional_app) +
                         const(total_claims_one) + const(patent_law_V2) + const(patent_law_V3) + const(patent_law_V4) + 
                         const(patent_law_V5), data = pharmacy,cause = 3, model = "prop")
## test the effect with or without type3_individual
comp_F_2 <- comp.risk(Event(event_age, cause) ~ const(IPCcount) + const(SingleIPC) + const(total_claims_one) + 
                         const(company_academic) + const(total_applicant_one) + const(Ln_total_inventers) + const(total_inventer_one) + 
                         const(type3_individual) + const(PCT_app) + const(Paris) + const(if_agency) + const(CN_priority) +
                         const(early_pub) + const(divisional_app) + const(annual_app) + const(patent_law_V2) + const(patent_law_V3) + 
                         const(patent_law_V4) + const(patent_law_V5) + const(Ln_tech_div) + const(Sq_tech_div) + const(paid_claims) + 
                         const(tech_paid) + const(tech_2_paid),data = pharmacy, cause = 3, model = "prop")

comp_F_3 <- comp.risk(Event(event_age, cause) ~ const(IPCcount) + const(SingleIPC) + const(total_claims_one) + 
                         const(company_academic) + const(total_applicant_one) + const(Ln_total_inventers) + const(total_inventer_one) + 
                         const(type3_individual) + const(PCT_app) + const(Paris) + const(if_agency) + const(CN_priority) +
                         const(early_pub) + const(divisional_app) + const(annual_app) + const(patent_law_V2) + const(patent_law_V3) + 
                         const(patent_law_V4) + const(patent_law_V5) + const(Ln_tech_div) + const(Sq_tech_div) + const(paid_claims) + 
                         const(tech_paid) + const(tech_2_paid),data = pharmacy, cause = 2, model = "prop")

comp_F_4 <- comp.risk(Event(event_age, cause) ~ const(IPCcount) + const(SingleIPC) + const(total_claims_one) + 
                         const(company_academic) + const(total_applicant_one) + const(Ln_total_inventers) + const(total_inventer_one) + 
                         const(type3_individual) + const(PCT_app) + const(Paris) + const(if_agency) + const(CN_priority) +
                         const(early_pub) + const(divisional_app) + const(annual_app) + const(patent_law_V2) + const(patent_law_V3) + 
                         const(patent_law_V4) + const(patent_law_V5) + const(Ln_tech_div) + const(Sq_tech_div) + const(paid_claims) + 
                         const(tech_paid) + const(tech_2_paid),data = pharmacy, cause = 1, model = "prop")


const(Ln_tech_div) + const(Sq_tech_div) + const(paid_claims) + const(tech_paid) + const(tech_2_paid)

#Goodness of fit test
timereg::wald.test(comp_F_21)

