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

pharmacy <- read.csv("/2017--Patent data/patent_UW/cleaned_data/pharmacy.csv", header = T)



require(eha)
n <- max(pharmacy$event)
rs.tot <- risksets(Surv(pharmacy$age_at_subreview, pharmacy$age_at_event, pharmacy$event > 0.5))
haz.tot <- rs.tot$n.events/rs.tot$size
n.times <- length(haz.tot) + 1
S <- numeric(n.times)
S[1] <- 1
for(i in 2:n.times) S[i] <- S[i-1] *(1 - haz.tot[i-1])
  
haz <- matrix(0, nrow = n, ncol = length(haz.tot))
P <- matrix(0, nrow = n, ncol = length(haz.tot) + 1)
for(row in 1:n){
  rs <- risksets(Surv(pharmacy$age_at_subreview, pharmacy$age_at_event, pharmacy$event == row))
  haz.row <- rs$n.events/rs$size
  tmp <- 0
  cols <- which(rs.tot$risktimes %in% rs$risktimes)
  haz[row, cols] <- haz.row
  P[row, 2:NCOL(P)] <- cumsum(S[1:(n.times -1)] * haz[row, ])
}
    
O <- t(P)
X <- as.data.frame(rs.tot$risktimes)
X <- rbind(0, X)

All <- cbind(X, S, O)
colnames(All) <- c("Risktimes", "Pending", "Withdrawal", "Rejection", "Grant")
    
All <- as.data.frame(All)
melt_tb <- melt(All, id="Risktimes")  # convert to long format
colnames(melt_tb) <- c("Duration_months", "Variables", "Proportion")

ggplot(melt_tb, aes(x= Duration_months, y=Proportion, colour=Variables)) + 
  geom_line(linetype = 1) +
  # use shape to change the point type and when shape = variable point shape will be changed based on variables
  geom_point(size=1, shape=16) + 
  theme_bw() + 
  theme(legend.text = element_text(colour="black", size = 15)) + 
  theme(legend.title = element_text(colour="black", size=15)) + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) + 
  theme(legend.position = c(0.9, 0.8)) + 
  guides(colour = guide_legend(title = NULL)) + 
  scale_x_continuous(breaks=seq(from=0,to=max(melt_tb$Duration_months, na.rm = T), by=12), expand = c(0, 0)) + 
  scale_y_continuous(breaks=seq(from=0,to=max(melt_tb$Proportion, na.rm = T), by=0.1)) 



