# PRELIMINARY PROCEDURES

# Set working directory
setwd("~/Desktop/Lab Stuff/Justin's Thesis/Spreadsheets")
getwd()

# Load required packages
library(robustfa) # Don't think I need this one actually
library(beeswarm)
library(sm)
library(car)
library(ppcor)
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
library(gridExtra)

#-------------------------------------------------------------------------------------

# LOADING AND PREPARING THE DATA

# Reading spreadsheets into R, and subsetting out empty rows 
Give_N <- read.csv("~/Desktop/Lab Stuff/Justin's Thesis/Spreadsheets/GiveN_JS_200219_ForThesis.csv", na.strings="N/A")
PPVT <- read.csv("~/Desktop/Lab Stuff/Justin's Thesis/Spreadsheets/PPVT_Data_JS_200224_ForThesis.csv", na.strings="N/A")
PPVT[3,29] <- 95
FullData <- merge(Give_N, PPVT, by.x="SUBJECT.ID", by.y="SUBJECT.ID")
FullData <- subset(FullData, FullData$Including.in.Study. == "Yes" & FullData$Age.x != "" & FullData$Raw_Score != "" & FullData$GN_All_Ceiling_Conservative != "")

# Checking if Date Tested matches for each ID on each spreadsheet (verification purposes) - all match, checked on 09/23/2020
Date.Tested_check <- data.frame(FullData$Date.Tested.x,FullData$Date.Tested.y)
Date.Tested_check$check <- ifelse(Date.Tested_check$FullData.Date.Tested.x==Date.Tested_check$FullData.Date.Tested.y,1,0)
nrow(Date.Tested_check[Date.Tested_check$check==0,])  

# Checking if DOB matches for each ID on each spreadsheet (verification purposes)
DOB_check <- data.frame(FullData$DOB,FullData$Birthdate)
DOB_check$check <- ifelse(DOB_check$FullData.DOB==DOB_check$FullData.Birthdate,1,0)
nrow(DOB_check[DOB_check$check==0,])  

# Checking if Age matches for each ID on each spreadsheet (verification purposes)
Age_check <- data.frame(FullData$Age.x,FullData$Age.y)
Age_check$check <- ifelse(Age_check$FullData.Age.x==Age_check$FullData.Age.y,1,0)
nrow(Age_check[Age_check$check==0,])

# Checking if Sex matches for each ID on each spreadsheet (verification purposes)
Sex_check <- data.frame(FullData$Sex.x,FullData$Sex.y)
Sex_check$check <- ifelse(Sex_check$FullData.Sex.x==Sex_check$FullData.Sex.y,1,0)
nrow(Sex_check[Sex_check$check==0,])

# Checking if Including in Study matches for each ID on each spreadsheet (verification purposes)
Inc_check <- data.frame(FullData$Including.in.Study.,FullData$Including_in_study)
Inc_check$check <- ifelse(Inc_check$FullData.Including.in.Study.==Inc_check$FullData.Including_in_study,1,0)
nrow(Inc_check[Inc_check$check==0,])

# Checking if SES matches for each ID on each spreadsheet (verification purposes)
SES_check <- data.frame(FullData$SES..8.66.,FullData$SES_Score)
SES_check$check <- ifelse(SES_check$FullData.SES..8.66.==SES_check$FullData.SES_Score,1,0)
nrow(SES_check[SES_check$check==0,]) # 7 mismatches, but all due to missing data. PPVT SES (FullData$SES_Score) accurate with BGQ but still has 3 missing values (HBE004,HBE010,HBE021)

# Checking if Language Tested matches for each ID on each spreadsheet (verification purposes)
LT_check <- data.frame(FullData$SUBJECT.ID,FullData$Child_LanguageTested,FullData$Language_Tested)
LT_check$check <- ifelse(LT_check$FullData.Child_LanguageTested==LT_check$FullData.Language_Tested,1,0)
nrow(LT_check[LT_check$check==0,]) 

# Delete duplicate columns
FullData[c(2,3,4,5,6,10,12)] <- list(NULL) # Remove Give N versions of duplicates, **DO NOT USE, delets columns used later for analyses (like "Age.x"). Need to revise to delete "___.y" type columns

# Check the structure and data type of each column
str(FullData)

#-------------------------------------------------------------------------------------

# CALCULATING THE DEMOGRAPHICS

# Calculating participants included
nrow(FullData[FullData$Including.in.Study. == "Yes",]) # 47 included
nrow(FullData[FullData$Including.in.Study. == "No",]) # 0 not included

# Calculating sex demographics
nrow(FullData[FullData$Sex.x=="Female",]) # 31 females
nrow(FullData[FullData$Sex.x=="Male",]) # 16 males

# Calculating age demographics
mean(FullData$Age.x)
sd(FullData$Age.x)
min(FullData$Age.x)
max(FullData$Age.x)

# Calculating SES demographics 
nrow(FullData[FullData$SES_Score=="",]) # 3 rows w/ N/A values (HBE004,HBE010,HBE021)
mean(FullData$SES_Score,na.rm=TRUE)
sd(FullData$SES_Score,na.rm=TRUE)
min(FullData$SES_Score,na.rm=TRUE)
max(FullData$SES_Score,na.rm=TRUE)

#-------------------------------------------------------------------------------------

# ***START OF ANALYSES***

# Subset by the two age groups we defined in analysis plan
SarneckaAge <-  subset(FullData, FullData$Age.x < 4.75) # 26 kids between 2.5 and 4.75
HornburgAge <- subset(FullData, FullData$Age.x < 6.03 & FullData$Age.x > 3.5) # 42 kids between 3.5 and 6.03
AnsariAge <- subset(FullData, FullData$Age.x < 5.25 & FullData$Age.x > 2.5) # 38 kids between 2.5 and 5.25

# Reading Hornburg specific spreadsheet for Hornburg analyses and subsetting
Hornburg_Load <- read.csv("~/Desktop/Lab Stuff/Thesis/Spreadsheets/GN_sm_1sttrial_GN_lg_JS_thesis_200511.csv", na.strings="N/A")
FD_Hornburg_Proced <- merge(Hornburg_Load, PPVT, by.x="SUBJECT.ID", by.y="SUBJECT.ID")
FD_Hornburg_Proced <- subset(FD_Hornburg_Proced, FD_Hornburg_Proced$Including.in.Study. == "Yes" & FD_Hornburg_Proced$Age.x != "" & FD_Hornburg_Proced$Raw_Score != "" & FD_Hornburg_Proced$Hornburg.Proportion.Correct != "" & FD_Hornburg_Proced$Total.Number.Trials.Completed==6)
HA_Hornburg_Proced <- subset(FD_Hornburg_Proced,FD_Hornburg_Proced$Age.x < 6.03 & FD_Hornburg_Proced$Age.x > 3.5)

#--------------------------------------------------------------------------------------

# AGE DENSITY PLOTS 

# Age density curves for the age groups made using built in plot functions
opar <- par(no.readonly = TRUE)
plot(density(SarneckaAge$Age.x, na.rm=TRUE,bw=0.6),xlim=c(3,8),ylim=c(0,0.6),col=2,lty=1,lwd=3,xlab="Age (Years)",main="Age Density Curves for the Age Group Subsets")
par(new=TRUE)
plot(density(HornburgAge$Age.x, na.rm=TRUE,bw=0.6),xlim=c(3,8),ylim=c(0,0.6),col=3,lty=2,lwd=3,xlab="Age (Years)",main="Age Density Curves for the Age Group Subsets")
par(new=TRUE)
plot(density(AnsariAge$Age.x, na.rm=TRUE,bw=0.6),xlim=c(3,8),ylim=c(0,0.6),col=4,lty=3,lwd=3,xlab="Age (Years)",main="Age Density Curves for the Age Group Subsets")
par(new=TRUE)
plot(density(FullData$Age.x, na.rm=TRUE,bw=0.6),xlim=c(3,8),ylim=c(0,0.6),col=5,lty=4,lwd=3,xlab="Age (Years)",main="Age Density Curves for the Age Group Subsets")
legend(5.8,0.51, title="Age Group Subsets",legend=c("Sarnecka Age Group (2.5 to 4.75)","Hornburg Age Group (3.5 to 6.03)","Ansari Age Group (2.5 to 5.25)","Full Age Group (3.0 to 6.5)"),col=c(2,3,4,5),lty=c(1,2,3,4),cex=0.75)
par(opar)

# Age density curves for the age groups made using ggplot2
ggplot(SarneckaAge, aes(x=Age.x,y=..count..,color="blue",kernel="rectangular")) +
  geom_density() +
  geom_density(data=HornburgAge, aes(x=Age.x,y=..count..,color="darkgreen")) +
  geom_density(data=AnsariAge, aes(x=Age.x,y=..count..,color="purple")) +
  geom_density(data=FullData, aes(x=Age.x,y=..count..,color="red")) +
  ggtitle("Age Density Curves by Age Groups") +
  xlab("Age (years)") + 
  ylab("Density") +
  scale_colour_manual(name = "Age Groups", 
    values =c("blue"="blue", "darkgreen"="darkgreen", "purple"="purple","red"="red"),     labels = c("Sarnecka Age Group (2.5 to 4.75)","Hornburg Age Group (3.5 to 6.03)" ,             "Ansari Age Group (2.5 to 5.25)" ,"Full Age Group (3.0 to 6.5)")) +
  theme(legend.position = c(0.8, 0.8))
# Figure out how to adjust kernel
# Sarnecka Data significantly younger, smaller # of participants in this range relative to others in our data and the peak of the Sarnecka data is younger

# Density Curve for Full Data Age group ages only
ggplot(FullData, aes(x=Age.x,y=..count..,kernel="rectangular")) +
  geom_density(color="black", fill="red") + 
  ggtitle("Participant Age Density Curve") + 
  xlab("Age (Years)") + 
  ylab("Frequency (Count)") 
# Figure out how to adjust kernel

# Histogram for Full Data Age
ggplot(FullData, aes(x=Age.x)) + 
  geom_histogram(breaks=seq(3.00,6.50,by=0.499999), col="black",fill="red") +
  ggtitle("Participant Age Histogram") +
  theme(plot.title=element_text(hjust=0.5,face="bold",size = 22),
        axis.text=element_text(size=14),axis.title=element_text(size=16)) +
  scale_y_continuous(name="Frequency (Count)", limits=c(0, 13)) +
  xlab("Age (Years)") 
# Sarnecka Data significantly younger, smaller # of participants in Sarnecka Age range relative to others in our data and the peak of the Sarnecka data is younger
# Make all histograms individually, and also make them better than this one

# Matrix of histograms to visualize the count of kids in each age group, need to improves these (titles)
SA_AgeHist <- ggplot(SarneckaAge, aes(x=Age.x)) + 
  geom_histogram(breaks=seq(2.50,6.50,by=0.499999), col="black",fill="forestgreen") +
  scale_y_continuous(name="Count", limits=c(0, 13))
HA_AgeHist <- ggplot(HornburgAge, aes(x=Age.x)) + 
  geom_histogram(breaks=seq(2.50,6.50,by=0.499999), col="black",fill="red") +
  scale_y_continuous(name="Count", limits=c(0, 13))
AA_AgeHist <- ggplot(AnsariAge, aes(x=Age.x)) + 
  geom_histogram(breaks=seq(2.50,6.50,by=0.499999), col="black",fill="blue") +
  scale_y_continuous(name="Count", limits=c(0, 13))
FD_AgeHist <- ggplot(FullData, aes(x=Age.x)) + 
  geom_histogram(breaks=seq(2.50,6.50,by=0.499999), col="black",fill="gold") +
  scale_y_continuous(name="Count", limits=c(0, 13))
grid.arrange(SA_AgeHist,HA_AgeHist,AA_AgeHist,FD_AgeHist,nrow=2)
# do 3x2 matrix comparing histograms of counts in age bins of our age groups with matching studies

# Table of number of kids broken down by age into age bins for each study age group
ABF_rnames <- c("Age Frequencies for Sarnecka Age Group","Age Frequencies for Hornburg Age Group","Age Frequencies for Ansari Age Group","Age Frequencies for Full Data Age Group")
ABF_cnames <- c("2.50-2.99","3.00-3.49","3.50-3.99","4.00-4.49","4.50-4.99","5.00-5.49","5.50-5.99","6.00-6.50", "Total")
AgeBin_Freq <- matrix(c(
  nrow(SarneckaAge[SarneckaAge$Age.x >= 2.50 & SarneckaAge$Age.x <= 2.99,]),
  nrow(SarneckaAge[SarneckaAge$Age.x >= 3.00 & SarneckaAge$Age.x <= 3.49,]),
  nrow(SarneckaAge[SarneckaAge$Age.x >= 3.50 & SarneckaAge$Age.x <= 3.99,]),
  nrow(SarneckaAge[SarneckaAge$Age.x >= 4.00 & SarneckaAge$Age.x <= 4.49,]),
  nrow(SarneckaAge[SarneckaAge$Age.x >= 4.50 & SarneckaAge$Age.x <= 4.99,]),
  nrow(SarneckaAge[SarneckaAge$Age.x >= 5.00 & SarneckaAge$Age.x <= 5.49,]),
  nrow(SarneckaAge[SarneckaAge$Age.x >= 5.50 & SarneckaAge$Age.x <= 5.99,]),
  nrow(SarneckaAge[SarneckaAge$Age.x >= 6.00 & SarneckaAge$Age.x <= 6.50,]),
  nrow(SarneckaAge),
  nrow(HornburgAge[HornburgAge$Age.x >= 2.50 & HornburgAge$Age.x <= 2.99,]),
  nrow(HornburgAge[HornburgAge$Age.x >= 3.00 & HornburgAge$Age.x <= 3.49,]),
  nrow(HornburgAge[HornburgAge$Age.x >= 3.50 & HornburgAge$Age.x <= 3.99,]),
  nrow(HornburgAge[HornburgAge$Age.x >= 4.00 & HornburgAge$Age.x <= 4.49,]),
  nrow(HornburgAge[HornburgAge$Age.x >= 4.50 & HornburgAge$Age.x <= 4.99,]),
  nrow(HornburgAge[HornburgAge$Age.x >= 5.00 & HornburgAge$Age.x <= 5.49,]),
  nrow(HornburgAge[HornburgAge$Age.x >= 5.50 & HornburgAge$Age.x <= 5.99,]),
  nrow(HornburgAge[HornburgAge$Age.x >= 6.00 & HornburgAge$Age.x <= 6.50,]),
  nrow(HornburgAge),
  nrow(AnsariAge[AnsariAge$Age.x >= 2.50 & AnsariAge$Age.x <= 2.99,]),
  nrow(AnsariAge[AnsariAge$Age.x >= 3.00 & AnsariAge$Age.x <= 3.49,]),
  nrow(AnsariAge[AnsariAge$Age.x >= 3.50 & AnsariAge$Age.x <= 3.99,]),
  nrow(AnsariAge[AnsariAge$Age.x >= 4.00 & AnsariAge$Age.x <= 4.49,]),
  nrow(AnsariAge[AnsariAge$Age.x >= 4.50 & AnsariAge$Age.x <= 4.99,]),
  nrow(AnsariAge[AnsariAge$Age.x >= 5.00 & AnsariAge$Age.x <= 5.49,]),
  nrow(AnsariAge[AnsariAge$Age.x >= 5.50 & AnsariAge$Age.x <= 5.99,]),
  nrow(AnsariAge[AnsariAge$Age.x >= 6.00 & AnsariAge$Age.x <= 6.50,]),
  nrow(AnsariAge),
  nrow(FullData[FullData$Age.x >= 2.50 & FullData$Age.x <= 2.99,]),
  nrow(FullData[FullData$Age.x >= 3.00 & FullData$Age.x <= 3.49,]),
  nrow(FullData[FullData$Age.x >= 3.50 & FullData$Age.x <= 3.99,]),
  nrow(FullData[FullData$Age.x >= 4.00 & FullData$Age.x <= 4.49,]),
  nrow(FullData[FullData$Age.x >= 4.50 & FullData$Age.x <= 4.99,]),
  nrow(FullData[FullData$Age.x >= 5.00 & FullData$Age.x <= 5.49,]),
  nrow(FullData[FullData$Age.x >= 5.50 & FullData$Age.x <= 5.99,]),
  nrow(FullData[FullData$Age.x >= 6.00 & FullData$Age.x <= 6.50,]),
  nrow(FullData)),
  nrow=4, ncol=9, byrow=TRUE, dimnames=list(ABF_rnames,ABF_cnames))
View(AgeBin_Freq)
# do matrix of histograms to visualize the count of kids in each age group, maybe do 3x2 matrix comparing histograms of counts in age bins of our age groups with matching studies, and then full data age bin count histrogram on its own?

# KL FREQUENCY DISTRIBUTION TABLE

# Table of frequency counts of knower levels for each age group - can find in Results/Discussion section in thesis
KLF_rnames <- c("Quantity of Sarnecka Age Kids","Quantity of Hornburg Age Kids","Quantity of Ansari Age Kids","Quantity of Full Data Age Kids","Proportion of Sarnecka Age Kids","Proportion of Hornburg Age Kids","Proportion of Ansari Age Kids","Proportion of Full Data Age Kids")
KLF_cnames <- c("Pre-knowers","1-knowers","2-knowers","3-knowers","4-knowers","5-knowers","6-knowers")
KnowerLevel_Freq <- matrix(c(format(round(nrow(SarneckaAge[SarneckaAge$GN_Small_Ceiling._FinalSet_conservative=="0",]))),
  nrow(SarneckaAge[SarneckaAge$GN_Small_Ceiling._FinalSet_conservative=="1",]),
  nrow(SarneckaAge[SarneckaAge$GN_Small_Ceiling._FinalSet_conservative=="2",]),
  nrow(SarneckaAge[SarneckaAge$GN_Small_Ceiling._FinalSet_conservative=="3",]),
  nrow(SarneckaAge[SarneckaAge$GN_Small_Ceiling._FinalSet_conservative=="4",]),
  nrow(SarneckaAge[SarneckaAge$GN_Small_Ceiling._FinalSet_conservative=="5",]),
  nrow(SarneckaAge[SarneckaAge$GN_Small_Ceiling._FinalSet_conservative=="6",]),
  nrow(HornburgAge[HornburgAge$GN_Small_Ceiling._FinalSet_conservative=="0",]),
  nrow(HornburgAge[HornburgAge$GN_Small_Ceiling._FinalSet_conservative=="1",]),
  nrow(HornburgAge[HornburgAge$GN_Small_Ceiling._FinalSet_conservative=="2",]),
  nrow(HornburgAge[HornburgAge$GN_Small_Ceiling._FinalSet_conservative=="3",]),
  nrow(HornburgAge[HornburgAge$GN_Small_Ceiling._FinalSet_conservative=="4",]),
  nrow(HornburgAge[HornburgAge$GN_Small_Ceiling._FinalSet_conservative=="5",]),
  nrow(HornburgAge[HornburgAge$GN_Small_Ceiling._FinalSet_conservative=="6",]),
  nrow(AnsariAge[AnsariAge$GN_Small_Ceiling._FinalSet_conservative=="0",]),
  nrow(AnsariAge[AnsariAge$GN_Small_Ceiling._FinalSet_conservative=="1",]),
  nrow(AnsariAge[AnsariAge$GN_Small_Ceiling._FinalSet_conservative=="2",]),
  nrow(AnsariAge[AnsariAge$GN_Small_Ceiling._FinalSet_conservative=="3",]),
  nrow(AnsariAge[AnsariAge$GN_Small_Ceiling._FinalSet_conservative=="4",]),
  nrow(AnsariAge[AnsariAge$GN_Small_Ceiling._FinalSet_conservative=="5",]),
  nrow(AnsariAge[AnsariAge$GN_Small_Ceiling._FinalSet_conservative=="6",]),
  nrow(FullData[FullData$GN_Small_Ceiling._FinalSet_conservative=="0",]),
  nrow(FullData[FullData$GN_Small_Ceiling._FinalSet_conservative=="1",]),
  nrow(FullData[FullData$GN_Small_Ceiling._FinalSet_conservative=="2",]),
  nrow(FullData[FullData$GN_Small_Ceiling._FinalSet_conservative=="3",]),
  nrow(FullData[FullData$GN_Small_Ceiling._FinalSet_conservative=="4",]),
  nrow(FullData[FullData$GN_Small_Ceiling._FinalSet_conservative=="5",]),
  nrow(FullData[FullData$GN_Small_Ceiling._FinalSet_conservative=="6",]),
  format(round(nrow(SarneckaAge[SarneckaAge$GN_Small_Ceiling._FinalSet_conservative=="0",])/26,2),nsmall=2),
  format(round(nrow(SarneckaAge[SarneckaAge$GN_Small_Ceiling._FinalSet_conservative=="1",])/26,2),nsmall=2),
  format(round(nrow(SarneckaAge[SarneckaAge$GN_Small_Ceiling._FinalSet_conservative=="2",])/26,2),nsmall=2),
  format(round(nrow(SarneckaAge[SarneckaAge$GN_Small_Ceiling._FinalSet_conservative=="3",])/26,2),nsmall=2),
  format(round(nrow(SarneckaAge[SarneckaAge$GN_Small_Ceiling._FinalSet_conservative=="4",])/26,2),nsmall=2),
  format(round(nrow(SarneckaAge[SarneckaAge$GN_Small_Ceiling._FinalSet_conservative=="5",])/26,2),nsmall=2),
  format(round(nrow(SarneckaAge[SarneckaAge$GN_Small_Ceiling._FinalSet_conservative=="6",])/26,2),nsmall=2),
  format(round(nrow(HornburgAge[HornburgAge$GN_Small_Ceiling._FinalSet_conservative=="0",])/42,2),nsmall=2),
  format(round(nrow(HornburgAge[HornburgAge$GN_Small_Ceiling._FinalSet_conservative=="1",])/42,2),nsmall=2),
  format(round(nrow(HornburgAge[HornburgAge$GN_Small_Ceiling._FinalSet_conservative=="2",])/42,2),nsmall=2),
  format(round(nrow(HornburgAge[HornburgAge$GN_Small_Ceiling._FinalSet_conservative=="3",])/42,2),nsmall=2),
  format(round(nrow(HornburgAge[HornburgAge$GN_Small_Ceiling._FinalSet_conservative=="4",])/42,2),nsmall=2),
  format(round(nrow(HornburgAge[HornburgAge$GN_Small_Ceiling._FinalSet_conservative=="5",])/42,2),nsmall=2),
  format(round(nrow(HornburgAge[HornburgAge$GN_Small_Ceiling._FinalSet_conservative=="6",])/42,2),nsmall=2),
  format(round(nrow(AnsariAge[AnsariAge$GN_Small_Ceiling._FinalSet_conservative=="0",])/38,2),nsmall=2),
  format(round(nrow(AnsariAge[AnsariAge$GN_Small_Ceiling._FinalSet_conservative=="1",])/38,2),nsmall=2),
  format(round(nrow(AnsariAge[AnsariAge$GN_Small_Ceiling._FinalSet_conservative=="2",])/38,2),nsmall=2),
  format(round(nrow(AnsariAge[AnsariAge$GN_Small_Ceiling._FinalSet_conservative=="3",])/38,2),nsmall=2),
  format(round(nrow(AnsariAge[AnsariAge$GN_Small_Ceiling._FinalSet_conservative=="4",])/38,2),nsmall=2),
  format(round(nrow(AnsariAge[AnsariAge$GN_Small_Ceiling._FinalSet_conservative=="5",])/38,2),nsmall=2),
  format(round(nrow(AnsariAge[AnsariAge$GN_Small_Ceiling._FinalSet_conservative=="6",])/38,2),nsmall=2),
  format(round(nrow(FullData[FullData$GN_Small_Ceiling._FinalSet_conservative=="0",])/47,2),nsmall=2),
  format(round(nrow(FullData[FullData$GN_Small_Ceiling._FinalSet_conservative=="1",])/47,2),nsmall=2),
  format(round(nrow(FullData[FullData$GN_Small_Ceiling._FinalSet_conservative=="2",])/47,2),nsmall=2),
  format(round(nrow(FullData[FullData$GN_Small_Ceiling._FinalSet_conservative=="3",])/47,2),nsmall=2),
  format(round(nrow(FullData[FullData$GN_Small_Ceiling._FinalSet_conservative=="4",])/47,2),nsmall=2),
  format(round(nrow(FullData[FullData$GN_Small_Ceiling._FinalSet_conservative=="5",])/47,2),nsmall=2),
  format(round(nrow(FullData[FullData$GN_Small_Ceiling._FinalSet_conservative=="6",])/47,2),nsmall=2)),
  nrow=8, ncol=7, byrow=TRUE, dimnames=list(KLF_rnames,KLF_cnames))
View(KnowerLevel_Freq)
# Do alternating totals and proportions for each study
# Add a total column? (if not already there)

# Another way to make the table - Outdated as of 04/24/20
print(table(SarneckaAge$GN_Small_Ceiling._FinalSet_conservative), zero.print=".")
print(table(HornburgAge$GN_Small_Ceiling._FinalSet_conservative), zero.print=".")
SarneckaAge$GN_SM_Fac <- factor(as.character(SarneckaAge$GN_Small_Ceiling._FinalSet_conservative), levels=c(0:6))
HornburgAge$GN_SM_Fac <- factor(as.character(HornburgAge$GN_Small_Ceiling._FinalSet_conservative), levels=c(0:6))
FullData$GN_SM_Fac <- factor(as.character(FullData$GN_Small_Ceiling._FinalSet_conservative),levels=c(0:6))
table(SarneckaAge$GN_SM_Fac)
table(HornburgAge$GN_SM_Fac)
prop.table(table(SarneckaAge$GN_SM_Fac))
prop.table(table(HornburgAge$GN_SM_Fac))

#-------------------------------------------------------------------------------------

# PLOTS OF AGE VS GIVE N SMALL (INTERVAL AND PROPORTION) FOR ALL AGE GROUPS

# Make all plots using ggplot

# Beeswarm plots of Age vs. Give-N small ordinal for all age groups
SarneckaAge$GN_SM_Fac <- factor(as.character(SarneckaAge$GN_Small_Ceiling._FinalSet_conservative), levels=c(0:6))
HornburgAge$GN_SM_Fac <- factor(as.character(HornburgAge$GN_Small_Ceiling._FinalSet_conservative), levels=c(0:6))
FullData$GN_SM_Fac <- factor(as.character(FullData$GN_Small_Ceiling._FinalSet_conservative),levels=c(0:6))
beeswarm(SarneckaAge$Age.x ~ SarneckaAge$GN_SM_Fac)
beeswarm(HornburgAge$Age.x ~ HornburgAge$GN_SM_Fac)
beeswarm(FullData$Age.x ~ FullData$GN_SM_Fac)
# Good plots to go along with KL-frequency table, shows concentration of kids in knower level categories for the different age groups, beginning of results and discussion

# **General Note: set height and width of jitter for consistency in plots
# Scatterplots of Sarnecka Age vs. Give-N small 
# Interval 
plot(SarneckaAge$GN_Small_Ceiling._FinalSet_conservative ~ jitter(SarneckaAge$Age.x))
lines(lowess(SarneckaAge$Age.x,SarneckaAge$GN_Small_Ceiling._FinalSet_conservative),col="orange")
ggplot(SarneckaAge, aes(x=Age.x, y=GN_Small_Ceiling._FinalSet_conservative)) +
  geom_jitter() +
  ggtitle("Sarnecka Age: Age vs. Interval GN Small") +
  geom_smooth(method = "loess",se=FALSE)
#Proportion
plot(SarneckaAge$Proportion.Correct..Give.N.small. ~ jitter(SarneckaAge$Age.x))
lines(lowess(SarneckaAge$Age.x,SarneckaAge$Proportion.Correct..Give.N.small.),col="blue")
ggplot(SarneckaAge, aes(x=Age.x, y=Proportion.Correct..Give.N.small.)) +
  geom_jitter() +
  ggtitle("Sarnecka Age: Age vs. Continuous GN Small") +
  geom_smooth(method = "loess",se=FALSE)
# Lowess curves look different between two different methods of making plots

# Scatterplots of Hornburg Age vs. Give-N small
# Interval
plot(HornburgAge$GN_Small_Ceiling._FinalSet_conservative ~ jitter(HornburgAge$Age.x))
lines(lowess(HornburgAge$Age.x,HornburgAge$GN_Small_Ceiling._FinalSet_conservative),col="yellow")
ggplot(HornburgAge, aes(x=Age.x, y=GN_Small_Ceiling._FinalSet_conservative)) +
  geom_jitter() +
  ggtitle("Hornburg Age: Age vs. Interval GN Small") +
  geom_smooth(method = "loess",se=FALSE)
# Proportion
plot(HornburgAge$Proportion.Correct..Give.N.small. ~ jitter(HornburgAge$Age.x))
lines(lowess(HornburgAge$Age.x,HornburgAge$Proportion.Correct..Give.N.small.),col="red") # Is this correct? I believe so, refer to the jitter, suprised there's no slope to start though. Points do seem to match up with the ggplot graph below
ggplot(HornburgAge, aes(x=Age.x, y=Proportion.Correct..Give.N.small.)) +
  geom_jitter() +
  ggtitle("Hornburg Age: Age vs. Continuous GN Small") +
  geom_smooth(method = "loess",se=FALSE)

# Scatterplots of Ansari Age vs. Give-N small
# Interval
plot(AnsariAge$GN_Small_Ceiling._FinalSet_conservative ~ jitter(AnsariAge$Age.x))
lines(lowess(AnsariAge$Age.x,AnsariAge$GN_Small_Ceiling._FinalSet_conservative),col="midnightblue")
ggplot(AnsariAge, aes(x=Age.x, y=GN_Small_Ceiling._FinalSet_conservative)) +
  geom_jitter() +
  ggtitle("Ansari Age: Age vs. Interval GN Small") +
  geom_smooth(method = "loess",se=FALSE)
# Proportion
plot(AnsariAge$Proportion.Correct..Give.N.small. ~ jitter(AnsariAge$Age.x))
lines(lowess(AnsariAge$Age.x,AnsariAge$Proportion.Correct..Give.N.small.),col="plum4")
ggplot(AnsariAge, aes(x=Age.x, y=Proportion.Correct..Give.N.small.)) +
  geom_jitter() +
  ggtitle("Ansari Age: Age vs. Continuous GN Small") +
  geom_smooth(method = "loess",se=FALSE)

# Scatterplots of Full Data Age vs. Give-N small
# Interval
plot(FullData$GN_Small_Ceiling._FinalSet_conservative ~ jitter(FullData$Age.x))
lines(lowess(FullData$Age.x,FullData$GN_Small_Ceiling._FinalSet_conservative),col="purple")
ggplot(FullData, aes(x=Age.x, y=GN_Small_Ceiling._FinalSet_conservative)) +
  geom_jitter()+
  xlab("Age (Years)") +
  ylab("Give-N Knower-level Score") +
  ggtitle("Age vs. Knower-level Give-N Score") +
  theme(plot.title=element_text(hjust=0.5,face="bold",size = 16),
        axis.text=element_text(size=12),axis.title=element_text(size=14)) +
  geom_smooth(method = "loess",se=FALSE)
# Interval (Age < 5)
ggplot(subset(FullData, FullData$Age.x<5), aes(x=Age.x, y=GN_Small_Ceiling._FinalSet_conservative)) +
  geom_jitter() +
  xlab("Age") +
  ylab("Give N Knower-level Score") +
  ggtitle("Full Data Set: Age vs. Knower-level Give-N Score") +
  geom_smooth(method = "lm",se=FALSE)
# Proportion
plot(FullData$Proportion.Correct..Give.N.small. ~ jitter(FullData$Age.x))
lines(lowess(FullData$Age.x,FullData$Proportion.Correct..Give.N.small.),col="forest green")
ggplot(FullData, aes(x=Age.x, y=Proportion.Correct..Give.N.small.)) +
  geom_jitter() +
  xlab("Age (Years)") +
  ylab("Give-N Proportion-Correct Score") +
  ggtitle("Age vs. Proportion-Correct Give-N Score") +
  theme(plot.title=element_text(hjust=0.5,face="bold",size = 16),
        axis.text=element_text(size=12),axis.title=element_text(size=14)) +
  geom_smooth(method = "loess",se=FALSE)
# Proportion (Age < 5)
ggplot(subset(FullData, FullData$Age.x<5), aes(x=Age.x, y=Proportion.Correct..Give.N.small.)) +
  geom_jitter() +
  xlab("Age") +
  ylab("Give N Knower-level Score") +
  ggtitle("Full Data Set: Age vs. Knower-level Give N Score") +
  geom_smooth(method = "lm",se=FALSE)

# Sarnecka Age, Hornburg Age, and Ansari Age Comparison Plot - Using ggplot
SA_SP_Small_Int <- ggplot(data=SarneckaAge, mapping=aes(x=Age.x, y=GN_Small_Ceiling._FinalSet_conservative)) + geom_jitter() + geom_smooth(method = "loess", se=FALSE) + ggtitle("Sarnecka Age Range:\nGive-N Interval Classification") 
SA_SP_Small_Prop <- ggplot(SarneckaAge, aes(x=Age.x, y=Proportion.Correct..Give.N.small.)) + geom_jitter() + geom_smooth(method = "loess",se=FALSE) + ggtitle("Sarnecka Age Range:\nGive-N Proportion Correct Classification")
HA_SP_Small_Int <- ggplot(HornburgAge, aes(x=Age.x, y=GN_Small_Ceiling._FinalSet_conservative)) + geom_jitter() + geom_smooth(method = "loess",se=FALSE) + ggtitle("Hornburg Age Range:\nGive-N Interval Classification")
HA_SP_Small_Prop <- ggplot(HornburgAge, aes(x=Age.x, y=Proportion.Correct..Give.N.small.)) + geom_jitter() + geom_smooth(method = "loess",se=FALSE) + ggtitle("Hornburg Age Range:\nGive-N Proportion Correct Classification")
AA_SP_Small_Int <- ggplot(data=AnsariAge, mapping=aes(x=Age.x, y=GN_Small_Ceiling._FinalSet_conservative)) + geom_jitter() + geom_smooth(method = "loess", se=FALSE) + ggtitle("Ansari Age Range:\nGive-N Interval Classification") 
AA_SP_Small_Prop <- ggplot(AnsariAge, aes(x=Age.x, y=Proportion.Correct..Give.N.small.)) + geom_jitter() + geom_smooth(method = "loess",se=FALSE) + ggtitle("Ansari Age Range:\nGive-N Proportion Correct Classification")
grid.arrange(SA_SP_Small_Int, SA_SP_Small_Prop, HA_SP_Small_Int, HA_SP_Small_Prop, AA_SP_Small_Int, AA_SP_Small_Prop, nrow=3) 
# Adjust titles and text sizes for these plots
# Standardize x-axis between plots (started this above)

#-------------------------------------------------------------------------------------

# SCATTERPLOTS OF AGE VS GIVE N ALL (INTERVAL AND PROPORTION) FOR ALL AGE GROUPS

# Scatterplots of Sarnecka Age vs. Give-N small and large
# Interval
plot(SarneckaAge$GN_All_Ceiling_Conservative ~ jitter(SarneckaAge$Age.x,1))
lines(predict.loess(SarneckaAge$Age.x,SarneckaAge$GN_All_Ceiling_Conservative,se=TRUE),col="brown")

fit1 <- loess(SarneckaAge$GN_All_Ceiling_Conservative ~ SarneckaAge$Age.x)
my.count <- seq(from=3.0,to=4.7,by=((4.7-3.0)/(1.7-1)))
my.count.rev <- order(my.count, decreasing=TRUE)
pred1 <- predict(fit1, my.count, se=TRUE)
plot(SarneckaAge$GN_All_Ceiling_Conservative ~ jitter(SarneckaAge$Age.x,1))
lines(pred1$fit, lty="solid", col="darkred", lwd=3)
y.polygon <- c((pred1$fit+1.96*pred1$se.fit)[my.count], (pred1$fit-1.96*pred1$se.fit)[my.count.rev])
x.polygon <- c(my.count, my.count.rev)
polygon(x.polygon, y.polygon, col="blue", border=NA)

# Proportion
plot(SarneckaAge$Proportion.Correct..all.Give.N. ~ jitter(SarneckaAge$Age.x,1))
lines(lowess(SarneckaAge$Age.x,SarneckaAge$Proportion.Correct..all.Give.N.),col="brown1")

# Scatterplots of Hornburg Age vs. Give-N small and large
# Interval
plot(HornburgAge$GN_All_Ceiling_Conservative ~ jitter(HornburgAge$Age.x,1))
lines(lowess(HornburgAge$Age.x,HornburgAge$GN_All_Ceiling_Conservative),col="cyan3")
# Proportion
plot(HornburgAge$Proportion.Correct..all.Give.N. ~ jitter(HornburgAge$Age.x,1))
lines(lowess(HornburgAge$Age.x,HornburgAge$Proportion.Correct..all.Give.N.),col="coral4")
# Hornburg Age (Using Hornburg Give N Trials)
plot(HA_Hornburg_Proced$Hornburg.Proportion.Correct ~ jitter(HA_Hornburg_Proced$Age.x))
lines(lowess(HA_Hornburg_Proced$Age.x,HA_Hornburg_Proced$Hornburg.Proportion.Correct),col="coral4")
abline(lm(formula=HA_Hornburg_Proced$Hornburg.Proportion.Correct ~ HA_Hornburg_Proced$Age.x),col="red")

# Sarnecka Age and Hornburg Age Comparison Plot (Doesn't work yet) - Using Built in Plot functions, try making this a matrix
opar <- par(no.readonly = TRUE)
par(mfcol=c(2,2))
plot(SarneckaAge$GN_All_Ceiling_Conservative ~ jitter(SarneckaAge$Age.x,1))
lines(lowess(SarneckaAge$Age.x,SarneckaAge$GN_All_Ceiling_Conservative),col="brown")
par(new=TRUE)
plot(SarneckaAge$Proportion.Correct..all.Give.N. ~ jitter(SarneckaAge$Age.x,1))
lines(lowess(SarneckaAge$Age.x,SarneckaAge$Proportion.Correct..all.Give.N.),col="brown1")
par(new=TRUE)
plot(HornburgAge$GN_All_Ceiling_Conservative ~ jitter(HornburgAge$Age.x,1))
lines(lowess(HornburgAge$Age.x,HornburgAge$GN_All_Ceiling_Conservative),col="cyan3")
par(new=TRUE)
plot(HornburgAge$Proportion.Correct..all.Give.N. ~ jitter(HornburgAge$Age.x,1))
lines(lowess(HornburgAge$Age.x,HornburgAge$Proportion.Correct..all.Give.N.),col="coral4")
par(opar)
legend(5.8,0.51, title="Age Group Subsets",legend=c("Sarnecka Age Group (2.5 to 4.75)","Hornburg Age Group (3.5 to 6.03)","Full Age Group (3.0 to 6.5)"),col=c(2,3,4),lty=c(1,2,3),cex=0.75)

# Sarnecka Age and Hornburg Age Comparison Plot - Using ggplot
SA_SP_All_Int <- ggplot(data=SarneckaAge, mapping=aes(x=Age.x, y=GN_All_Ceiling_Conservative)) + geom_jitter() + geom_smooth(method = "loess", se=FALSE) + ggtitle("Sarnecka Age Range:\nGive-N Interval Classification") 
SA_SP_All_Prop <- ggplot(SarneckaAge, aes(x=Age.x, y=Proportion.Correct..all.Give.N.)) + geom_jitter() + geom_smooth(method = "loess",se=FALSE) + ggtitle("Sarnecka Age Range:\nGive-N Proportion Correct Classification")
HA_SP_All_Int <- ggplot(HornburgAge, aes(x=Age.x, y=GN_All_Ceiling_Conservative)) + geom_jitter() + geom_smooth(method = "loess",se=FALSE) + ggtitle("Hornburg Age Range:\nGive-N Interval Classification")
HA_SP_All_Prop <- ggplot(HornburgAge, aes(x=Age.x, y=Proportion.Correct..all.Give.N.)) + geom_jitter() + geom_smooth(method = "loess",se=FALSE) + ggtitle("Hornburg Age Range:\nGive-N Proportion Correct Classification")
grid.arrange(SA_SP_All_Int,SA_SP_All_Prop,HA_SP_All_Int,HA_SP_All_Prop,nrow=2)
+ scale_x_continuous()
# Add in Ansari data, but need to remake overall grid using Give N small data
# Create new side by side plot of our full data and Hornburg Age data using Give N all
# Standardize x-axis (started this above)
# Figure caption: Points on left have y-axis values drawn from a limited set of possible values (0,1,2,3,4,5,6,7,9,10,12,16), but are jittered to avoid overlapping.

# Scatterplots of Full Data Age vs. Give-N small and large 
# Interval
plot(FullData$GN_All_Ceiling_Conservative ~ jitter(FullData$Age.x,1))
lines(lowess(FullData$Age.x,FullData$GN_All_Ceiling_Conservative),col="deepskyblue4",)
# Proportion
plot(FullData$Proportion.Correct..all.Give.N. ~ jitter(FullData$Age.x,1))
lines(lowess(FullData$Age.x,FullData$Proportion.Correct..all.Give.N.),col="gold4")
# Kids confidently hitting ceiling after 5 it appears, interesting, contrasts literature
# Full Data (Using Hornburg Give-N Trials)
plot(FD_Hornburg_Proced$Hornburg.Proportion.Correct ~ jitter(FD_Hornburg_Proced$Age.x))
abline(lm(formula=FD_Hornburg_Proced$Hornburg.Proportion.Correct ~ FD_Hornburg_Proced$Age.x),col="red")

# Make plot of age vs. GN all ceiling conservative for Sarnecka and Hornburg age kids, color points according to the ranges (colors for separate groups and for overlapping) - Apply same logic as with density plots

#-------------------------------------------------------------------------------------

# SCATTERPLOTS OF AGE VS PPVT RAW SCORES 

# Sarnecka Age 
plot(SarneckaAge$Raw_Score ~ jitter(SarneckaAge$Age.x))
lines(lowess(SarneckaAge$Age.x,SarneckaAge$Raw_Score),col="darkslateblue")

# Hornburg Age
plot(HornburgAge$Raw_Score ~ jitter(HornburgAge$Age.x))
lines(lowess(HornburgAge$Age.x,HornburgAge$Raw_Score),col="deeppink3")

# Ansari Age
plot(AnsariAge$Raw_Score ~ jitter(AnsariAge$Age.x))
lines(lowess(AnsariAge$Age.x,AnsariAge$Raw_Score),col="dodgerblue4")

# Full Data Age
with(FullData,plot(Raw_Score ~ jitter(Age.x)))
with(FullData,lines(lowess(Age.x,Raw_Score),col="hotpink4",lwd=2))
# I don't think this plot is working'
# **This is the plot I will want to use for PPVT**, adjust axes names for paper <-- may be referring to one of the plots below
# Full Data Age
ggplot(FullData, aes(x=Age.x, y=Raw_Score)) +
  geom_jitter() +
  xlab("Age") +
  ylab("PPVT Raw Score") +
  ggtitle("Age vs. PPVT Score") +
  theme(plot.title=element_text(hjust=0.5,face="bold",size = 16),
        axis.text=element_text(size=12),axis.title=element_text(size=14)) +
  geom_smooth(method = "loess",se=FALSE)
# Full Data Age (<6.5 yrs)
ggplot(subset(FullData,FullData$Age.x<6.5), aes(x=Age.x, y=Raw_Score)) +
  geom_jitter() +
  xlab("Age (Years)") +
  ylab("PPVT Raw Score") +
  ggtitle("Age vs. PPVT Raw Score") +
  theme(plot.title=element_text(hjust=0.5,face="bold",size = 16),
        axis.text=element_text(size=12),axis.title=element_text(size=14)) +
  geom_smooth(method = "loess",se=FALSE)

#-------------------------------------------------------------------------------------

# CORRELATION OF GIVE-N SCORE TYPES: KL AND PC

# Correlation between Give N knower-level and Give N proportion-correct
# Give N small - Sarnecka Age - Pearson
pcor.test(SarneckaAge$Proportion.Correct..Give.N.small.,SarneckaAge$GN_Small_Ceiling._FinalSet_conservative,SarneckaAge$Age.x) # r = 0.892, p<0.0001, need to be spearman since we are correlating interval and continuous data?
# Negen and Sarnecka did not find this correlation, but they used titrated method of Give N (page 2023 of paper)
# We can try to recode our data to match the titrated method to create a comparison method between ways of administering the task to see if methodology of administration influences conclusion of whether or not we examine a relationship.

# Give-N small - Full Data - Pearson
pcor.test(FullData$Proportion.Correct..Give.N.small.,FullData$GN_Small_Ceiling._FinalSet_conservative,FullData$Age.x) # r = 0.926, p<0.0001
# Give-N small - Full Data - Spearman
pcor.test(FullData$Proportion.Correct..Give.N.small.,FullData$GN_Small_Ceiling._FinalSet_conservative,FullData$Age.x, method="spearman") # rho = 0.919, p<0.0001, different ways of classifying Give N data need to be correlated with each other in order for us to compare how the different classifications are correlated with the PPVT raw scores.In order for us to interpret any differences we see in the correlations of the PPVT scores, it is important that the two ways of classifying the data are correlated themselves; it essentially rules out a contributing factor to why certain studies may find differences in whether or not they examine a relationship between vocab measure and Give-N. Negen and Sarnecka did not find this correlation, but they used titrated method of Give N (page 2023 of paper, check on this)

# Give N all - Sarnecka Age - Pearson
pcor.test(SarneckaAge$Proportion.Correct..all.Give.N.,SarneckaAge$GN_All_Ceiling_Conservative,SarneckaAge$Age.x) # r = 0.691, p=0.0001, need to run as spearman though (same reason as above)?

#-------------------------------------------------------------------------------------

# PLOTS AND CORRELATIONS OF PPVT VS GIVE N (SMALL AND ALL, INTERVAL AND PROPORTION) FOR ALL AGE GROUPS

# Correlations between small interval Give N and PPVT scores - do plots in ggplot
# Sarnecka Age
plot(SarneckaAge$GN_Small_Ceiling._FinalSet_conservative ~ SarneckaAge$Raw_Score)
lines(lowess(SarneckaAge$Raw_Score,SarneckaAge$GN_Small_Ceiling._FinalSet_conservative),col="blue")
cor.test(SarneckaAge$Raw_Score,SarneckaAge$GN_Small_Ceiling._FinalSet_conservative,use="complete.obs", method="spearman") # rho = 0.540, p=0.0044
(cor(SarneckaAge$Raw_Score,SarneckaAge$GN_Small_Ceiling._FinalSet_conservative,use="complete.obs", method="spearman"))^2 # rho^2 = 0.292
pcor.test(SarneckaAge$Raw_Score,SarneckaAge$GN_Small_Ceiling._FinalSet_conservative,SarneckaAge$Age.x,method="spearman") # rho = 0.264, p=0.203, Sarnecka experiment 2 found this correlation to be significant but we did not find it to be
# Look at characteristics of our Sarnecka Age Give N distribution, differing number of kids overall and in each bin. They have more kids overall, and more kids at younger age (our min. age is 3). This affects the correlation (why they find it and why we do not) because their data are not as subjected to the ceiling affect on PPVT and Give N, which is going to naturally lead to a better correlation (need to compare the Give N vs Age plots across all studies, did this with the ggplot grid ~ line 470). - done in methods section document
# We are comparing the significance of our correlations and theirs across all studies, not necessarily the strengths, but also need to think about the strengths
# Table comparing the strengths/correlations and significance for each paper and our study, basically already have this with the tables comparing the studies in google docs


# Hornburg Age
plot(HornburgAge$GN_Small_Ceiling._FinalSet_conservative ~ HornburgAge$Raw_Score)
lines(lowess(HornburgAge$Raw_Score,HornburgAge$GN_Small_Ceiling._FinalSet_conservative),col="blue")
cor.test(HornburgAge$Raw_Score,HornburgAge$GN_Small_Ceiling._FinalSet_conservative,use="complete.obs", method="spearman") # rho = 0.458
(cor(HornburgAge$Raw_Score,HornburgAge$GN_Small_Ceiling._FinalSet_conservative,use="complete.obs", method="spearman"))^2 # rho^2 = 0.210
pcor.test(HornburgAge$Raw_Score,HornburgAge$GN_Small_Ceiling._FinalSet_conservative,HornburgAge$Age.x, method="spearman") # rho = 0.270, p=0.087 

# Ansari Age
plot(AnsariAge$GN_Small_Ceiling._FinalSet_conservative ~ AnsariAge$Raw_Score)
lines(lowess(AnsariAge$Raw_Score,AnsariAge$GN_Small_Ceiling._FinalSet_conservative),col="blue")
cor.test(AnsariAge$Raw_Score,AnsariAge$GN_Small_Ceiling._FinalSet_conservative,use="complete.obs", method="spearman") # rho = 0.596, p<0.0001
(cor(AnsariAge$Raw_Score,AnsariAge$GN_Small_Ceiling._FinalSet_conservative,use="complete.obs", method="spearman"))^2 # rho^2 = 0.355
pcor.test(AnsariAge$Raw_Score,AnsariAge$GN_Small_Ceiling._FinalSet_conservative,AnsariAge$Age.x, method="spearman") # rho = 0.362, p=0.028 

# Full Data Age 
plot(FullData$GN_Small_Ceiling._FinalSet_conservative ~ FullData$Raw_Score)
lines(lowess(FullData$Raw_Score,FullData$GN_Small_Ceiling._FinalSet_conservative),col="blue")
ggplot(FullData, aes(x=Raw_Score, y=GN_Small_Ceiling._FinalSet_conservative)) +
  geom_jitter() +
  xlab("PPVT Raw Score") +
  ylab("Give-N Knower-level Score") +
  ggtitle("PPVT Raw Scores vs. Knower-level Give-N Scores") +
  theme(plot.title=element_text(hjust=0.5,face="bold",size = 18),
        axis.text=element_text(size=12),axis.title=element_text(size=14)) +
  geom_smooth(method = "loess",se=FALSE)
cor.test(FullData$Raw_Score,FullData$GN_Small_Ceiling._FinalSet_conservative,use="complete.obs",method="spearman") # rho = 0.578,  p<0.0001
(cor(FullData$Raw_Score,FullData$GN_Small_Ceiling._FinalSet_conservative,use="complete.obs", method="spearman"))^2 # rho^2 = 0.334
pcor.test(FullData$Raw_Score,FullData$GN_Small_Ceiling._FinalSet_conservative,FullData$Age.x,method="spearman") # rho = 0.313, p=0.03


# Correlations between small proportion correct Give N and PPVT scores - do all plots using ggplot
# Sarnecka Age
plot(SarneckaAge$Proportion.Correct..Give.N.small. ~ SarneckaAge$Raw_Score)
lines(lowess(SarneckaAge$Raw_Score,SarneckaAge$Proportion.Correct..Give.N.small.),col="red")
cor.test(SarneckaAge$Raw_Score,SarneckaAge$Proportion.Correct..Give.N.small.,use = "complete.obs") # r = 0.681, p=0.0001
(cor(SarneckaAge$Raw_Score,SarneckaAge$Proportion.Correct..Give.N.small.,use = "complete.obs"))^2 # r^2 = 0.464
pcor.test(SarneckaAge$Raw_Score,SarneckaAge$Proportion.Correct..Give.N.small.,SarneckaAge$Age.x) # r = 0.474, p=0.02
pcor.test(SarneckaAge$Age.x,SarneckaAge$Proportion.Correct..Give.N.small.,SarneckaAge$Raw_Score) # r = 0.671, p=0.0002

# Hornburg Age
plot(HornburgAge$Proportion.Correct..Give.N.small. ~ HornburgAge$Raw_Score)
lines(lowess(HornburgAge$Raw_Score,HornburgAge$Proportion.Correct..Give.N.small.),col="red")
cor.test(HornburgAge$Proportion.Correct..Give.N.small.,HornburgAge$Raw_Score,use = "complete.obs") # r = 0.668, p<0.0001
(cor(HornburgAge$Raw_Score,HornburgAge$Proportion.Correct..Give.N.small.,use = "complete.obs"))^2 # r^2 = 0.447
pcor.test(HornburgAge$Raw_Score,HornburgAge$Proportion.Correct..Give.N.small.,HornburgAge$Age.x) # r = 0.545, p=0.0002

# Ansari Age
plot(AnsariAge$Proportion.Correct..Give.N.small. ~ AnsariAge$Raw_Score)
lines(lowess(AnsariAge$Raw_Score,AnsariAge$Proportion.Correct..Give.N.small.),col="red")
cor.test(AnsariAge$Proportion.Correct..Give.N.small.,AnsariAge$Raw_Score,use = "complete.obs") # r = 0.722, p<0.0001
(cor(AnsariAge$Raw_Score,AnsariAge$Proportion.Correct..Give.N.small.,use = "complete.obs"))^2 # r^2 = 0.521
pcor.test(AnsariAge$Raw_Score,AnsariAge$Proportion.Correct..Give.N.small.,AnsariAge$Age.x) # r = 0.464, p=0.004
# They found total number correct vs. BPVS standard score to be significant with everything factored in. When they partialed out age AND pattern construction raw score (which we do not have) the correlation was not significant between number correct and BPVS standard score.

# Full Data Age
plot(FullData$Proportion.Correct..Give.N.small. ~ FullData$Raw_Score)
lines(lowess(FullData$Raw_Score,FullData$Proportion.Correct..Give.N.small.),col="red")
ggplot(FullData, aes(x=Raw_Score, y=Proportion.Correct..Give.N.small.)) +
  geom_jitter() +
  xlab("PPVT Raw Score") +
  ylab("Give-N Proportion-Correct Score") +
  ggtitle("PPVT Raw Scores vs. Proportion-Correct Give-N Scores") +
  theme(plot.title=element_text(hjust=0.5,face="bold",size = 18),
        axis.text=element_text(size=12),axis.title=element_text(size=14)) +
  geom_smooth(method = "loess",se=FALSE)
cor.test(FullData$Raw_Score,FullData$Proportion.Correct..Give.N.small.,use = "complete.obs") # r = 0.734, p<0.0001
(cor(FullData$Raw_Score,FullData$Proportion.Correct..Give.N.small.,use = "complete.obs"))^2 # r^2 = 0.539
pcor.test(FullData$Raw_Score,FullData$Proportion.Correct..Give.N.small.,FullData$Age.x) # r = 0.547, p<0.0001


# Correlations between all interval Give N and PPVT scores 
# Sarnecka Age
plot(SarneckaAge$GN_All_Ceiling_Conservative ~ jitter(SarneckaAge$Raw_Score))
lines(lowess(SarneckaAge$Raw_Score,SarneckaAge$GN_All_Ceiling_Conservative),col="blue")
cor.test(SarneckaAge$Raw_Score,SarneckaAge$GN_All_Ceiling_Conservative,use="complete.obs", method="spearman") # rho = 0.611, p=0.0009
(cor(SarneckaAge$Raw_Score,SarneckaAge$GN_All_Ceiling_Conservative,use="complete.obs",method="spearman"))^2 # rho^2 = 0.374
pcor.test(SarneckaAge$Raw_Score,SarneckaAge$GN_All_Ceiling_Conservative,SarneckaAge$Age.x,method="spearman") # rho = 0.365, p=0.073

# Hornburg Age
plot(HornburgAge$GN_All_Ceiling_Conservative ~ jitter(HornburgAge$Raw_Score))
lines(lowess(HornburgAge$Raw_Score,HornburgAge$GN_All_Ceiling_Conservative),col="blue")
cor.test(HornburgAge$Raw_Score,HornburgAge$GN_All_Ceiling_Conservative,use="complete.obs", method="spearman") # rho = 0.635, p<0.001
(cor(HornburgAge$Raw_Score,HornburgAge$GN_All_Ceiling_Conservative,use="complete.obs",method="spearman"))^2 # rho^2 = 0.404
pcor.test(HornburgAge$Raw_Score,HornburgAge$GN_All_Ceiling_Conservative,HornburgAge$Age.x,method="spearman") # rho = 0.471, p=0.002

# Full Data Age
plot(FullData$GN_All_Ceiling_Conservative ~ jitter(FullData$Raw_Score))
lines(lowess(FullData$Raw_Score,FullData$GN_All_Ceiling_Conservative),col="blue")
cor.test(FullData$Raw_Score,FullData$GN_All_Ceiling_Conservative,use="complete.obs", method="spearman") # rho = 0.700, p<0.001
(cor(FullData$Raw_Score,FullData$GN_All_Ceiling_Conservative,use="complete.obs",method="spearman"))^2 # rho^2 = 0.490
pcor.test(FullData$Raw_Score,FullData$GN_All_Ceiling_Conservative,FullData$Age.x,method="spearman") # rho = 0.477, p<0.0001


# Correlations between all proportion correct Give N and PPVT scores
# Sarnecka Age
plot(SarneckaAge$Proportion.Correct..all.Give.N. ~ jitter(SarneckaAge$Raw_Score))
lines(lowess(SarneckaAge$Raw_Score,SarneckaAge$Proportion.Correct..all.Give.N.),col="red")
cor.test(SarneckaAge$Raw_Score,SarneckaAge$Proportion.Correct..all.Give.N.,use = "complete.obs") # r = 0.681, p=0.0001
(cor(SarneckaAge$Raw_Score,SarneckaAge$Proportion.Correct..all.Give.N.,use = "complete.obs"))^2 # r^2 = 0.464
pcor.test(SarneckaAge$Raw_Score,SarneckaAge$Proportion.Correct..all.Give.N.,SarneckaAge$Age.x) # r = 0.472, p=0.02
pcor.test(SarneckaAge$Age.x,SarneckaAge$Proportion.Correct..all.Give.N.,SarneckaAge$Raw_Score) # r = 0.693, p=0.0001

# Hornburg Age
plot(HornburgAge$Proportion.Correct..all.Give.N. ~ jitter(HornburgAge$Raw_Score))
lines(lowess(HornburgAge$Raw_Score,HornburgAge$Proportion.Correct..all.Give.N.),col="red")
cor.test(HornburgAge$Proportion.Correct..all.Give.N.,HornburgAge$Raw_Score,use = "complete.obs") # r = 0.702, p<0.0001
(cor(HornburgAge$Raw_Score,HornburgAge$Proportion.Correct..all.Give.N.,use = "complete.obs"))^2 # r^2 = 0.493
pcor.test(HornburgAge$Raw_Score,HornburgAge$Proportion.Correct..all.Give.N.,HornburgAge$Age.x) # r = 0.584, p<0.0001

# Hornburg Age (Using Hornburg Give N Trials)
plot(HA_Hornburg_Proced$Hornburg.Proportion.Correct ~ jitter(HA_Hornburg_Proced$Raw_Score))
lines(lowess(HA_Hornburg_Proced$Raw_Score,HA_Hornburg_Proced$Hornburg.Proportion.Correct),col="red")
cor.test(HA_Hornburg_Proced$Raw_Score,HA_Hornburg_Proced$Hornburg.Proportion.Correct,use = "complete.obs") # r = 0.261, p=0.142
(cor(HA_Hornburg_Proced$Raw_Score,HA_Hornburg_Proced$Hornburg.Proportion.Correct,use = "complete.obs"))^2 # r^2 = 0.068
pcor.test(HA_Hornburg_Proced$Raw_Score,HA_Hornburg_Proced$Hornburg.Proportion.Correct,HA_Hornburg_Proced$Age.x) # r = 0.159, p=0.385

# Full Data Age
plot(FullData$Proportion.Correct..all.Give.N. ~ jitter(FullData$Raw_Score))
lines(lowess(FullData$Raw_Score,FullData$Proportion.Correct..all.Give.N.),col="red")
cor.test(FullData$Raw_Score,FullData$Proportion.Correct..Give.N.small.,use = "complete.obs") # r = 0.734, p<0.0001
(cor(FullData$Raw_Score,FullData$Proportion.Correct..all.Give.N.,use = "complete.obs"))^2 # r^2 = 0.571
pcor.test(FullData$Raw_Score,FullData$Proportion.Correct..all.Give.N.,FullData$Age.x) # r = 0.575, p<0.0001

# Full Data (Using Hornburg Give N Trials)
plot(FD_Hornburg_Proced$Hornburg.Proportion.Correct ~ jitter(FD_Hornburg_Proced$Raw_Score))
lines(lowess(FD_Hornburg_Proced$Raw_Score,FD_Hornburg_Proced$Hornburg.Proportion.Correct),col="red")
cor.test(FD_Hornburg_Proced$Raw_Score,FD_Hornburg_Proced$Hornburg.Proportion.Correct,use = "complete.obs") # r = 0.589, p=0.0001
(cor(FD_Hornburg_Proced$Raw_Score,FD_Hornburg_Proced$Hornburg.Proportion.Correct,use = "complete.obs"))^2 # r^2 = 0.347
pcor.test(FD_Hornburg_Proced$Raw_Score,FD_Hornburg_Proced$Hornburg.Proportion.Correct,FD_Hornburg_Proced$Age.x) # r = 0.350, p=0.036

# Negen & Sarnecka showed in experiment 3 that it's less likely to find a significant correlation between Give-N and PPVT when looking at proportion correct classification, but that does not necessarily mean it does not exist. Hornburg's correlation between Give-N and Language (CELF) was not significant (0.44). Reasons for difference between our percent correct vs. vocab findings and Hornburg's... Expressive (CELF) vs. receptive (PPVT) vocab measure? CELF is not just testing vocabulary like PPVT (read about it in Hornburg study). Percent correct characterization less likely to detect correlations? Differences in quantities tested during Give N? Differences in number of trials during Give-N testing? Uncontrollable differences/outside factors (importance for replication)? Include Ansari in this (they used same trials for small and used receptive language measure)... Overarching point and analysis/conclusion structure: REPLICATION IS IMPORTANT, SO IS SPECIFIC TEST USED, CHARACTERIZATION OF DATA, and AGE OF PARTICIPANTS (for developmental measures)
# Include discussion of SES for final paper, Negen and Sarnecka has data/a paper on this. Gave a talk at CUNY 2017 or 2018 talk, about the influence of culture on number knowledge.

#-------------------------------------------------------------------------------------

# LINEAR MODELS - Probably don't need these

# Ordinal logistic regression models for ordinal Give N - need to specify a method? (see Details in ?optim) - Raw score integer or numeric? Does it matter?
# Sarnecka age group
olr_S <- polr(GN_SM_Fac ~ Age.x*SES_Score*Raw_Score, data = SarneckaAge, Hess=TRUE)
print(olr_S)
summary(olr_S)

# Sarnecka age group
olr_H <- polr(GN_SM_Fac ~ Age.x*SES_Score*Raw_Score, data = HornburgAge, Hess=TRUE)
print(olr_H)
summary(olr_H)

# Sarnecka age group
olr_F <- polr(GN_SM_Fac ~ Age.x*SES_Score*Raw_Score, data = FullData, Hess=TRUE)
print(olr_F)
summary(olr_F)


# Linear models for continuous Give N - how do we go about omitting NA's?
# Sarnecka age group
lm_S <- lm(formula=Proportion.Correct..Give.N.small.~Age.x+Raw_Score, data=SarneckaAge)
print(lm_S)
summary(lm_S)
cor.test(SarneckaAge$Proportion.Correct..Give.N.small,SarneckaAge$SES_Score)

# Hornburg age group
lm_H1 <- lm(formula=Proportion.Correct..Give.N.small.~Age.x+Raw_Score, data=HornburgAge)
lm_H2 <- lm(formula=Proportion.Correct..Give.N.small.~Age.x+Raw_Score+SES_Score, data=HornburgAge)
lm_H3 <- lm(formula=Proportion.Correct..Give.N.small.~Age.x+Raw_Score+SES_Score+Sex.x, data=HornburgAge)
summary(lm_H1) 
summary(lm_H2)
summary(lm_H3)
# Based on residual error and adjusted R-squared, lm_H1 model fits data the best

# Full data age group
lm_F <- lm(formula=Proportion.Correct..Give.N.small.~Age.x+Raw_Score, data=FullData,)
print(lm_F)
summary(lm_F)

#--------------------------------------------------------------------------------------

# New analyses as of 02/26/21
# Age groups divided by older/younger kids (less than or greater than 5)
Age_less_5 <- subset(FullData, FullData$Age.x < 5) # 31 kids
Age_greater_5 <- subset(FullData, FullData$Age.x >= 5) # 16 kids

# Younger Kids (age < 5)
# Knower-Level---------
cor.test(Age_less_5$Raw_Score,Age_less_5$GN_Small_Ceiling._FinalSet_conservative,use = "complete.obs") # r = 0.595, p<0.001 (p=0.0004)
(cor(Age_less_5$Raw_Score,Age_less_5$GN_Small_Ceiling._FinalSet_conservative,use = "complete.obs"))^2 # r^2 = 0.354
pcor.test(Age_less_5$Raw_Score,Age_less_5$GN_Small_Ceiling._FinalSet_conservative,Age_less_5$Age.x) # r = 0.253 (p=0.18)
# Proportion Correct---------
cor.test(Age_less_5$Raw_Score,Age_less_5$Proportion.Correct..Give.N.small.,use = "complete.obs") # r = 0.707, p<0.0001 (p=0.000009)
(cor(Age_less_5$Raw_Score,Age_less_5$Proportion.Correct..Give.N.small.,use = "complete.obs"))^2 # r^2 = 0.500
pcor.test(Age_less_5$Raw_Score,Age_less_5$Proportion.Correct..Give.N.small.,Age_less_5$Age.x) # r = 0.453, p<0.05 (p=0.01)

# Older Kids (age >= 5)
# Knower-Level---------
cor.test(Age_greater_5$Raw_Score,Age_greater_5$GN_Small_Ceiling._FinalSet_conservative,use = "complete.obs") # r = 0.654, p<0.01 (p=0.006)
(cor(Age_greater_5$Raw_Score,Age_greater_5$GN_Small_Ceiling._FinalSet_conservative,use = "complete.obs"))^2 # r^2 = 0.428
pcor.test(Age_greater_5$Raw_Score,Age_greater_5$GN_Small_Ceiling._FinalSet_conservative,Age_greater_5$Age.x) # r = 0.705, p<0.01 (p=0.003)
# Proportion Correct---------
cor.test(Age_greater_5$Raw_Score,Age_greater_5$Proportion.Correct..Give.N.small.,use = "complete.obs") # r = 0.712, p<0.01 (p=0.002)
(cor(Age_greater_5$Raw_Score,Age_greater_5$Proportion.Correct..Give.N.small.,use = "complete.obs"))^2 # r^2 = 0.507
pcor.test(Age_greater_5$Raw_Score,Age_greater_5$Proportion.Correct..Give.N.small.,Age_greater_5$Age.x) # r = 0.751, p<0.01 (p=0.001) 

# Table of correlations between PPVT and Give-N (KL and PC) for age groups broken down around age 5 yrs
Age5_rnames <- c("Age < 5","Age >= 5")
Age5_cnames <- c("Knower-Level", "Proportion-Correct")
Age5_Corr <- matrix(c(
  cor(Age_less_5$Raw_Score,Age_less_5$GN_Small_Ceiling._FinalSet_conservative,use = "complete.obs"),
  cor(Age_less_5$Raw_Score,Age_less_5$Proportion.Correct..Give.N.small.,use = "complete.obs"),
  cor(Age_greater_5$Raw_Score,Age_greater_5$GN_Small_Ceiling._FinalSet_conservative,use = "complete.obs"),
  cor(Age_greater_5$Raw_Score,Age_greater_5$Proportion.Correct..Give.N.small.,use = "complete.obs")),
  nrow=2, ncol=2, byrow=TRUE, dimnames=list(Age5_rnames,Age5_cnames))
View(Age5_Corr)


# Group totals uneven, cut into more even groups around median
median(FullData$Age.x) # Median is 4.7
Age_less_4.7 <- subset(FullData, FullData$Age.x < 4.7) # 23 kids
Age_greater_4.7 <- subset(FullData, FullData$Age.x >= 4.7) # 24 kids

# Young Kids (age < 4.7)
# Knower-Level---------
cor.test(Age_less_4.7$Raw_Score,Age_less_4.7$GN_Small_Ceiling._FinalSet_conservative,use = "complete.obs") # r = 0.576, p<0.01 (p=0.004)
(cor(Age_less_4.7$Raw_Score,Age_less_4.7$GN_Small_Ceiling._FinalSet_conservative,use = "complete.obs"))^2 # r^2 = 0.332
pcor.test(Age_less_4.7$Raw_Score,Age_less_4.7$GN_Small_Ceiling._FinalSet_conservative,Age_less_4.7$Age.x) # r = 0.281 (p=0.21)
# Proportion Correct---------
cor.test(Age_less_4.7$Raw_Score,Age_less_4.7$Proportion.Correct..Give.N.small.,use = "complete.obs") # r = 0.715, p<0.001 (p=0.0001)
(cor(Age_less_4.7$Raw_Score,Age_less_4.7$Proportion.Correct..Give.N.small.,use = "complete.obs"))^2 # r^2 = 0.511
pcor.test(Age_less_4.7$Raw_Score,Age_less_4.7$Proportion.Correct..Give.N.small.,Age_less_4.7$Age.x) # r = 0.496, p<0.05 (p=0.02)

# Old Kids (age >= 4.7)
# Knower-Level---------
cor.test(Age_greater_4.7$Raw_Score,Age_greater_4.7$GN_Small_Ceiling._FinalSet_conservative,use = "complete.obs") # r = 0.429, p<0.05 (p=0.04)
(cor(Age_greater_4.7$Raw_Score,Age_greater_4.7$GN_Small_Ceiling._FinalSet_conservative,use = "complete.obs"))^2 # r^2 = 0.184
pcor.test(Age_greater_4.7$Raw_Score,Age_greater_4.7$GN_Small_Ceiling._FinalSet_conservative,Age_greater_4.7$Age.x) # r = 0.416, p<0.05 (p=0.048)
# Proportion Correct---------
cor.test(Age_greater_4.7$Raw_Score,Age_greater_4.7$Proportion.Correct..Give.N.small.,use = "complete.obs") # r = 0.456, p<0.05 (p=0.025)
(cor(Age_greater_4.7$Raw_Score,Age_greater_4.7$Proportion.Correct..Give.N.small.,use = "complete.obs"))^2 # r^2 = 0.208
pcor.test(Age_greater_4.7$Raw_Score,Age_greater_4.7$Proportion.Correct..Give.N.small.,Age_greater_4.7$Age.x) # r = 0.456, p<0.05 (p=0.03) 

# Table of correlations between PPVT and Give-N (KL and PC) for age groups broken down around age 4.7 yrs
Age4.7_rnames <- c("Age < 4.7","Age >= 4.7")
Age4.7_cnames <- c("Knower-Level", "Proportion-Correct")
Age4.7_Corr <- matrix(c(
  cor(Age_less_4.7$Raw_Score,Age_less_4.7$GN_Small_Ceiling._FinalSet_conservative,use = "complete.obs"),
  cor(Age_less_4.7$Raw_Score,Age_less_4.7$Proportion.Correct..Give.N.small.,use = "complete.obs"),
  cor(Age_greater_4.7$Raw_Score,Age_greater_4.7$GN_Small_Ceiling._FinalSet_conservative,use = "complete.obs"),
  cor(Age_greater_4.7$Raw_Score,Age_greater_4.7$Proportion.Correct..Give.N.small.,use = "complete.obs")),
  nrow=2, ncol=2, byrow=TRUE, dimnames=list(Age4.7_rnames,Age4.7_cnames))
View(Age4.7_Corr)

# Age split around 5 years plots
# KL
ggplot(Age_less_5, aes(x=Raw_Score, y=GN_Small_Ceiling._FinalSet_conservative)) +
  geom_jitter() +
  xlab("PPVT Raw Score") +
  ylab("Give-N Knower-Level Score") +
  ggtitle("Age < 5: PPVT Raw Scores vs. Knower-Level Give-N Scores") +
  theme(plot.title=element_text(hjust=0.5,face="bold",size = 13),
        axis.text=element_text(size=12),axis.title=element_text(size=14)) +
  geom_smooth(method = "loess",se=FALSE)

ggplot(Age_greater_5, aes(x=Raw_Score, y=GN_Small_Ceiling._FinalSet_conservative)) +
  geom_jitter() +
  xlab("PPVT Raw Score") +
  ylab("Give-N Knower-Level Score") +
  ggtitle("Age >= 5: PPVT Raw Scores vs. Knower-Level Give-N Scores") +
  theme(plot.title=element_text(hjust=0.5,face="bold",size = 13),
        axis.text=element_text(size=12),axis.title=element_text(size=14)) +
  geom_smooth(method = "loess",se=FALSE)

# PC
ggplot(Age_less_5, aes(x=Raw_Score, y=Proportion.Correct..Give.N.small.)) +
  geom_jitter() +
  xlab("PPVT Raw Score") +
  ylab("Give-N Proportion-Correct Score") +
  ggtitle("Age < 5: PPVT Raw Scores vs. Proportion-Correct Give-N Scores") +
  theme(plot.title=element_text(hjust=0.5,face="bold",size = 13),
        axis.text=element_text(size=12),axis.title=element_text(size=14)) +
  geom_smooth(method = "loess",se=FALSE)

ggplot(Age_greater_5, aes(x=Raw_Score, y=Proportion.Correct..Give.N.small.)) +
  geom_jitter() +
  xlab("PPVT Raw Score") +
  ylab("Give-N Proportion-Correct Score") +
  ggtitle("Age < 5: PPVT Raw Scores vs. Proportion-Correct Give-N Scores") +
  theme(plot.title=element_text(hjust=0.5,face="bold",size = 13),
        axis.text=element_text(size=12),axis.title=element_text(size=14)) +
  geom_smooth(method = "loess",se=FALSE)
