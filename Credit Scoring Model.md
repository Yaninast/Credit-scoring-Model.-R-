---
title: "Credit Scoring Model"
author: "Yanina Strylets"
date: "September 28, 2019"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
# Load required packages 
```{r}
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_221')
library(rJava)
library(xlsxjars)
library(xlsx)

library(readr)
library(pbkrtest)
library(car)
library(leaps)
library(MASS)

library(kableExtra)
library(knitr)
library(ggplot2)
library(dplyr)
library(psych)
library(flux)
library(gridExtra)
library(moments)
library(rockchalk)
library(RColorBrewer)
require(RColorBrewer)

library(readr)
library(zoo)
library(psych)
library(ROCR)
library(corrplot)
library(InformationValue)
library(pbkrtest)
library(glm2)
library(aod)
library(rpart)
library(tidyverse)
library(broom)
library(lattice)
library(caret)

library(stargazer)#Well-Formatted Regression and Summary Statistics Tables.
library(mice)
library(VIM)
library(woeBinning)
library(OneR)
library(pryr)

```
# Read_credit_card_default_data.R
```{r}
# Define path and file;

setwd('C:\\Users\\')

# Read the RData object using readRDS();
credit_card_default <- readRDS('credit_card_default.RData')
```
# Explore data
```{r}
str(credit_card_default)
summary(credit_card_default)

file.name <- 'table_0.html'
out.path = 'C:\\Users\\yanin\\Desktop\\Capstone\\Project Output\\'

stargazer(credit_card_default, type=c('html'),out=paste(out.path,file.name,sep=''),
	title=c('Table 2: Summary Statistics for Credit Card Default Data'),
	align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, median=TRUE)


```
# Table: The composition of train, test and validate data sets
```{r}
traint_test_val<- (table(credit_card_default$data.group)/30000)*100
traint_test_val<-cbind (table(credit_card_default?data.group), traint_test_val)

traint_test_val <- as.data.frame( traint_test_val)
colnames(traint_test_val) <- c('# of observations', ' % distribution')
rownames(traint_test_val) <- c('Train ','Test', 'Validate')

file.name <-'example_1.html'
stargazer(traint_test_val, type=c('html'),out=paste(out.path,file.name,sep=''),
	title=c('Table XX: The composition of train, test and validate data sets'),
	align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE,
	summary=FALSE )

```
# Data quality check 

1. Remap invalid observations of EDUCATION variable to valid data points

```{r}
#The distribution of the variable revealed the presence of two classes, 5 and 6, that are not supposed to be there based on the data dictionary 
#I don't known what the respondents meant by putting 5 and 6 values as their answers I decided to predict their responses 
#using multiple imputation method  

table(credit_card_default$EDUCATION)

data_subset <- subset(credit_card_default, select = c(LIMIT_BAL,SEX,EDUCATION,MARRIAGE,AGE,DEFAULT))
data_subset$EDUCATION[data_subset$EDUCATION == 5 | data_subset$EDUCATION == 6 | data_subset$EDUCATION == 0] <- NA
summary(data_subset)

# Impute NA's using pmm and cart methods within MICE function 
tempData <- mice(data_subset,m=5,maxit=100,meth='pmm',seed=500)
tempData_cart <- mice(data_subset,m=5,maxit=100,meth='cart',seed=500)

# check the imputed values for EDUCATION stored in each of the 5 imputed dataset of each multiple imputed dataset object created using pmm, random forest and cart methods 

tempData$imp$EDUCATION
tempData_cart$imp$EDUCATION

# check and compare the distribution of the imputed and original data 
library(lattice)

densityplot(tempData)
densityplot(tempData_cart)

# check which dataset in tempdata_cart produces the most accurate prediction of default 

fitm <- with(tempData_cart, glm(DEFAULT ~ LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE,family="poisson"(link="log")))
ls(fitm)
summary(fitm)
fitm$analyses

# replace NA's with values from 5 th dataset of tempdata_cart mids since its distribution resembles the original data distribution the most 
library(conflicted)
imputedData <- mice::complete(tempData_cart,5)
head(imputedData)
colSums(is.na(imputedData))

#Replace the original EDUCATION variable column  with imputed in the credit_card_default  dataframe
credit_card_default$EDUCATION<-imputedData$EDUCATION

# check the distribution of values within   EDUCATION variable 
colSums(is.na(credit_card_default))
table(credit_card_default$EDUCATION)

mem_change(rm(fitm))
mem_change(rm(imputedData))

```
2. Remap invalid observations of MARRIAGE variable to valid values

```{r}
# the range of the variable' values does not align with the one expected based on the dictionary
table(credit_card_default$MARRIAGE)
# replace the typo or out of range values with NA and predict the missing values 
#using multiple imputation and cart method 

data_subset <- subset(credit_card_default, select = c(LIMIT_BAL,SEX,EDUCATION,MARRIAGE,AGE,DEFAULT))
data_subset$MARRIAGE[data_subset$MARRIAGE == 0] <- NA
summary(data_subset)


# Impute NA's using cart methods within MICE function 
tempData_cart_2 <- mice(data_subset,m=5,maxit=100,meth='cart',seed=500)

# check  the imputed values stored in each of the 5 imputed dataset of tempData_cart_2 object
tempData_cart_2$imp$MARRIAGE

# check and compare the distribution of the imputed and original data 
densityplot(tempData_cart_2)

# check which dataset in tempdata_cart produces the most accurate prediction of default 

fitm_2 <- with(tempData_cart_2, glm(DEFAULT ~ LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE,family="poisson", link="log"))

fitm_2$analyses

# replace NA's with values from 5 th dataset of tempdata_cart mids 
#since its distribution resembles the original data distribution the most 

imputedData_2 <- mice::complete(tempData_cart_2,5)
head(imputedData_2)
colSums(is.na(imputedData_2))

#Replace the original MARRIAGE variable column  with imputed in the credit_card_default dataframe
credit_card_default$MARRIAGE<- imputedData_2$MARRIAGE

# check the distribution of values within MARRIAGE variable 

table(credit_card_default$MARRIAGE)
class(credit_card_default$MARRIAGE)

mem_change(rm(fitm_2))
mem_change(rm(imputedData_2))
```
3. Rename PAY_0 column to PAY_1 

```{r}
# Rename PAY_0 column to PAY_1 
names(credit_card_default)[7]<-  "PAY_1"
head(credit_card_default)
```
4. Need to make sure the data is understood correctly by R, since we have a mix of numerical and categorical variables 

```{r}
credit_card_default$ID<- as.factor(credit_card_default$ID)
credit_card_default$SEX<- as.factor(credit_card_default$SEX)
credit_card_default$EDUCATION<-as.factor(credit_card_default$EDUCATION)
credit_card_default$MARRIAGE<-as.factor(credit_card_default$MARRIAGE)
str(credit_card_default)
```
5. Distribution of PAY variable conditioned by credit_card_default$BILL_AMT2 == credit_card_default$PAY_AMT1

```{r}
a<- round((table(credit_card_default$PAY_2[credit_card_default$BILL_AMT2 <= credit_card_default$PAY_AMT1])/length(credit_card_default$PAY_2[credit_card_default$BILL_AMT2 <= credit_card_default$PAY_AMT1])*100),2)

b<- round((table(credit_card_default$PAY_3[credit_card_default$BILL_AMT3 <= credit_card_default$PAY_AMT2])/length(credit_card_default$PAY_3[credit_card_default$BILL_AMT3 <= credit_card_default$PAY_AMT2])*100),2)

c<- round((table(credit_card_default$PAY_4[credit_card_default$BILL_AMT4 <= credit_card_default$PAY_AMT3])/length(credit_card_default$PAY_4[credit_card_default$BILL_AMT4 <= credit_card_default$PAY_AMT3])*100),2)

d<- round((table(credit_card_default$PAY_5[credit_card_default$BILL_AMT5 <= credit_card_default$PAY_AMT4])/length(credit_card_default$PAY_5[credit_card_default$BILL_AMT5 <= credit_card_default$PAY_AMT4])*100),2)

e<- round((table(credit_card_default$PAY_6[credit_card_default$BILL_AMT6 <= credit_card_default$PAY_AMT5])/length(credit_card_default$PAY_6[credit_card_default$BILL_AMT6 <= credit_card_default$PAY_AMT5])*100),2)

na_append <- function(x){
  x[unique(names(a)[! names(a)%in% names(x)])] <-'NA'
  return(x[order(as.numeric(names(x)))])
}

b<- na_append(b)
c<- na_append(c)
d<- na_append(d)
e<- na_append(e)

pool<- c(a,b,c,d,e)
results<- as.data.frame(matrix(pool,nrow = 5, byrow = TRUE));
colnames(results) <- names(a)
rownames(results)<- c("PAY_2", "PAY_3","PAY_4","PAY_5","PAY_6")

file.name <- 'table_1.html'
out.path = 'C:\\Users\\yanin\\Desktop\\Capstone\\Project Output\\'
stargazer(results, type=c('html'),out=paste(out.path, file.name, sep=''),title=c('Table 2. The frequency table of Pay variable under condition that the balance has been paid in full'), align=TRUE, digits=2, initial.zero=TRUE, summary=FALSE )

```
6. Distribution of PAY variable conditioned by credit_card_default$BILL_AMT2 != credit_card_default$PAY_AMT1 & credit_card_default$PAY_AMT1!= 0

```{r}
a_1<- round((table(credit_card_default$PAY_2[credit_card_default$BILL_AMT2 > credit_card_default$PAY_AMT1& credit?card_default$PAY_AMT1> 0])/length(credit_card_default$PAY_2[credit_card_default$BILL_AMT2 > credit_card_default$PAY_AMT1& credit_card_default$PAY_AMT1> 0])*100),2)

b_1<- round((table(credit_card_default$PAY_3[credit_card_default$BILL_AMT3 > credit_card_default$PAY_AMT2& credit_card_default$PAY_AMT2> 0])/length(credit_card_default$PAY_3[credit_card_default$BILL_AMT3 > credit_card_default$PAY_AMT2& credit_card_default$PAY_AMT2> 0])*100),2)

c_1<- round((table(credit_card_default$PAY_4[credit_card_default$BILL?AMT4 > credit_card_default$PAY_AMT3& credit_card_default$PAY_AMT3> 0])/length(credit_card_default$PAY_4[credit_card_default$BILL_AMT4 > credit_card_default$PAY_AMT3& credit_card_default$PAY_AMT3> 0])*100),2)

d_1<-round((table(credit_card_default$PAY_5[cre?it_card_default$BILL_AMT5 > credit_card_default$PAY_AMT4& credit_card_default$PAY_AMT4>0])/length(credit_card_default$PAY_5[credit_card_default$BILL_AMT5 > credit_card_default$PAY_AMT4& credit_card_default$PAY_AMT4> 0])*100),2)

e_1<- round((table(credit_c?rd_default$PAY_6[credit_card_default$BILL_AMT6 > credit_card_default$PAY_AMT5& credit_card_default$PAY_AMT5> 0])/length(credit_card_default$PAY_6[credit_card_default$BILL_AMT6 > credit_card_default$PAY_AMT5 & credit_card_default$PAY_AMT5> 0])*100),2)

na_append <- function(x){
  x[unique(names(c_1)[! names(c_1)%in% names(x)])] <-'NA'
  return(x[order(as.numeric(names(x)))])
}

a_1<- na_append(a_1)
b_1<- na_append(b_1)
d_1<- na_append(d_1)
e_1<- na_append(e_1)

pool_2<- c(a_1,b_1,c_1,d_1,e_1)
results_2<- as.da?a.frame(matrix(pool_2,nrow = 5, byrow = TRUE));
colnames(results_2) <- names(a_1)
rownames(results_2)<- c("PAY_2", "PAY_3","PAY_4","PAY_5","PAY_6")

file.name <- 'table_2.html'
out.path = 'C:\\Users\\yanin\\Desktop\\Capstone\\Project Output\\'

stargazer(results_2, type=c('html'),out=paste(out.path, file.name, sep=''),title=c('Table 3. The frequency table of Pay variable under condition that the balance has been partially repaid'), align=TRUE, digits=2, initial.zero=TRUE, summary=FALSE )
```
7. Check PAY_variable binning

```{r}

pay_2.bin <- woe.binning(df=credit_card_default,target.var=c('DEFAULT'),pred.var=c('PAY_2'), event.class = 1)
woe.binning.plot(pay_2.bin)
woe.binning.table(pay_2.bin)
```
8. Map -2 values to -1 in Pay variable

```{r}
credit_card_default[7:12]<-sapply(credit_card_default[7:12],  function (x) {ifelse (x<0, -1,x)})
range(credit_card_default$PAY_1)
```
# Data Engineering 

1. Bin Age variable using WOE binning

```{r}
# Load library;
library(woeBinning)
library(OneR)

age.bin <- woe.binning(df=credit_card_default,target.var=c('DEFAULT'),pred.var=c('AGE'), event.class = 1)
# WOE plot for age bins;
woe.binning.plot(age.bin)

# See the WOE Binning Table
woe.binning.table(age.bin)

# Score bins on data frame;
woe.df <- woe.binning.deploy(df= credit_card_default,binning=age.bin, add.woe.or.dum.var ='woe')
head(woe.df)
table(woe.df$AGE.binned)

credit_card_default$AGE <- droplevels(woe.df$AGE.binned, exclude = 'Missing')
levels(credit_card_default$AGE)<- c('Age_18_25', 'Age_26_40', 'Age_41_100')
class(credit_card_default$AGE)

woe_age<- woe.df$woe.AGE.binned
```
2. Create Avg_Bill_Amt variable that indicates the average monthly expenditure  

```{r}
credit_card_default$Avg_Bill_Amt <- round(apply(credit_card_default[13:18],1, FUN = mean),2)
head(credit_card_default)
```
3. Create Avg_Pmt_Amt variable as a proxy for income and indicator of the customer ability to pay the balance  

```{r}
credit_card_default$Avg_Pmt_Amt <- round(apply(credit_card_default[19:24],1, FUN = mean), 2)
head(credit_card_default)
```
4. Create Pmt_Ratio variable to indicate how much of the balance statement is paid each month from April through August 

```{r}
pmt_ratio <- function(bill_amt, pmt_amt){ifelse(pmt_amt<= bill_amt & bill_amt>0, pmt_amt/bill_amt, 1)}
credit_card_default[33:37]<- round(mapply(pmt_ratio, credit_card_default[14:18], credit_card_default[19:23]), 2)
names(credit_card_default)[33:37]<-c('Pmt_Ratio_2','Pmt_Ratio_3', 'Pmt_Ratio_4', 'Pmt_Ratio_5', 'Pmt_Ratio_6')
head(credit_card_default, 10)
credit_card_default[33:37]<- round(mapply(pmt_ratio, credit_card_default[14:18], credit_card_default[19:23]), 2)
names(credit_card_default)[33:37]<-c('Pmt_Ratio_2','Pmt_Ratio_3', 'Pmt?Ratio_4', 'Pmt_Ratio_5', 'Pmt_Ratio_6')
head(credit_card_default, 10)
```
5. Create Average Payment Ratio variable (Avg_Pmt_Ratio) showing what percentage of bill a customer paid from April through August on average

```{r}
credit_card_default$Avg_Pmt_Ratio <- round(apply(credit_card_default[33:37],1, FUN = mean),2)
head(credit_card_default)
range(credit_card_default$Avg_Pmt_Ratio)
```
6. Create Utilization (Util) variable to advise how much of the credit line is the customer using each month from April through September 

```{r}
util_ratio<- function(bill_amt, LIMIT_BAL){
  ifelse(LIMIT_BAL< bill_amt,1,
          ifelse( bill_amt >= 0, bill_amt/LIMIT_BAL, 0)
  )}

credit_card_default[39:44]<- round(mapply(util_ratio, credit_card_default[13:18],credit_card_default[2]), 2)
names(credit_card_default)[39:44]<- c('Util_1','Util_2', 'Util_3', 'Util_4', 'Util_5', 'Util_6' )
 
head(credit_card_default)
```
7. Create Average Utilization (Avg_Util) to advise the habit of card utilization, how much of the credit given to the customer has been used on average for 6 months from April through September 

```{r}
credit_card_default$Avg_Util <- round(apply(credit_card_default[39:44],1, FUN = mean),2)
head(credit_card_default)
range(credit_card_default$Avg_Util)  
```
8. Create the variable that will identify the number of months closed with over limit 

```{r}
util_over_limit<- function(bill_amt, LIMIT_BAL){
  ifelse(LIMIT_BAL < bill_amt,1,0)}

credit_card_default$OVER_LIMIT <-apply(mapply(util_over_limit,credit_card_default[13:18],credit_card_default[2]),1, FUN = sum)
```
9. Create variable to identify customers with payment larger than balance 

```{r}
over_pmt <- function(bill_amt, pmt_amt){ifelse(pmt_amt>bill_amt, 1, 0)}
credit_card_default$OVER_PMT <-apply(mapply(over_pmt? credit_card_default[14:18], credit_card_default[19:23]),1, FUN = sum)
 
credit_card_default[credit_card_default$OVER_PMT>0,]
head(credit_card_default, 10)
```
10. Balance Growth Over 6 Months (Bal_Growth_6mo) - Is the balance growing due to continued spending with only partial payments each month?

```{r}
bal_growth_rate <- function(x, y){ifelse(y!=0, (x-y)/y, (x-y)/1)}
credit_card_default$Bal_Growth_6mo<- round(bal_growth_rate(credit_card_default$BILL_AMT1,credit_card_default$BILL_AMT6),2)

head(credit_card_default,20)
summary(credit_card_default$Bal_Growth_6mo)
```
11. Utilization Growth Over 6 Months (Util_Growth_6mo) - Is the balance getting close to the credit limit?

```{r}
credit_card_default$Util_Growth_6mo <- credit_card_default$Util_1-credit_card_d?fault$Util_6
```
12. Max_Util and Min_Util. Maximum and minimum of credit limit used over 6 months.  

```{r}
credit_card_default$Max_Util <- round(apply(credit_card_default[39:44],1, FUN = max),2)
credit_card_default$Min_Util <- round(apply(credit_card_def?ult[39:44],1, FUN = min),2)
head(credit_card_default)
```
13. Create variable to see the ratio of the current utilization to Maximum Utilization(MAX_Util_ratio)

```{r}
max_current_util_ratio <- function(x, y){ifelse(y!=0, x/y,0)}
credit_card_default$MAX_?til_ratio <- round(max_current_util_ratio(credit_card_default$Util_1, credit_card_default$Max_Util),2)
head(credit_card_default)
```
14. Max Bill Amount (Max_Bill_Amt) - Maximum billed amount over the 6 months.

```{r}
credit_card_default$Max_Bill_Amt <- round(apply(credit_card_default[13:18],1, FUN = max),2)
head(credit_card_default)

```
15. Max Payment Amount (Max_Pmt_Amt) - Maximum payment amount over the 6 months.

```{r}
credit_card_default$Max_Pmt_Amt <- round(apply(credit_card_default[19:24],1, FUN = max),2)
head(credit_card_default)
```
16. Max Delinquency (Max_DLQ) - Take the max of the Pay_X variables. Set -1 and -2 to 0, and then take the max.

```{r}
credit_card_default$Max_DLQ<- apply(sapply(credit_card_default[7:12],  function (x) {ifelse (x<0, 0,x)}), 1,FUN = max)
head(credit_card_default, 10)
hist(credit_card_default$Max_DLQ)
```
17. The most frequent repayment status for a customer over 6 months (Freq_PAY)

```{r}
credit_card_default$Freq_PAY<- as.numeric(apply(credit_card_default[7:12],1,function(x) names(which.max(table(x)))))

# Bin the variable 

Freq_PAY.bin <- woe.binning(df=credit_card_default,target.var=c('DEFAULT'),pred.var=c('Freq_PAY'), event.class = 1)
woe.binning.plot(Freq_PAY.bin)
woe.binning.table(Freq_PAY.bin)

# Score bins on data frame;
woe.df <- woe.binning.deploy(df= credit_card_default,binning=Freq_PAY.bin,add.woe.or.dum.var='woe')

credit_card_default$Freq_PAY_bin <- droplevels(woe.df$Freq_PAY.bin, exclude = 'Missing')
levels(credit_card_default$Freq_PAY_bin )<- c('Z_DULY_PAY', 'DELAY_PAY')
class(credit_card_default$Freq_PAY_bin)

head(woe.df)
WOE_Freq_PAY <- woe.df$woe.Freq_PAY.binned

```
18. Create a REPAY_PATTERN variable to categorize the bank customers according to their balance repayment patterns  

```{r}
output <- array(dim= (dim(credit_card_default[1:5])))
for (row in 1:nrow(credit_card_default)){
  ifelse(credit_card_default$BILL_AMT1[row]>credit_card_default$BILL_AMT2[row], output[row,1]<-1, 0)
  ifelse(credit_card_default$BILL_AMT2[row]>credit_card_default$BILL_AMT3[row], output[row,2]<-1, 0)
  ifelse(credit_card_default$BILL_AMT3[row]>credit_card_default$BILL_AMT4[row], output[row,3]<-1, 0)
  ifelse(credit_card_default$BILL_AMT4[row]>credit_card_default$BILL_AMT5[row], output[row,4]<-1, 0)
  ifelse(credit_card_default$BILL_AMT5[row]>credit_card_default$BILL_AMT6[row], output[row,5]<-1, 0)
}
output[is.na(output == TRUE)]<- 0
table(output)


a<- apply(output, 1, function(x) {
  r <- rle(x >= 1)
  w <- which(r$values & r$lengths ==2)
  if (length(w) > 0) {
     lapply(w,FUN=function(w1){ before <- sum(r$lengths[1:w1]) - r$lengths[w1];
                               c(before+1,before+r$lengths[w1])})
  } else
    0
})

outcome<-array(dim=c(30000,1))
for(i in 1:length(a)){
  ifelse(length(a[[i]])==2,outcome[i,1]<-2,
          ifelse(length(a[[i]])!=2 & a[i]>0,outcome[i,1]<-1, outcome[i,1]<-0)
  )
}
outcome[is.na(outcome)]<-1

b<- apply(output, 1, function(x) {
  r <- rle(x >= 1)
  w <- which(r$values & r$lengths > 2)
  if (length(w) > 0) {
     lapply(w,FUN=function(w1){ before <- sum(r$lengths[1:w1]) - r$lengths[w1];
                               c(before+1,before+r$lengths[w1])})
  } else
    0
})

outcome_2<-array(dim=c(30000,1))
for(i in 1:length(b)){
  ifelse(b[i]>0,outcome_2[i,1]<-3,outcome_2[i,1]<-outcome[i,1])
}
outcome_2[is.na(outcome_2)]<-3

outcome_2[which(output[,1]==1&output[,3]==1 & output[,5] & output[,2]==0 &output[,4]==0 | output[,1]==0&output[,3]==0 & output[,5] ==0 & output[,2]==1 &output[,4]==1)] <- 4

table(outcome_2)

repay_pattern<-cbind(output, outcome_2) 

credit_card_default$REPAY_PATTERN<- outcome_2

head(credit_card_default)

rm(repay_pattern)

```
19. Create a UTIL_PATTERN variable to categorize the bank customers according to their credit utilization patterns 

```{r}
output_3 <- array(dim= (dim(credit_card_default[1:5])))
for (row in 1:nrow(credit_card_default)){
  ifelse(credit_card_default$Util_1[row]>credit_card_default$Util_2[row], output_3[row,1]<-1, 0)
  ifelse(credit_card_default$Util_2[row]>credit_card_default$Util_3[row], output_3[row,2]<-1, 0)
  ifelse(credit_card_default$Util_3[row]>credit_card_default$Util_4[row], output_3[row,3]<-1, 0)
  ifelse(credit_card_default$Util_4[row]>credit_card_default$Util_5[row], output_3[row,4]<-1, 0)
  ifelse(credit_card_default$Util_5[row]>credit_card_default$Util_6[row], output_3[row,5]<-1, 0)
}
output_3[is.na(output_3 == TRUE)]<- 0
table(output_3)
head(credit_card_default[13:18],20)


a_3<- apply(output_3, 1, function(x) {
  r <- rle(x >= 1)
  w <- which(r$values & r$lengths ==2)
  if (length(w) > 0) {
     lapply(w,FUN=function(w1){ before <- sum(r$lengths[1:w1]) - r$lengths[w1];
                               c(before+1,before+r$lengths[w1])})
  } else
    0
})

outcome_3<-array(dim=c(30000,1))
for(i in 1:length(a_3)){
  ifelse(length(a_3[[i]])==2,outcome_3[i,1]<-2,
          ifelse(length(a_3[[i]])!=2 & a_3[i]>0,outcome_3[i,1]<-1, outcome_3[i,1]<-0)
  )
}
outcome_3[is.na(outcome_3)]<-1

b_3<- apply(output_3, 1, function(x) {
  r <- rle(x >= 1)
  w <- which(r$values & r$lengths > 2)
  if (length(w) > 0) {
     lapply(w,FUN=function(w1){ before <- sum(r$lengths[1:w1]) - r$lengths[w1];
                               c(before+1,before+r$lengths[w1])})
  } else
    0
})

outcome_4<-array(dim=c(30000,1))
for(i in 1:length(b_3)){
  ifelse(b_3[i]>0,outcome_4[i,1]<-3,outcome_4[i,1]<-outcome_3[i,1])
}
outcome_4[is.na(outcome_4)]<-3

table(outcome_4)

util_pattern<-cbind(output_3, outcome_4) 

credit_card_default$UTIL_PATTERN<- outcome_4

head(credit_card_default)

rm(util_pattern)

```
# Define my data and Partition data

```{r}
##################################################################################
# Note: data.group values are constructed to partition the data set in a single dimension;
# data.group <- 1*my.data$train + 2*my.data$test + 3*my.data$validate;
# Use the train, test, and validate flags to define the train, test, and validate data sets
#################################################################################
index<-c(25,2:6,31:59)
train<-credit_card_default[credit_card_default$data.group==1,index]
test<-credit_card_default[credit_card_default$data.group==2,index]
validate<-credit_card_default[credit_card_default$data.group==3,index] 

```
# EDA. Part 1. 

1. Get descriptive statistics of the data 

```{r}
str(train)
describe(train)
summary(train)
file.name <- 'table_3.html'
out.path = 'C:\\Users\\yanin\\Desktop\\Capstone\\Project Output\\'

stargazer(train, type=c('html'),out=paste(out.path,file.name,sep=''),
	title=c('Table : Summary Statistics for Train data with engineered features'),
	align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, median=TRUE)

```
2. Identify outliers and extreme outliers 

```{r}
numeric <- c(2,7,8,24,29,30)
lapply(train[numeric], function(x) boxplot.stats(x)$out)
lapply(train[numeric], function(x) boxplot.stats(x, coef= 3)$out)
```
3. Univariate analysis. Numeric data

```{r}
numeric <- c(2,7,8,9:21,24:30)
count_i<- 1
for(column in train[numeric]){
  print(ggplot(train[numeric], aes(x=column)) + 
    geom_histogram(color="blue",aes(fill=..count..)) +
    scale_fill_gradient("Count", low = "blue", high = "red")+
    ggtitle(sprintf("Distribution of %s",names(train[numeric][count_i])))+
    xlab(sprintf("%s",names(train[numeric][count_i])))+
   theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)))
  count_i = count_i+1 
}

count_i<- 1
for (column in train[numeric]){
  print(names(train[numeric][count_i]))
  print(quantile(column, probs = c(0.01, 0.99), na.rm = TRUE))
  count_i= count_i +1
}
```
# Transformation. Numeric variables 
1. train$LIMIT_BAL
```{r}

# Square root and log transformation or truncating the outliers 
#to the 99 percentile values are considered as a remedy for the right skeweness of the variable. 

par(mfrow=c(1,2))

ggplot(data=train, aes(log(train$LIMIT_BAL))) + 
  geom_histogram( col="BLUE", 
                 aes(fill=..count..))+
  xlab("log_LIMIT_BAL")+
  scale_fill_gradient("Count", low = "blue", high = "red")

ggplot(data=train, aes(sqrt(train$LIMIT_BAL))) + 
  geom_histogram( col="BLUE", 
                 aes(fill=..count..))+
   xlab("sqrt_LIMIT_BAL")+
  ggtitle('Distribution of sqrt_LIMIT_BAL')+
  scale_fill_gradient("Count", low = "blue", high = "red")+
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(data=NULL, aes(sqrt(train$LIMIT_BAL[(train$LIMIT_BAL<=500000)]))) + 
  geom_histogram( col="BLUE", 
                 aes(fill=..count..))+
   xlab("")+
  scale_fill_gradient("Count", low = "blue", high = "red")

train$sqrt_LIMIT_BAL<- sqrt(train$LIMIT_BAL)

```
Another method considered to remedy the effect of outliers is to bin the variable. 

```{r}
LIMIT_BAL.bin <- woe.binning(df=credit_card_default,target.var=c('DEFAULT'),pred.var=c('LIMIT_BAL'))

# WOE plot for age bins;
woe.binning.plot(LIMIT_BAL.bin)

# Score bins on data frame;
woe.df <- woe.binning.deploy(df=credit_card_default,binning=LIMIT_BAL.bin,add.woe.or.dum.var='woe' )
head(woe.df)
table(woe.df$LIMIT_BAL.bin)

# See the WOE Binning Table
woe.binning.table(LIMIT_BAL.bin)

credit_card_default$LIMIT_BAL_bin <- droplevels(woe.df$LIMIT_BAL.bin, exclude = 'Missing')
levels(credit_card_default$LIMIT_BAL_bin)<- c('Low', 'Average', 'High')
table(train$LIMIT_BAL_bin)
train$LIMIT_BAL_bin<- credit_card_default[credit_card_default$data.group==1,60]

WOE_LIMIT_BAL<- woe.df$woe.LIMIT_BAL.binned

describe(train)
```
2. train$Avg_Bill_Amt

```{r}
# Use woe.Binning and Optbin to advise if the values less than 1% and greater than 99% could be remapped 
#to the respective percentiles' threshold  

Avg_Bill_Amt.bin <- woe.binning(df=train,target.var=c('DEFAULT'),pred.var=c('Avg_Bill_Amt'))
woe.binning.plot(Avg_Bill_Amt.bin)
woe.df <- woe.binning.deploy(df=train,binning=Avg_Bill_Amt.bin)
head(woe.df)
table(woe.df$Avg_Bill_Amt.bin)
woe.binning.table(Avg_Bill_Amt.bin)

# Binning based on entropy - decision tree;
bin.5 <- optbin(train$DEFAULT ~ train$Avg_Bill_Amt,method=c('infogain'));
table(bin.5)
aggregate(train$DEFAULT, by=list(Avg_Bill_Amt.bin=bin.5[,1]), FUN=mean)

# Create transfomed Avg_Bill_Amt variable with values less than 1% and greater 99% remapped to respective percentiles' thresholds 
train$Avg_Bill_Amt_tfm<-train$Avg_Bill_Amt 
train$Avg_Bill_Amt_tfm[(train$Avg_Bill_Amt_tfm > 302472)]<-302472
train$Avg_Bill_Amt_tfm[(train$Avg_Bill_Amt_tfm< 0)]<- 0
range(train$Avg_Bill_Amt_tfm)


ggplot(data=NULL, aes(train$Avg_Bill_Amt)) + 
  geom_histogram( col="BLUE", 
                 aes(fill=..count..))+
   xlab("")+
  scale_fill_gradient("Count", low = "blue", high = "red")

ggplot(data=train, aes(yjPower(train$Avg_Bill_Amt_tfm, 0.2, jacobian.adjusted = FALSE)))+ 
  geom_histogram( col="BLUE", 
                 aes(fill=..count..))+
  xlab("Transformed_Avg_Bill_Amt")+
  ggtitle('Distribution of Transformed_Avg_Bill_Amt')+
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))+
  scale_fill_gradient("Count", low = "blue", high = "red")

ggplot(data=train, aes(sqrt(train$Avg_Bill_Amt_tfm))) + 
  geom_histogram( col="BLUE", 
                 aes(fill=..count..))+
   xlab("sqrt_Avg_Bill_Amt")+
  ggtitle('Distribution of sqrt_Avg_Bill_Amt')+
  scale_fill_gradient("Count", low = "blue", high = "red")+
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


train$Avg_Bill_Amt_tfm<-yjPower(train$Avg_Bill_Amt_tfm, 0.2, jacobian.adjusted = FALSE)
head(train)
```
3. train$Avg_Pmt_Amt

```{r}
# Use woe.Binning and Optbin to advise if the outliers could be remapped to the respective percentiles' threshold  

Avg_Pmt_Amt.bin <- woe.binning(df=train,target.var=c('DEFAULT'),pred.var=c('Avg_Pmt_Amt'))
woe.binning.plot(Avg_Pmt_Amt.bin)
woe.df <- woe.binning.deploy(df=train,binning=Avg_Pmt_Amt.bin)
head(woe.df)
table(woe.df$Avg_Pmt_Amt.bin)
woe.binning.table(Avg_Pmt_Amt.bin)

# Binning based on entropy - decision tree;
bin.5 <- optbin(train$DEFAULT ~ train$Avg_Pmt_Amt,method=c('infogain'));
table(bin.5)
aggregate(train$DEFAULT, by=list(Avg_Pmt_Amt.bin=bin.5[,1]), FUN=mean)

ggplot(data=train, aes(yjPower(train$Avg_Pmt_Amt, 0, jacobian.adjusted = FALSE)))+ 
  geom_histogram( col="BLUE", 
                 aes(fill=..count..))+
  xlab("Transformed_Avg_Pmt_Amt")+
  ggtitle('Distribution of Transformed_Avg_Pmt_Amt')+
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))+
  scale_fill_gradient("Count", low = "blue", high = "red")

ggplot(data=train, aes(sqrt(train$Avg_Pmt_Amt))) + 
  geom_histogram( col="BLUE", 
                 aes(fill=..count..))+
   xlab("sqrt_Avg_Bill_Amt")+
  ggtitle('Distribution of sqrt_Avg_Pmt_Amt')+
  scale_fill_gradient("Count", low = "blue", high = "red")+
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

train$Avg_Pmt_Amt_tfm<- yjPower(train$Avg_Pmt_Amt, 0, jacobian.adjusted = FALSE)
head(train)
```
4. train$Bal_Growth_6mo

```{r}
Bal_Growth_6mo.bin  <- woe.tree.binning(df=credit_card_default,target.var=c('DEFAULT'),pred.var=c('Bal_Growth_6mo'), stop.limit = 0.5)
woe.binning.plot(Bal_Growth_6mo.bin)
woe.df <- woe.binning.deploy(df=credit_card_default,binning=Bal_Growth_6mo.bin, add.woe.or.dum.var='woe')
head(woe.df)
table(woe.df$Bal_Growth_6mo.bin)
woe.binning.table(Bal_Growth_6mo.bin)

credit_card_default$Bal_Growth_6mo_bin <- droplevels(woe.df$Bal_Growth_6mo.bin, exclude = 'Missing')
levels(credit_card_default$Bal_Growth_6mo_bin)<- c('Large Negative Growth', 'Small Growth', 'Large Positive Growth')
table(credit_card_default$Bal_Growth_6mo_bin)
train$Bal_Growth_6mo_bin<- credit_card_default[credit_card_default$data.group==1,61]

WOE_Bal_Growth_6mo<- woe.df$woe.Bal_Growth_6mo.binned

describe(credit_card_default)
head(train)
```
5. train$Max_Bill_Amt

```{r}
# Use woe.Binning and Optbin to advise if the values less than 1% and greater than 99% could be remapped to the respective percentiles' threshold  

Max_Bill_Amt.bin <- woe.binning(df=train,target.var=c('DEFAULT'),pred.var=c('Max_Bill_Amt'))
woe.binning.plot(Max_Bill_Amt.bin)
woe.df <- woe.binning.deploy(df=train,binning=Max_Bill_Amt.bin)
head(woe.df)
table(woe.df$Max_Bill_Amt.bin)
woe.binning.table(Max_Bill_Amt.bin)

# Binning based on entropy - decision tree;
bin.5 <- optbin(train$DEFAULT ~ train$Max_Bill_Amt,method=c('infogain'));
table(bin.5)
aggregate(train$DEFAULT, by=list(Max_Bill_Amt.bin=bin.5[,1]), FUN=mean)

# Create transfomed Avg_Bill_Amt variable with values less than 1% and greater 99% remapped to respective percentiles' thresholds 
train$Max_Bill_Amt_tfm<-train$Max_Bill_Amt 
train$Max_Bill_Amt_tfm[(train$Max_Bill_Amt_tfm > 374259.3 )]<-374259.3 
train$Max_Bill_Amt_tfm[(train$Max_Bill_Amt_tfm< 0)]<- 0
range(train$Max_Bill_Amt_tfm)


ggplot(data=NULL, aes(train$Max_Bill_Amt_tfm)) + 
  geom_histogram( col="BLUE", 
                 aes(fill=..count..))+
   xlab("")+
  scale_fill_gradient("Count", low = "blue", high = "red")

ggplot(data=train, aes(yjPower(train$Max_Bill_Amt_tfm, 0.2, jacobian.adjusted = FALSE)))+ 
  geom_histogram( col="BLUE", 
                aes(fill=..count..))+
  xlab("Transformed_Max_Bill_Amt")+
   ggtitle('Distribution of Transformed_Max_Bill_Amt')+
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))+
  scale_fill_gradient("Count", low = "blue", high = "red")

ggplot(data=train, aes(sqrt(train$Max_Bill_Amt_tfm))) + 
  geom_histogram( col="BLUE", 
                 aes(fill=..count..))+
   xlab("sqrt_Max_Bill_Amt")+
  ggtitle("Distribution of sqrt_Max_Bill_Amt")+
  scale_fill_gradient("Count", low = "blue", high = "red")+
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

train$Max_Bill_Amt_tfm<-yjPower(train$Max_Bill_Amt_tfm, 0.2, jacobian.adjusted = FALSE)
```
6. train$Max_Pmt_Amt

```{r}
# Use woe.Binning and Optbin to advise if the outliers could be remapped to the respective percentiles' threshold  

Max_Pmt_Amt.bin <- woe.binning(df=train,target.var=c('DEFAULT'),pred.var=c('Avg_Pmt_Amt'))
woe.binning.plot(Max_Pmt_Amt.bin)
woe.df <- woe.binning.deploy(df=train,binning=Max_Pmt_Amt.bin)
head(woe.df)
table(woe.df$Max_Pmt_Amt.bin)
woe.binning.table(Max_Pmt_Amt.bin)

# Binning based on entropy - decision tree;
bin.5 <- optbin(train$DEFAULT ~ train$Max_Pmt_Amt,method=c('infogain'));
table(bin.5)
aggregate(train$DEFAULT, by=list(Max_Pmt_Amt.bin=bin.5[,1]), FUN=mean)


ggplot(data=train, aes(yjPower(train$Max_Pmt_Amt, 0, jacobian.adjusted = FALSE)))+ 
  geom_histogram( col="BLUE", 
                 aes(fill=..count..))+
  xlab("Transformed_Max_Pmt_Amt")+
  scale_fill_gradient("Count", low = "blue", high = "red")+
  ggtitle('Distribution of Transformed_Max_Pmt_Amt')+
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(data=train, aes(sqrt(train$Max_Pmt_Amt))) + 
  geom_histogram( col="BLUE", 
                 aes(fill=..count..))+
   xlab("sqrt_Avg_Bill_Amt")+
  ggtitle('Distribution of sqrt_Max_Pmt_Amt')+
  scale_fill_gradient("Count", low = "blue", high = "red")+
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

train$Max_Pmt_Amt_tfm<- yjPower(train$Max_Pmt_Amt, 0, jacobian.adjusted = FALSE)
```
Create additional binned variables for Avg_Util 

```{r}
# Avg_Util
Avg_Util.bin <- woe.binning(df=credit_card_default,target.var=c('DEFAULT'),pred.var=c('Avg_Util'))
woe.df <- woe.binning.deploy(df=credit_card_default,binning=Avg_Util.bin, add.woe.or.dum.var='woe')
credit_card_default$Avg_Util_bin <- droplevels(woe.df$Avg_Util.bin, exclude = 'Missing')
levels(credit_card_default$Avg_Util_bin)<- c('No_Util', 'Small_Util', 'Mod_Util', "High_Util")
woe.binning.table(Avg_Util.bin)

train$Avg_Util_bin<- credit_card_default[credit_card_default$data.group==1,62]

WOE_Avg_Util<- woe.df$woe.Avg_Util.binned

head(train)
describe(credit_card_default)
```
# Univariate analysis. Categorical variables 
```{r}
describe(train)
count_i<- 1
for(column in train[-numeric]){
  print(ggplot(train, aes(x=column)) + 
    geom_bar( fill= "tomato2", stat="count") +
    ggtitle(sprintf("Distribution of %s",names(train[-numeric][count_i])))+
    xlab(sprintf("%s",names(train[-numeric][count_i])))+
    theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)))
  count_i = count_i+1 
}

num_var<-c(1,22,23,31:36,39,42)
count_i<- 1
for(column in train[num_var]){
  print(ggplot(train, aes(x=as.numeric(column))) + 
    geom_bar(aes(y = ..prop..), fill= "tomato2", stat="count") +
     geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "Percent") +
    ggtitle(sprintf("Distribution of %s",names(train[num_var][count_i])))+
    xlab(sprintf("%s",names(train[num_var][count_i])))+
    scale_y_continuous(labels = scales::percent)+
    theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)))
  count_i = count_i+1 
}

count_i<- 1
for(column in train[-numeric]){
  print(ggplot(train, aes(x=as.numeric(column))) + 
    geom_bar(aes(y = ..prop..), fill= "tomato2", stat="count") +
     geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "Percent") +
    ggtitle(sprintf("Distribution of %s",names(train[-numeric][count_i])))+
    xlab(sprintf("%s",names(train[-numeric][count_i])))+
    scale_y_continuous(labels = scales::percent)+
    scale_x_discrete(breaks=waiver(), labels = levels(as.factor(column)))+
    theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)))
  count_i = count_i+1 
}

```
# Bin OVER_LIMIT, OVER_PMT, MAX_DLQ, REPAY_PATTERN, UTIL_PATTERN
```{r}
# Bin OVER_LIMIT
OVER_LIMIT.bin <- woe.binning(credit_card_default, target.var = ('DEFAULT'), pred.var=c('OVER_LIMIT'), min.perc.class = 0.07)
woe.df <- woe.binning.deploy(df=credit_card_default,binning=OVER_LIMIT.bin, add.woe.or.dum.var='woe')
woe.binning.table(OVER_LIMIT.bin)
credit_card_default$OVER_LIMIT <- droplevels(woe.df$OVER_LIMIT.bin, exclude = 'Missing')
levels(credit_card_default$OVER_LIMIT)<- c('0', '1')
table(credit_card_default$OVER_LIMIT)
class(credit_card_default$OVER_LIMIT)

train$OVER_LIMIT<- credit_card_default[credit_card_default$data.group==1,46]
WOE_OVER_LIMIT<- woe.df$woe.OVER_LIMIT.binned


# Bin OVER_PMT
OVER_PMT.bin <- woe.binning(credit_card_default, target.var = ('DEFAULT'), pred.var=c('OVER_PMT'))
woe.df <- woe.binning.deploy(df=credit_card_default,binning=OVER_PMT.bin, add.woe.or.dum.var='woe')
woe.binning.table(OVER_PMT.bin)
credit_card_default$OVER_PMT <- droplevels(woe.df$OVER_PMT.bin, exclude = 'Missing')
levels(credit_card_default$OVER_PMT)<- c('0', '1')
table(credit_card_default$OVER_PMT)
class(credit_card_default$OVER_PMT)

train$OVER_PMT<-credit_card_default[credit_card_default$data.group==1,47]

WOE_OVER_PMT<- woe.df$woe.OVER_PMT.binned

# Bin MAX_DLQ
MAX_DLQ.bin <- woe.binning(credit_card_default, target.var = ('DEFAULT'), pred.var=c('Max_DLQ'))
woe.df <- woe.binning.deploy(df=credit_card_default,binning=MAX_DLQ.bin, add.woe.or.dum.var='woe')
woe.binning.table(MAX_DLQ.bin)
credit_card_default$Max_DLQ <- droplevels(woe.df$Max_DLQ.binned, exclude = 'Missing')
levels(credit_card_default$Max_DLQ)<- c('1', '2')
table(credit_card_default$Max_DLQ)
class(credit_card_default$Max_DLQ)

train$Max_DLQ<-credit_card_default[credit_card_default$data.group==1,55]
WOE_MAX_DLQ<- woe.df$woe.Max_DLQ.binned


# Bin REPAY_PATTERN
REPAY_PATTERN.bin <- woe.binning(credit_card_default, target.var = ('DEFAULT'), pred.var=c('REPAY_PATTERN'))
woe.df <- woe.binning.deploy(df=credit_card_default,binning=REPAY_PATTERN.bin, add.woe.or.dum.var='woe')
woe.binning.table(REPAY_PATTERN.bin)
credit_card_default$REPAY_PATTERN_bin<- droplevels(woe.df$REPAY_PATTERN.bin, exclude = 'Missing')
levels(credit_card_default$REPAY_PATTERN_bin)<- c('0', '1-3', '4' )
table(credit_card_default$REPAY_PATTERN_bin)
class(credit_card_default$REPAY_PATTERN_bin)

train$REPAY_PATTERN_bin<- credit_card_default[credit_card_default$data.group==1,63]

WOE_RAPAY_PATTERN<- woe.df$woe.REPAY_PATTERN.binned

# Bin UTIL_PATTERN
UTIL_PATTERN.bin <- woe.binning(credit_card_default, target.var = ('DEFAULT'), pred.var=c('UTIL_PATTERN'))
woe.df <- woe.binning.deploy(df=credit_card_default,binning=UTIL_PATTERN.bin, add.woe.or.dum.var='woe')
woe.binning.table(UTIL_PATTERN.bin)
credit_card_default$UTIL_PATTERN_bin<- droplevels(woe.df$UTIL_PATTERN.bin, exclude = 'Missing')
levels(credit_card_default$UTIL_PATTERN_bin)<- c('0', '1-3' )
table(credit_card_default$UTIL_PATTERN_bin)
class(credit_card_default$UTIL_PATTERN_bin)

train$UTIL_PATTERN_bin<- credit_card_default[credit_card_default$data.group==1,64]

WOE_UTIL_PATTERN<- woe.df$woe.UTIL_PATTERN.binned

describe(credit_card_default)
head(train)
```
# Bivariate analysis. Numeric variables 
```{r}
numeric <- c(2,7,8,9:21,24:30,36,38, 39, 41,42)

count_i<- 1

for(column in train[numeric]){
  print(ggplot(train, aes(x=as.factor(train$DEFAULT), y= column)) + 
    geom_boxplot(color = 'blue') +
    ggtitle(sprintf("BoxPlot of DEFAULT vs %s",names(numeric[count_i])))+
    ylab(sprintf("%s",names(train[numeric][count_i])))+
    xlab("DAFAULT")+
    stat_smooth()+
    theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)))
  count_i = count_i+1 
}

#Trace Pmt_ratio change over time 
table<-apply(train[9:14],2, function(x){aggregate(x, by = list(train$DEFAULT),FUN= median)})

meadian_pmt_ratio<- drop_columns(as.data.frame(table), c(3,5,7,9,11))
colnames(meadian_pmt_ratio)<- c("DEFAULT", 'Pmt_Ratio_2', 'Pmt_Ratio_3', 'Pmt_Ratio_4', 'Pmt_Ratio_5', 'Pmt_Ratio_6', 'Avg_Pmt_Ratio')

file.name <- 'table_4.html'
out.path = 'C:\\Users\\yanin\\Desktop\\Capstone\\Project Output\\'

stargazer(meadian_pmt_ratio, type=c('html'),out=paste(out.path,file.name,sep=''),
	title=c('Table : The chang? in Median of Pmt_Ratio variable over 6-month period'),
	align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, summary = FALSE)

#Trace Util change over time 

table_2<-apply(train[15:21],2, function(x){aggregate(x, by = list(train$DEFAULT),FUN= median)})

meadian__util<- drop_columns(as.data.frame(table_2), c(3,5,7,9,11,13))
colnames(meadian__util)<- c("DEFAULT", 'Util_1', 'Util_2', 'Util_3', 'Util_4', 'Util_5', 'Util_6', "Avg_Util")

file.name <- 'table_5.html'
out.path = 'C:\\Users\\yanin\\Desktop\\Capstone\\Project Output\\'

stargazer(meadian__util, type=c('html'),out=paste(out.path,file.name,sep=''),
	title=c('Table : The change in Median of Util variable over 6-month period'),
	align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, summary = FALSE)
```
# Corrplot

```{r cor_matrix, fig.height=10}
library(corrplot)
library(DataExplorer)
numeric <- c(1,2,7,8,9,14,15,21,24:30,36,38, 39, 41,42)
discrete <- c(1,4,6,22,23,31,33:35,37,40,43:45)
# Scatterplot Matrix
mcor <- cor(train[numeric])
corrplot(mcor, method="number", shade.col=NA, tl.col="black",tl.cex=0.8)

# Plot correlation matrix using spearman correlation coefficient 

plot_correlation(train[numeric], cor_args = list("use" = "pairwise.complete.obs", "method"= "spearman"))

plot_correlation(train[d?screte], cor_args = list("use" = "pairwise.complete.obs", "method"= "spearman"))
```
# Explore the relationship between categorical and DEFAULT variables. 
```{r}
count_i<- 1
for (column in train[discrete]){
  print(names(train[discrete][count_i]))
  prin?(with(train, tapply(DEFAULT,column, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
})))
  count_i= count_i+1
}
```
# Visualize the relationship between categorical and DEFAULT variables. 

```{r}
count_i<- 1

for(column in train[discrete]){
  print(ggplot(train, aes(DEFAULT, fill = column)) +
  geom_histogram(binwidth=.5, position="dodge")+
  ggtitle(sprintf("Distribution of %s",names(train[discrete][count_i]))))
  count_i = count_i+1
  }
```
#Bin Util_Growth_6mo
```{r}
Util_Growth_6mo.bin<- woe.binning(df=credit_card_default,target.var=c('DEFAULT'),pred.var=c('Util_Growth_6mo'), event.class = '1')
woe.binning.plot(Util_Growth_6mo.bin)
woe.df <- woe.binning.deploy(df=credit_card_default,binning=Util_Growth_6mo.bin, add.woe.or.dum.var='woe')
head(woe.df)
table(woe.df$Util_Growth_6mo.bin)
woe.binning.table(Util_Growth_6mo.bin)

# Binning based on entropy - decision tree;
bin.5 <- optbin(credit_card_default$DEFAULT ~ credit_card_default$Util_Growth_6mo,method=c('infogain'));
table(bin.5)
aggregate(credit_card_default$DEFAULT, by=list(Util_Growth_6mo=bin.5[,1]), FUN=mean)

credit_card_default$Util_Growth_6mo_bin <- droplevels(woe.df$Util_Growth_6mo.bin, exclude = 'Missing')
levels(credit_card_default$Util_Growth_6mo_bin)<- c('Large Negative Growth'? 'Small Negative Growth', 'Mod Negative Growth','Mod Positive Growth','Large Positive Growth')
table(credit_card_default$Util_Growth_6mo_bin)

train$Util_Growth_6mo_bin<- credit_card_default[credit_card_default$data.group==1,65]
head(train)

WOE_Util_Growth_6mo<- woe.df$woe.Util_Growth_6mo.binned

ggplot(train, aes(DEFAULT, fill = Util_Growth_6mo_bin)) +
  geom_histogram(binwidth=.5, position="dodge")+
  ggtitle('Distribution of Util_Growth_6mo_bin')

range(credit_card_default$Util_Growth_6mo)

```
# Explore data after additional transformation has been performed and add WOE variables 

```{r}
credit_card_default[66:76]<- c(woe_age, WOE_Freq_PAY, WOE_Avg_Util,WOE_Bal_Growth_6mo, WOE_LIMIT_BAL, WOE_MAX_DLQ, WOE_OVER_LIMIT, WOE_OVER_PMT, WOE_REPAY_PATTERN, WOE_Util_Growth_6mo, WOE_UTIL_PATTERN)
names(credit_card_default)[66:76]<-c('WOE_AGE', 'WOE_Freq_PAY','WOE_Avg_Util','WOE_Bal_Growth_6mo', 'WOE_LIMIT_BAL', 'WOE_MAX_DLQ', 'WOE_OVER_LIMIT','WOE_OVER_PMT', 'WOE_REPAY_PATTERN', 'WOE_Util_Growth_6mo', 'WOE_UTIL_PATTE?N')

train[47:57]<-credit_card_default[credit_card_default$data.group==1,66:76]
describe(train)

```
# Transform test data 

```{r}
credit_card_default$sqrt_LIMIT_BAL<- sqrt(credit_card_default$LIMIT_BAL)
credit_card_default$Avg_Bill_Amt_tfm<-credit_card_default$Avg_Bill_Amt 
credit_card_default$Avg_Bill_Amt_tfm[(credit_card_default$Avg_Bill_Amt_tfm > 302472)]<-302472
credit_card_default$Avg_Bill_Amt_tfm[(credit_card_default$Avg_Bill_Amt_tfm< 0)]<- 0
credit_card_default$Avg_Bill_Amt_tfm<-yjPower(credit_card_default$Avg_Bill_Amt_tfm, 0.2, jacobian.adjusted = FALSE)
credit_card_default$Avg_Pmt_Amt_tfm<- yjPower(credit_card_default$Avg_Pmt_Amt, 0, jacobian.adjusted = FALSE)
credit_card_default$Max_Bill_Amt_tfm<-credit_card_default$Max_Bill_Amt 
credit_card_default$Max_?ill_Amt_tfm[(credit_card_default$Max_Bill_Amt_tfm > 374259.3 )]<-374259.3 
credit_card_default$Max_Bill_Amt_tfm[(credit_card_default$Max_Bill_Amt_tfm< 0)]<- 0
credit_card_default$Max_Bill_Amt_tfm<-yjPower(credit_card_default$Max_Bill_Amt_tfm, 0.2, jacobi?n.adjusted = FALSE)
credit_card_default$Max_Pmt_Amt_tfm<- yjPower(credit_card_default$Max_Pmt_Amt, 0, jacobian.adjusted = FALSE)

index_2 <- names(train)

test <- credit_card_default[credit_card_default$data.group ==2,index_2]

head(test)
```
# Relevel MARRIAGE and EDUCATION variables

```{r}

#relevel MARRIAGE variable 

contrasts(train$MARRIAGE)

train <- train %>%
  mutate(MARRIAGE = relevel(MARRIAGE, ref = "3"))
glm(DEFAULT~ MARRIAGE, data = train, family = binomial)

summary(train$MARRIAGE)

# relevel EDUCATION variable 

contrasts(train$EDUCATION)
summary(train$EDUCATION)

train <- train %>%
  mutate(EDUCATION = relevel(EDUCATION, ref = "4"))
glm(DEFAULT~ EDUCATION, data = train, family = binomial)

summary(train$EDUCATION)

train$DEFAULT<- as.factor(train$DEFAULT)

# Perform the same for test data 

#relevel test Data
contrasts(test$MARRIAGE)

test <- test %>%
  mutate(MARRIAGE = relevel(MARRIAGE, ref = "3"))
glm(DEFAULT~ MARRIAGE, data = test, family = binomial)

summary(test$MARRIAGE)

# relevel EDUCATION variable 

contrasts(test$EDUCATION)
summary(test$EDUCATION)

test <- test %>%
  mutate(EDUCATION = relevel(EDUCATION, ref = "4"))
glm(DEFAULT~ EDUCATION, data = test, family = binomial)

summary(test$EDUCATION)

test$DEFAULT<- as.factor(test$DEFAULT)

```
#  Create subsets of data sets 

```{r}
# train data 

describe(train)
train_original<- train[-c(33,36:57)]  
train_bin<- train[-c(2,7,8,21,24,25,29,30,32,34,35,47:57)]# 25 - sqrt_LIM_BAL # train data with binned variables 
train_WOE <- train[-c(2,6,7,8,21,22,23,24,25,29,30,31,32,33,34,35,37,40,43,44:46)]#20 - sqrt_LIM_BAL # train data 
# binned variables replaced by their respective WOE values
subset<- train_bin[-c(6:10,12:17)]

# test data 

test_original<- test[-c(33,36:57)]
test_bin<- test[-c(2,7,8,21,24,25,29,30,32,34,35,47:57)]# 25 - sqrt_LIM_BAL
test_WOE <- test[-c(2,6,7,8,21,22,23,24,25,29,30,31,32,33,34,35,37,40,43,44:46)]#20 - sqrt_LIM_BAL
subset_test<- test_bin[-c(6:10,12:17)]

```
# 2. Model - Based EDA

#Enhanced tree plot for variable selection 
```{r}
library(rattle) # Fancy tree plot
library(rpart.plot) # Enhanced tree plots
library(RColorBrewer) # Color selection for fancy tree plot
library(party) # A?ternative decision tree algorithm
library(partykit) # Convert rpart object to BinaryTree
library(caret) # Just a data source for this script


# Make big tree
form <- as.formula(DEFAULT ~ .)
tree.1 <- rpart(form,data=train_original, control=rpart.control(cp = 0.00098))
#
plot(tree.1) # Will make a mess of the plot
text(tree.1)
#
prp(tree.1,type= 2, extra=106, nn=TRUE, ni=TRUE) # Will plot the tree
printcp(tree.1)
summary(tree.1)

# Interatively prune the tree
new.tree.1 <- prp(tree.1,snip=TRUE)$obj # interact?vely trim the tree
prp(new.tree.1,  fallen.leaves = FALSE ) # display the new tree

summary(new.tree.1)
fancyRpartPlot(new.tree.1,tweak = 1.8, space = 0) # A fancy plot from rattle
var_imp<- as.data.table(tree.1$variable.importance, keep.rownames=TRUE)
file.name <- 'tree_1.html'
out.path = 'C:\\Users\\yanin\\Desktop\\Capstone\\Project Output\\'

stargazer(var_imp, type=c('html'),out=paste(out.path,file.name,sep=''),
	title=c('Table : Variable Importance'),
	align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, summary = FALSE)

df<- data.frame(imp=tree.1$variable.importance)
df_2 <- df %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

ggplot(df_2) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = imp, col = variable), 
             size = 3, show.legend = F) +
  coord_flip() +
  theme_bw()

```
Building trees with different combination of variables 

```{r}

# Make big tree
form <- as.formula(DEFAULT ~ .)
tree.1 <- rpart(form,data=train_bin, control=rpart.control(cp= 0.0014))

plot(tree.1) # Will make a mess of the plot
text(tree.1)

prp(tree.1,type= 2, extra=106, nn=TRUE, ni=TRUE) # Will plot the tree
printcp(tree.1)

# Interatively prune the tree
new.tree.1 <- prp(tree.1,snip=TRUE)$obj # interactively trim the tree
prp(new.tree.1 ) # display the new tree

summary(new.tree.1)
fancyRpartPlot(new.tree.1,tweak = 1.8, space = 0) # A fancy plot from rattle

var_imp_2<- as.data.table(tree.1$variable.importance, keep.rownames=TRUE)
file.name <- 'tree_2.html'
out.path = 'C:\\Users\\yanin\\Desktop\\Capstone\\Project Output\\'

stargazer(var_imp_2, type=c('html'),out=paste(out.path,file.name,sep=''),
	title=c('Table : Variable Importance'),
	align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, summary = FALSE)


df<- data.frame(imp=tree.1$variable.importance)
df_2 <- df %>% 
  tibble::rown?mes_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

ggplot(df_2) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
               size ? 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = imp, col = variable), 
             size = 3, show.legend = F) +
  coord_flip() +
  theme_bw()

#Building trees with different combination of variables 

form <- as.formula(DEFAULT ~ .)
tree.1 <- rpart(form,data=subset, control=rpart.control(cp= 0.0014))
plot(tree.1) 
text(tree.1)
#
prp(tree.1,type= 2, extra=106, nn=TRUE, ni=TRUE) # Will plot the tree
printcp(tree.1)


# Interatively prune the tree
new.tree.1 <- prp(tree.1,snip=TRUE)$obj # interactively trim the tree
prp(new.tree.1 ) # display the new tree

summary(new.tree.1)
fancyRpartPlot(new.tree.1) # A fancy plot from rattle


var_imp_subset<- as.data.table(tree.1$variable.importance, keep.rownames=TRUE)
file.name <- 'tree_3.html'
out.path = 'C:\\Users\\yanin\\Desktop\\Capstone\\Project Output\\'

stargazer(var_imp_subset, type=c('html'),out=paste(out.path,file.name,sep=''),
	title=c('Table : Variable Importance'),
	align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, summary = FALSE)

```
# Perform OneR for variable importance 
```{r}

model.1 <- OneR::OneR(DEFAULT ~ ., data=train_bin, verbose=TRUE)
summary(model.1)
###########################################################################
# Print out a confusion matrix;
###########################################################################
y.hat <- predict(model.1,newdata=test_bin);
y <- test_bin$DEFAULT

OneR::eval_model(prediction=y.hat, actual=y, dimnames=c('Prediction','Actual'))

```
# RANDOM FOREST FEATURE SELECTION
```{r}
library(randomForest)
fit_rf = randomForest(DEFAULT~., data=train_WOE)
# Create an importance based on mean decreasing gini
randomForest::importance(fit_rf)

varImpPlot(fit_rf,sort=TRUE, n.var= 10)

```
# Extract variables' importance using caret() and compare with random forest results
```{r}
library(caret)
# compare the feature importance with varImp() function
varImp(fit_rf)
 
# Create a plot of importance scores by random forest
varImpPlot(fit_rf)
```
#Using Boruta algorthm to compare results obtained with random forest. 

```{r}
library(Boruta)
set.seed(111)
boruta.bank <- Boruta(train$DEFAULT~.,data = train_original[,-1], doTrace = 2)
print(boruta.bank)
```
#Fix tentative attributes 
```{r}
boruta.bank_tnt <- TentativeRoughFix(boruta.bank)
print(boruta.bank_tnt)
```
#Boruta important attributes 
```{r}
getSelectedAttributes(boruta.bank, withTentative = F)
bank_df <- attStats(boruta.bank)
print(bank_df) 
bank_df<- data.frame(bank_df)

file.name <- 'boruta.html'
out.path = 'C:\\Users\\yanin\\Desktop\\Capstone\\Project Out?ut\\'

stargazer(bank_df, type=c('html'),out=paste(out.path,file.name,sep=''),
	title=c('Table : Variable Importance'),
	align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, summary = FALSE)
```
# Model Development.
## 1. Random Forest  
```{r}
library(ParamHelpers)
library(mlr)

#Create a Task. Exclude LIMIT_BAL_bin variable

trainTask <- makeClassifTask(data = train_bin[,-26],target = "DEFAULT", positive = "1")
testTask <- makeClassifTask(data = test_bin[,-26], target = "DEFAULT", positive = "1")

#normalize the variables
trainTask <- normalizeFeatures(trainTask,method = "standardize")
testTask <- normalizeFeatures(testTask,method = "standardize")

im_feat <- generateFilterValuesData(trainTask, method = 'FSelectorRcpp_gain.ratio')
plotFilterValues(im_feat)

```
Random Forest. Train_bin data  

```{r}
set.seed(1001)
getParamSet("classif.randomForest")
rf <- makeLearner("classif.randomForest", predict.type = "prob", predict.threshold = 0.17, par.vals = list(ntree = 200, mtry = 3))
rf$par.vals <- list(importance = TRUE)

#set tunable parameters
#use random and grid search to find optimal hyperparameters

rf_param <- makeParamSet(
makeIntegerParam("ntree",lower = 50, upper = 1000),
makeIntegerParam("mtry", lower = 3, upper = 33),
makeIntegerParam("nodesize", lower = 10, upper = 100)
)

#let's do Random search for 100 iterations
rancontrol <- makeTuneControlRandom(tune.threshold = TRUE)

# perform the hyperparameters search in parallel 
library(parallelMap)
library(parallel)
parallelStartSocket(cpus = detectCores()-1)

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV", iters = 3L)

#hypertuning to maximize TPR
rf_tune <- tuneParams(learner = rf, resampling = set_cv, task = trainTask, par.set = rf_param, control = rancontrol, measures = tpr, show.info = T)

# CV accuracy 
rf_tune$y
#best parameters
par_1<- rf_tune$x

#perform Gridsearch 

gridcontrol<- makeTuneControlGrid()
rf_param <- makeParamSet(
makeIntegerParam("ntree",lower = 50, upper = 500),
makeIntegerParam("mtry", lower = 3, upper = 20),
makeIntegerParam("nodesize", lower = 10, upper = 100)
)

# finetune to maximize accuracy 

rf_tune_2 <- tuneParams(learner = rf, resampling = set_cv, task = trainTask, par.set = rf_param, control = gridncontrol, measures = acc, show.info = T)

par_2<- rf_tune_2$x

rf_tune_2$y

#Build the random forest models using hyperparameters obtained from Gridsearch nad Randomsearch
#and check their performances.

# Random Forest with hyperparameters obtained from RandomSearch 
rf.tree <- setHyperPars(rf, par.vals = rf_tune$x)
rdesc <- makeResampleDesc("CV", iters = 10, stratify = TRUE)
rf_c <- resample(learner = rf.tree, task = trainTask, resampling = rdesc, measures = list(tpr,fpr,fnr,tpr,acc, f1, mlr::auc, ppv), show.info = T)

# Random Forest with hyperparameters obtained from GridSearch 
rf.tree_2 <- setHyperPars(rf, par.vals = rf_tune_2$x)
rdesc <- makeResampleDesc("CV", iters = 10)
r_2 <- resample(learner = rf.tree_2, task = trainTask, resampling = desc, measures = list(tpr,fpr,fnr,fpr,acc, f1), show.info = T)

#train Random Forest  with hyperparameters obtained from RandomSearch 

rforest <- train(rf.tree, trainTask)

#make predictions and evaluate model's in sample and out-of-sample performances 

rfmodel_in_sample <- predict(rforest, trainTask)
rfmodel <- predict(rforest, testTask)

calculateConfusionMatrix(rfmodel, relative = TRUE, sums = TRUE, set = "both")
calculateConfusionMatrix(rfmodel_in_sample, relative = TRUE, sums = TRUE, set = "both")

# Calculate Area Under ROC Curve
ROCR::performance(asROCRPrediction(rfmodel_in_sample), "auc")


# RandomSearch hypertuning with maximizing f1 
rf_tune_f1 <- tuneParams(learner = rf, resampling = set_cv, task = trainTask, par.set = rf_param, control = rancontrol, measures = f1, show.info = T)
par_tune_tpr<-rf_tune_f1$x
rf_tune_f1$y

rf.tree_f1 <- setHyperPars(rf, par.vals = rf_tune_f1$x)
rf_tune_f1<- resample(learner = rf.tree_f1, task = trainTask, resampling = rdesc, measures = list(tpr,fpr,fnr,fpr,acc, f1, ppv, mlr::auc ), show.info = T)

#train a model
rforest_f1 <- train(rf.tree_f1, trainTask)

#make predictions and evaluate the model's in sample and out-of-sample performances 

rfmodel_f1.in_sample <- predict(rforest_f1, trainTask)
tuneThreshold(rfmodel_f1.in_sample, f1)
rfmodel_f1.in_sample<- setThreshold(rfmodel_f1.in_sample, 0.1035395)
# Calculate Area Under ROC Curve
ROCR::performance(asROCRPrediction(rfmodel_f1.in_sample), "auc")
calculateConfusionMatrix(rfmodel_f1.in_sample, relative = TRUE, sums = TRUE, set = "both")


rfmodel_f1<- predict(rforest_f1, testTask)
tuneThreshold(rfmodel_f1, f1)
rfmodel_f1<- setThreshold(rfmodel_f1, 0.2280686)
calculateConfusionMatrix(rfmodel_f1, relative = TRUE, sums = TRUE, set = "both")

```
# ROC. Precision/Recall curve. KS Statistic. Rfmodel

```{r}
library(ROCR)
## ROC Curve and Area under ROC
##-------------------------------------------------------------------------------

# Create prediction object for the training dataset
predition_rfmodel <- asROCRPrediction(rfmodel)

# get data for ROC Curve
rocData.rfmodel <- ROCR::performance (predition_rfmodel, "tpr", "fpr")

# Calculate Area Under ROC Curve
auc.rfmodel<- ROCR::performance(predition_rfmodel, "auc")
auc.rfmodel <- unlist(slot(auc.rfmodel, "y.values"))
print(auc.rfmodel)

# Plot ROC curve
plot(rocData.rfmodel, colorize = T,
main = "ROC Curve for Random_Forest_Model_1 \n Train_bin Data",
ylab = "Sensitivity (TPR)",
xlab = "1-Specificity (FPR)")
legend (0.8, 0.4, round(auc.rfmodel,4), title = "AUC", cex = 1.0, col = "blue")
# Draw line in the middle
abline(a=0, b = 1)

## Precision vs Recall (TPR)
precObj.rfmodel <- ROCR::performance (predition_rfmodel, "prec", "rec")
plot(precObj.rfmodel, colorize = T,
main = "Precision- Recall Curve for Random_Forest_Model_1 \n Train_bin Data",
ylab = "Precision", xlab = "Recall (TPR)")
legend (0.2, 0.5, round(auc.rfmodel,4), title = "AUC", cex = 1.0, col = "blue")


## KS Statistic
TPR.rfmodel <- unlist(slot(rocData.rfmodel, "y.values"))
FPR.rfmodel <- unlist(slot(rocData.rfmodel, "x.values"))
diffTPR.FPR.rfmodel<- TPR.rfmodel-FPR.rfmodel
KS.rfmodel <- max(diffTPR.FPR.rfmodel)

```
# ROC. Precision/Recall curve. KS Statistic. Rfmodel_f1

```{r}
## ROC Curve and Area under ROC

# Create prediction object for the training dataset
predition_rfmodel_f1 <- asROCRPrediction(rfmodel_f1)

# get data for ROC Curve
rocData.rfmodel_f1 <- ROCR::performance (predition_rfmodel_f1, "tpr", "fpr")

# Calculate Area Under ROC Curve
auc.rfmodel_f1<- ROCR::performance(predition_rfmodel_f1, "auc")
auc.rfmodel_f1 <- unlist(slot(auc.rfmodel_f1, "y.values"))
print(auc.rfmodel_f1)

# Plot ROC curve
plot(rocData.rfmodel_f1, colorize = T,
main = "ROC Curve for Random_Forest_Model_2 \n Train_bin Data",
ylab = "Sensitivity (TPR)",
xlab = "1-Specificity (FPR)")
legend (0.8, 0.4, round(auc.rfmodel_f1,4), title = "AUC", cex = 1.0, col = "blue")
# Draw line in the middle
abline(a=0, b = 1)

## Precision vs Recall (TPR)
precObj.rfmodel_f1 <- ROCR::performance (predition_rfmodel_f1, "prec", "rec")
plot(precObj.rfmodel_f1, colorize = T,
main = "Precision Curve for Random_Forest_Model_2 \n Train_bin Data",
ylab = "Precision", xlab = "Recall (TPR)")
legend (0.2, 0.5, round(auc.rfmodel_f1,4), title = "AUC", cex = 1.0, col = "blue")

## KS Statistic
TPR.rfmodel_f1 <- unlist(slot(rocData.rfmodel_f1, "y.values"))
FPR.rfmodel_f1 <- unlist(slot(rocData.rfmodel_f1, "x.values"))
diffTPR.FPR.rfmodel_f1<- TPR.rfmodel_f1-FPR.rfmodel_f1
KS.rfmodel_f1 <- max(diffTPR.FPR.rfmodel_f1)

```
Random Forest. Train_original data

```{r}

##Create task
set.seed(1001)
#create a task. Exclude LIMIT_BAL_bin variable
trainTask_2 <- makeClassifTask(data = train_original[,-26],target = "DEFAULT", positive = "1")
testTask_2 <- m?keClassifTask(data = test_original[,-26], target = "DEFAULT", positive = "1")

#normalize the variables
trainTask_2 <- normalizeFeatures(trainTask_2,method = "standardize")
testTask_2 <- normalizeFeatures(testTask_2,method = "standardize")
```
#use random search to find optimal hyperparameters

```{r}
rf_param <- makeParamSet(
makeIntegerParam("ntree",lower = 50, upper = 1000),
makeIntegerParam("mtry", lower = 3, upper = 33),
makeIntegerParam("nodesize", lower = 10, upper = 100)
)

rf_tune_2 <- tuneParams(learner = rf, resampling = set_cv, task = trainTask_2, par.set = rf_param, control = rancontrol, measures = f1, show.info = T)

#Now, we have the final parameters. Let's check the list of parameters and CV accuracy.

rf_tune_2$y
#best parameters
par_org_2<- rf_tune_2$x

#Let's build the random forest model using hyperparameters obtained from 
# randomsearch and evaluate its performance

rf.tree_2 <- setHyperPars(rf, par.vals = rf_tune_2$x)
r_2 <- resample(learner = rf.tree_2, task = trainTask_2, resampling = rdesc, measures = list(tpr,fpr,fnr,fpr,acc), show.info = T)


#train a model 
rforest_2 <- train(rf.tree_2, trainTask_2)

#make predictions
rfmodel_2 <- predict(rforest_2, testTask_2)
calculateConfusionMatrix(rfmodel_2, relative = TRUE, sums = TRUE, set = "both")
## ROC Curve and Area under ROC. Rfmodel_2
```
## ROC Curve and Area under ROC. KS- statistics. Rfmodel_2

```{r}
# Create prediction object for the training dataset
predition_rfmodel_2 <- asROCRPrediction(rfmodel_2)

# get data for ROC Curve
rocData.rfmodel_2 <- ROCR::performance (predition_rfmodel_2, "tpr", "fpr")

# Calculate Area Under ROC Curve
auc.rfmodel_2<- ROCR::performance(predition_rfmodel_2, "auc")
auc.rfmodel_2 <- unlist(slot(auc.rfmodel_2, "y.values"))
print(auc.rfmodel_2)

# Plot ROC curve
plot(rocData.rfmodel_2, colorize = T,
main = "ROC Curve for Random_Forest_Model \n Train_Original Data",
ylab = "Sensitivity (TPR)",
xlab = "1-Specificity (FPR)")
legend (0.8, 0.4, round(auc.rfmodel_2,4), title = "AUC", cex = 1.0, col = "blue")
# Draw line in the middle
abline(a=0, b = 1)

## Precision vs Recall (TPR)
precObj.rfmodel_2 <- ROCR::performance (predition_rfmodel_2, "prec", "rec")
plot(precObj.rfmodel_2, colorize = T,
main = "Precision- Recall Curve for Random_Forest_Model \n Train_Original Data",
ylab = "Precision", xlab = "Recall (TPR)")
legend (0.2, 0.5, round(auc.rfmodel_2,4), title = "AUC", cex = 1.0, col = "blue")

## KS Statistic
TPR.rfmodel_2 <- unlist(slot(rocData.rfmodel_2, "y.values"))
FPR.rfmodel_2 <- unlist(slot(rocData.rfmodel_2, "x.values"))
diffTPR.FPR.rfmodel_2<- TPR.rfmodel_2-FPR.rfmodel_2
KS.rfmodel_2 <- max(diffTPR.FPR.rfmodel_2)
KS.rfmodel_2

```
# Champion Random Forest model. Feature importance plots. 

```{r}
# Feature importance measured f1 
champion_imp<- generateFeatureImportanceData(trainTask, method = "permutation.importance", rf.grid.tuned, features = getTaskFeatureNames(trainTask), interaction = FALSE, f1)

df<- data.frame(results)
df_2 <- df %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(f1) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
  
ggplot(df_2) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = f1), 
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = f1, col = variable), 
             size = 3, show.legend = F) +
  coord_flip() +
  theme_bw()

# Feature importance measured by mean decrease in Gini impurity 

rf_imp <- getFeatureImportance(rf_grid_model)
results_2<- t(rf_imp$res)
feature_imp<- data.frame(imp= results_2)
df_3 <- feature_imp %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

ggplot(df_3) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = imp, col = variable), 
             size = 3, show.legend = F) +
  coord_flip() +
  theme_bw()

# Feature importance measured by decrease in tpr

champion_imp<- generateFeatureImportanceData(trainTask, method = "permutation.importance", rf.tree_f1, features = getTaskFeatureNames(trainTask), interaction = FALSE, tpr)

results_tpr <- t(champion_imp$res)
df_tpr<- data.frame(results_tpr)
df_tpr <- df_tpr%>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr:?arrange(tpr) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

ggplot(df_tpr) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = tpr), 
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = tpr, col = variable), 
             size = 3, show.legend = F) +
  coord_flip() +
  theme_bw()

```
# XGboost

XGboost Model 1 fine-tuned using RandomSearch to maximize F1. Train_bin data 

```{r}
train_one_hot <- createDummyFeatures(
  train_bin, target = "DEFAULT")

test_one_hot <- createDummyFeatures(
  test_bin, target = "DEFAULT")


summarizeColumns(train_one_hot) %>%
  kable(digits = 2)

#create a task. Exclude LIMIT_BAL_bin variable
trainTask_enc <- makeClassifTask(data = train_one_hot[,-26],target = "DEFAULT", positive = "1")
testTask_enc <- makeClassifTask(data = test_one_hot[,-26], target = "DEFAULT", positive = "1")

#normalize the variables
trainTask <- normalizeFeatures(trainTask,method = "standardize")
testTask <- normalizeFeatures(testTask,method = "standardize")

#load xgboost
set.seed(1001)
getParamSet("classif.xgboost")

#make learner with inital parameters
xg_set <- makeLearner("classif.xgboost", predict.type = "prob")
xg_set$par.vals <- list(
objective = "binary:logistic",
eval_metric = "auc",
nrounds = 250
)
#define parameters for tuning
xg_ps <- makeParamSet(
makeIntegerParam("nrounds",lower=200,upper=600),
makeIntegerParam("max_depth",lower=3,upper=15),
makeNumericParam("lambda",lower=0.55,upper=0.60),
makeNumericParam("eta", lower = 0.001, upper = 0.5),
makeNumericParam("subsample", lower = 0.10, upper = 0.80),
makeNumericParam("min_child_weight",lower=1,upper=10),
makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8)
)
#define search function
rancontrol <- makeTuneControlRandom(maxit = 100L,tune.threshold = TRUE) #do 100 iterations

#3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)
#tune parameters
xg_tune <- tuneParams(learner = xg_set, task = trainTask, resampling = set_cv,measures = f1, par.set = xg_ps, control = rancontrol)
#set parameters
xg_new <- setHyperPars(learner = xg_set, par.vals = xg_tune$x)

xg_cv <- resample(learner = xg_new, task = trainTask, resampling = rdesc, measures = list(tpr,fpr,fnr,fpr,acc), show.info = T)

#train model
xgmodel <- train(xg_new, trainTask)
#test model
predict.xg <- predict(xgmodel, testTask)

calculateConfusionMatrix(predict.xg, relative = TRUE, sums = TRUE, set = "both")

```
# Xgboost Model 1. ROC and Precision - Recall Curve. KS statistic

```{r}

# Create prediction object for the training dataset
predition_xg <- asROCRPrediction(predict.xg)

# get data for ROC Curve
rocData.xg<- ROCR::performance (predition_xg, "tpr", "fpr")

# Calculate Area Under ROC Curve
auc.xg<- ROCR::performance(predition_xg, "auc")
auc.xg<- unlist(slot(auc.xg, "y.values"))
print(auc.xg)

# Plot ROC curve
plot(rocData.xg, colorize = T,
main = "ROC Curve for Xgboost_Model \n Train_Bin Data",
ylab = "Sensitivity (TPR)",
xlab = "1-Specificity (FPR)")
legend (0.8, 0.4, round(auc.xg,4), title = "AUC", cex = 1.0, col = "blue")
# Draw line in the middle
abline(a=0, b = 1)

## Precision vs Recall (TPR)
precObj.xg <- ROCR::performance (predition_xg, "prec", "rec")
plot(precObj.xg, colorize = T,
main = "Precision- Recall Curve for Xgboost \n Train_Bin Data",
ylab = "Precision", xlab = "Recall (TPR)")
legend (0.2, 0.5, round(auc.xg,4), title = "AUC", cex = 1.0, col = "b?ue")


## KS Statistic
TPR.xg <- unlist(slot(rocData.xg, "y.values"))
FPR.xg <- unlist(slot(rocData.xg, "x.values"))
diffTPR.FPR.xg<- TPR.xg-FPR.xg
KS.xg <- max(diffTPR.FPR.xg)
KS.xg

```
XGboost Model 2. Fine-tuned using RandomSearch to maxinmize TPR  

```{r}
#load xgboost
set.seed(1001)

#tune parameters to maximize TPR
xg_tune_tpr <- tuneParams(learner = xg_set, task = trainTask, resampling = set_cv,measures = tpr, par.set = xg_ps, control = rancontrol)
#set parameters
xg_tpr <- setHyperPars(learner = xg_set, par.vals = xg_tune_tpr$x)

xg_cv_tpr <- resample(learner = xg_tpr, task = trainTask, resampling = rdesc, measures = list(tpr,fpr,fnr,fpr,acc), show.info = T)

#train model
xgmodel_2 <- train(xg_tpr, trainTask)
#test model
predict_2.xg <- predict(xgmodel_2, testTask)

calculateConfusionMatrix(predict_2.xg, relative = TRUE, sums = TRUE, set = "both")
xg_tune_tpr$threshold

```
XGboost Model 3 fine-tuned using RandomSearch to maximize F1. Train_original Data 

```{r}

train_original_enc<- createDummyFeatures(
  train_original, target = "DEFAULT")

test_original_enc <- createDummyFeatures(
  test_original, target = "DEFAULT")

#create a task. Exclude LIMIT_BAL_bin variable
trainTask_2 <- makeClassifTask(data = train_one_hot[,-26],target = "DEFAULT", positive = "1")
testTask_2 <- makeClassifTask(data = test_o?e_hot[,-26], target = "DEFAULT", positive = "1")

#normalize the variables
trainTask_2 <- normalizeFeatures(trainTask,method = "standardize")
testTask_2 <- normalizeFeatures(testTask,method = "standardize")

#load xgboost
set.seed(1001)

#tune parameters
xg_tune_2 <- tuneParams(learner = xg_set, task = trainTask_2, resampling = set_cv,measures = f1, par.set = xg_ps, control = rancontrol)
#set parameters
xg_2 <- setHyperPars(learner = xg_set, par.vals = xg_tune_2$x)

xg_cv_2 <- resample(learner = xg_2, task = trainTask_2, resampling = rdesc, measures = list(tpr,fpr,fnr,fpr,acc), show.info = T)

#train model
xgmodel_org<- train(xg_2, trainTask_2)

#test model
predict_org.xg <- predict(xgmodel_org, testTask_2)
calculateConfusionMatrix(predict_org.xg, relative = TRUE, sums = TRUE, set = "both")
```
# Feature importance plot for XGmodel 1 measured by mean decrease in Gini impurity 

```{r}
xg_imp <- getFeatureImportance(xgmodel)
results_xg<- t(xg_imp$res)
impFeature.xg<- data.frame(i?p=results_xg)
df_4 <- impFeature.xg %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

ggplot(df_4) +
  geom_segment(aes(x = variable, y ? 0, xend = variable, yend = imp), 
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = imp, col = variable), 
             size = 3, show.legend = F) +
  coord_flip() +
  theme_bw()
```
# Ensemble module 1. Stacking of Random Forest and XGboost models both fine-tuned 
# to maximize F1 using RandomSearch 

```{r}
rdesc <- makeResampleDesc("CV", iters = 10,stratify= TRUE)
learners<- list(rf.tree_f1,xg_new)
stack_learner <- makeStackedLearner(learners, super.learner = rf.tree_f1,predict.type = 'prob', method = "stack.cv", resampling = rdesc)

stack_cv <- resample(learner = stack_learner, task = trainTask_enc, resampling = rdesc, measures = list(tpr,fpr,fnr,fpr,acc), show.info = T)
#train a model
stack_lrn <- mlr::train(stack_learner, trainTask_enc)
#make predictions
stack_lrn.predict <- predict(stack_lrn, testTask_enc)

tuneThreshold(stack_lrn.predict, f1)
stack_lrn.predict<- setThreshold(stack_lrn.predict,0.1521336)

calculateConfusionMatrix(stack_lrn.predict, relative = TRUE, sums = TRUE, set = "both")
```
# Ensemble Model 1. ROC. Precsion - Recall curve. KS- statistic

```{r}

# Create prediction object for the training dataset
predition_lrn <- asROCRPrediction(stack_lrn.predict)

# get data for ROC Curve
rocData.lrn<- ROCR::performance (predition_lrn, "tpr", "fpr")

# Calculate Area Under ROC Curve
auc.lrn<- ROCR::performance(predition_lrn, "auc")
auc.lrn<- unlist(slot(auc.lrn, "y.values"))
print(auc.lrn)

# Plot ROC curve
plot(rocData.lrn, colorize = T,
main = "ROC Curve for Stacked model \n Train_Bin Data",
ylab = "Sensitivity (TPR)",
xlab = "1-Specificity (FPR)")
legend (0.8, 0.4, round(auc.lrn,4), title = "AUC", cex = 1.0, col = "blue")
# Draw line in the middle
abline(a=0, b = 1)

## Precision vs Recall (TPR)
precObj.lrn <- ROCR::performance (predition_lrn, "prec", "r?c")
plot(precObj.lrn, colorize = T,
main = "Precision- Recall Curve for Stacked model \n Train_Bin Data",
ylab = "Precision", xlab = "Recall (TPR)")
legend (0.2, 0.5, round(auc.lrn,4), title = "AUC", cex = 1.0, col = "blue")


## KS Statistic
TPR.lrn <- unlist(slot(rocData.lrn, "y.values"))
FPR.lrn <- unlist(slot(rocData.lrn, "x.values"))
diffTPR.FPR.lrn<- TPR.lrn-FPR.lrn
KS.lrn <- max(diffTPR.FPR.lrn)
KS.lrn

```
# Ensemble Model 2. Stacking of Random Forest fine-tuned using Random Search to maximize TPR and XGboost using GridSearch to maximize F1  
Goal programming approach to ensemble model 
Goal 1 Increase TPR
Goal 2. Increase F1

```
# First need to train another XGboost model 

Xgboost Model fine-tuned using GridSearch to maximize F1

```{r}
set.seed(1001)
getParamSet("classif.xgboost")

                             #make learner with inital parameters

xg_set <- makeLearner("classif.xgboost", predict.type = "prob")
xg_set$par.vals <- list(
objective = "binary:logistic",
eval_metric = "auc",
nrounds = 250
)
                            #define parameters for tuning
xg_ps <- makeParamSet(
makeIntegerParam("nrounds",lower=300,upper=600),
makeIntegerParam("max_depth",lower=3,upper=15),
makeNumericParam("lambda",lower=0.55,upper=0.60),
makeNumericParam("eta", lower = 0.001, upper = 0.5),
makeNumericParam("subsample", lower = 0.10, upper = 0.80),
makeNumericParam("min_child_weight",lower=1,upper=10),
makeNumericParam("colsample_bytree",?ower = 0.2,upper = 0.8)
)
                           #define search function
gridcontrol<- makeTuneControlGrid(tune.threshold = TRUE)

                             
                             #tune parameters
                             
xg_grid_tuned<- tuneParams(learner = xg_set, task = trainTask_enc, resampling = set_cv,measures = f1, par.set = xg_ps, control = racontrol)
#set parameters
xg_grid <- setHyperPars(learner = xg_set, par.vals = xg_grid_tuned$x)

xg_grid_cv <- resample(learner = xg_grid, task = trainTask_enc, resampling = rdesc, measures = list(tpr,fpr,fnr,tnr,acc, ppv,mlr::auc ), show.info = T)

#train model
xg_grid_model <- mlr:: train(xg_grid, trainTask_enc)
```
Evaluate the model's performance 

```{r}

# In-sample performance 

predict_xg.in_sample <- predict(xg_grid_model, trainTask_enc)
predict_xg.in_sample<- setThreshold(predict_xg.in_sample,0.2565574)
tuneThreshold(predict_xg.in_sample, f1)
calculateConfusionMatrix(predict_xg.in_sample, relative = TRUE, sums = TRUE, set = "both")


#Calculate Area Under ROC Curve
ROCR::performance(asROCRPrediction(predict_xg.in_sample), "auc")

rocData<- ROCR::performance (asROCRPrediction(predict_xg.in_sample), "tpr", "fpr")
TPR <- unlist(slot(rocData, "y.values"))
FPR <- unlist(slot(rocData, "x.values"))
diffTPR.FPR<- TPR-FPR
KS <- max(diffTPR.FPR)
KS

#Out-of-sample performance 

predict.xg_grid <- predict(xg_grid_model, testTask_enc)
tuneThreshold(predict.xg_grid, f1)
pr<- setThreshold(predict.xg_grid, 0.2937275)
calculateConfusionMatrix(pr, relative = TRUE, sums = TRUE, set = "both")

#ROC. Precision and Recall Curve

# Create prediction object for the training dataset
predition_xg_grid <- asROCRPrediction(pr)

# get data for ROC Curve
rocData.xg_grid<- ROCR::performance (predition_xg_grid, "tpr", "fpr")

# Calculate Area Under ROC Curve
auc.xg_grid<- ROCR::performance(predition_xg_grid, "auc")
auc.xg_grid<- unlist(slot(auc.xg_grid, "y.values"))
print(auc.xg_grid)

# Plot ROC curve
plot(rocData.xg_grid, colorize = T,
main = "ROC Curve for GridSearch Tuned Xgboost_Model \n Train_Bin Data",
ylab = "Sensitivity (TPR)",
xlab = "1-Specificity (FPR)")
legend (0.8, 0.4, round(auc.xg_grid,4), title = "AUC", cex = 1.0, col = "blue")
# Draw line in the middle
abline(a=0, b = 1)

## Precision vs Recall (TPR)
precObj.xg_grid <- ROCR::performance (predition_xg_grid, "prec", "rec")
plot(precObj.xg_grid, colorize = T,
main = "Precision- Recall Curve for GridSearch tuned Xgboost \n Train_Bin Data",
ylab = "Precision", xlab = "Recall (TPR)")
legend (0.2, 0.5, round(auc.xg_grid,4), title = "AUC", cex = 1.0, col = "blue")

## KS Statistic
TPR.xg <- unlist(slot(rocData.xg_grid, "y.values"))
FPR.xg <- unlist(slot(rocData.xg_grid, "x.values"))
diffTPR.FPR.xg<- TPR.xg-FPR.xg
KS.xg <- max(diffTPR.FPR.xg)
KS.xg

```
# Train Ensemble Model 2

```{r}
rdesc <- makeResampleDesc("CV", iters = 10,stratify = TRUE)
learners<- list(rf.tree,xg_grid)
stack_learner_2 <- makeStackedLearner(learners, super.learner = rf.tree_f1,predict.type = 'prob', method = "stack.cv", resampling = rdesc)

stack_cv_2<- resample(learner = stack_learner, task = trainTask_enc, resampling = rdesc, measures = list(tpr,fpr,fnr,fpr,acc), show.info = T)


#train a model
stack_lrn_2 <- mlr::train(stack_learner_2, trainTask_nc)

#make predictions

stack_lrn_2.predict <- predict(stack_lrn_2, testTask_enc)

mlr::tuneThreshold(stack_lrn_2.predict, mlr::f1)
stack_lrn_2.predict<- mlr::setThreshold(stack_lrn.predict,0.1521336)

mlr::calculateConfusionMatrix(stack_lrn_2.predict, relative = TRUE, sums = TRUE, set = "both")
```
# Ensemble Model 2. ROC Curve and Area under ROC, KS statistic
```{r}

# Create prediction object for the training dataset
predition_stacking <- asROCRPrediction(stack_lrn_2.predict)

# get data for ROC Curve
rocData.stacking<- ROCR::performance (predition_stacking, "tpr", "fpr")

# Calculate Area Under ROC Curve
auc.stacking<- ROCR::performance(predition_stacking, "auc")
auc.stacking<- unlist(slot(auc.stacking, "y.values"))
print(auc.stacking)

# Plot ROC curve
plot(rocData.stacking, colorize = T,
main = "ROC Curve for Stacking_Model \n Train_Bin Data",
ylab = "Sensitivity (TPR)",
xlab = "1-Specificity (FPR)")
legend (0.8, 0.4, round(auc.stacking,4), title = "AUC", cex = 1.0, col = "blue")
# Draw line in the middle
abline(a=0, b = 1)


## Precision vs Recall (TPR)
prec_recall_stack <- ROCR::performance (predition_stacking, "prec", "rec")
plot(precObj.xg, colorize = T,
main = "Precision-Recall Curve for Stacking \n Train_Bin Data",
ylab = "Precision", xlab = "Recall (TPR)")
legend (0.2, 0.5, round(auc.stacking,4), title = "AUC", cex = 1.0, col = "b?ue")


## KS Statistic
TPR <- unlist(slot(rocData.stacking, "y.values"))
FPR<- unlist(slot(rocData.stacking, "x.values"))
diffTPR.FPR<- TPR-FPR
KS.stacking<- max(diffTPR.FPR)
KS.stacking

```
# Ensemble Model 3. Stacking of Random Forest fine-tuned using Random Search to maximize TPR and XGboost using GridSearch to maximize F1. Average method for stacking 
```{r}
learners<- list(rf.tree,xg_grid)
stack_learner_av<- makeStackedLearner(learners,predict.type = 'prob', method = 'average')

stack_cv_average<- resample(learner = stack_learner_av, task = trainTask_enc, resampling = rdesc, measures = list(tpr,fpr,tnr,fnr,acc, ppv, auc), show.info = T)


#train a model
stack_lrn_av <- train(stack_learner_av, trainTask_enc)
#make predictions
stack_lrn_av.predict <- predict(stack_lrn_av, testTask_enc)

tuneThreshold(stack_lrn_av.predict, f1)
stack_lrn_av.predict<- setThreshold(stack_lrn_av.predict,0.302341)

calculateConfusionMatrix(stack_lrn_av.predict, relative = TRUE, sums = TRUE, set = "both")

performance(stack_lrn_av.predict, f1, auc)

```
#  ROC. Precsion - Recall curve. KS statistic for Averaging stacking model 
```{r}
predition_stacking <- asROCRPrediction(stack_lrn_av.predict)

# get data for ROC Curve
rocData.stacking<- ROCR::performance (predition_stacking, "tpr", "fpr")

# Calculate Area Under ROC Curve
auc.stacking<- ROCR::performance(predition_stacking, "auc")
auc.stacking<- unlist(slot(auc.stacking, "y.values"))
print(auc.stacking)

# Plot ROC curve
plot(rocData.stacking, colorize = T,
main = "ROC Curve for Stacking Model",
ylab = "Sensitivity (TPR)",
xlab = "1-Specificity (FPR)")
legend (0.8, 0.4, round(auc.stacking,4), title = "AUC", cex = 1.0, col = "blue")
# Draw line in the middle
abline(a=0, b = 1)


## Precision vs Recall (TPR)
prec_recall_stack <- ROCR::performance (predition_stacking, "prec", "rec")
plot(precObj.xg, colorize = T,
main = "Precision-Recall Curve for Stacking Model",
ylab = "Precision", xlab = "Recall (TPR)")
legend (0.2, 0.5, round(auc.stacking,4), title = "AUC", cex = 1.0, col = "blue")


## KS Statistic
TPR <- unlist(slot(rocData.stacking, "y.values"))
FPR<- unlist(slot(rocData.stacking, "x.values"))
diffTPR.FPR<- TPR-FPR
KS.stacking<- max(diffTPR.FPR)
KS.stacking
```
# Averaging Stacking model. In - sample performance 

```{r}
stacking_in_sample <- predict(stack_lrn_av, trainTask_enc)

tuneThreshold(stacking_in_sample, f1)

stacking_in_sample <- setThreshold(stacking_in_sample, 0.2939737)
calculateConfusionMatrix(stacking_in_sample, relative = TRUE, sums = TRUE, set = "both")

# Calculate Area Under ROC Curve
ROCR::performance(asROCRPrediction(stacking_in_sample), "auc")

rocData<- ROCR::performance (asROCRPrediction(stacking_in_sample), "tpr", "fpr")
TPR <- unlist(slot(rocData, "y.values"))
FPR <- unlist(slot(rocData, "x.values"))
diffTPR.FPR<- TPR-FPR
KS <- max(diffTPR.FPR)
KS

```
# Investigate possible interaction terms 

```{r}
test.glm.fit=glm(DEFAULT ~ (SEX + EDUCATION + MARRIAGE + Pmt_Ratio_2 + Pmt_Ratio_3 + 
    Pmt_Ratio_4 + Pmt_Ratio_5 + Pmt_Ratio_6 + Avg_Pmt_Ratio + 
    Util_1 + Util_2 + Util_3 + Util_4 + Util_5 + Util_6 + Max_Util + 
    Min_Util + MAX_Util_ratio +sqrt_LIMIT_BAL + Avg_Bill_Amt_tfm + 
    Avg_Pmt_Amt_tfm + Max_Bill_Amt_tfm + Max_Pmt_Amt_tfm + WOE_AGE + 
    WOE_Freq_PAY + WOE_Avg_Util + WOE_Bal_Growth_6mo + WOE_LIMIT_BAL + 
    WOE_MAX_DLQ + WOE_OVER_LIMIT + WOE_OVER_PMT + WOE_REPAY_PATTERN + 
    WOE_Util_Growth_6mo + WOE_UTIL_PATTERN)^2, data = train_WOE,family = binomial) 
                               
anova(test.glm.fit)
options(max.print=100000)
print(summary(test.glm.fit))

```
# Logistic Regression. Automatic variable selection. 
## Stepwise variable selection. Transformed WOE data 
```{r}
train_one_hot_WOE <- createDummyFeatures(
  train_WOE, target = "DEFAULT")

test_one_hot_WOE <- createDummyFeatures(
  test_WOE, target = "DEFAULT")

# full model
stepwisemodel <- glm(DEFAULT ~., data = train_WOE, family = binomial)
stepwise <- stepAIC(stepwisemodel, direction = "both")
summary(stepwise)


stepwise_2 <- stepAIC(upper.lm, direction = "both")
summary(stepwise_2)

# Let's built model with automaticaly selected variables and also include statistically significant interation terms 
stepwise_subj <-glm(DEFAULT ~ SEX + MAX_Util_ratio +
    WOE_Avg_Util + WOE_LIMIT_BAL + WOE_MAX_DLQ + W?E_OVER_LIMIT + 
    WOE_OVER_PMT + WOE_REPAY_PATTERN + WOE_Util_Growth_6mo + 
    Pmt_Ratio_6 + 
    Util_2:MAX_Util_ratio + Util_3:Avg_Pmt_Amt_tfm + Util_3:Max_Pmt_Amt_tfm + 
    MAX_Util_ratio:WOE_Freq_PAY + Max_Util:sqrt_LIMIT_BAL + Max_Util:WOE_Avg_Util + 
    Avg_Pmt_Amt_tfm:Max_Pmt_Amt_tfm + Avg_Pmt_Amt_tfm:WOE_OVER_PMT + 
    Max_Bill_Amt_tfm:WOE_Avg_Util + Max_Pmt_Amt_tfm:WOE_OVER_PMT + 
    WOE_Freq_PAY:WOE_OVER_LIMIT + WOE_REPAY_PATTERN:WOE_Util_Growth_6mo + 
    WOE_Util_Growth_6mo:WOE_UTIL_PATTER?,data = train_WOE, family = binomial)
 summary(stepwise_subj)

```
#Stepwise model. 10-fold CV 
```{r}
# Stepwise model 
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE, repeats = 10)

model_fit_stepwise <- caret::train(DEFAULT ~ SEX + MARRIAGE + Util_1 + Util_2 + Util_5 + 
    MAX_Util_ratio + Avg_Pmt_Amt_tfm + Max_Bill_Amt_tfm + WOE_Freq_PAY + 
    WOE_Avg_Util + WOE_LIMIT_BAL + WOE_MAX_DLQ + WOE_OVER_PMT + 
    WOE_REPAY_PATTERN + Pmt_Ratio_6, data = train_WOE, method="glm", family="binomial", trControl = ctrl, tuneLength = 5)

pred_stepwise <- predict(model_fit_stepwise, newdata=test_WOE, type = "response")
caret::confusionMatrix(data=pred_stepwise, test_WOE$DEFAULT, positive = "1")

# stepwise model with interaction terms included 
model_fit_stepwise_2 <- caret::train(DEFAULT ~ SEX + EDUCATION + MARRIAGE + Util_2 + 
    Util_3 + Max_Util + MAX_Util_ratio + sqrt_LIMIT_BAL + Avg_Bill_Amt_tfm + 
    Avg_Pmt_Amt_tfm + Max_Bill_Amt_tfm + Max_Pmt_Amt_tfm + WOE_Freq_PAY + 
    WOE_Avg_Util + WOE_LIMIT_BAL + WOE_MAX_DLQ + WOE_OVER_LIMIT + 
    WOE_OVER_PMT + WOE_REPAY_PATTERN + WOE_Util_Growth_6mo + 
    WOE_UTIL_PATTERN + Pmt_Ratio_6 + EDUCATION:WOE_Freq_PAY + 
    Util_2:MAX_Util_ratio + Util_3:Avg_Pmt_Amt_tfm + Util_3:Max_Pmt_Amt_tfm + 
    MAX_Util_ratio:WOE_Freq_PAY + Max_Util:sqrt_LIMIT_BAL + Max_Util:WOE_Avg_Util + 
    Avg_Pmt_Amt_tfm:Max_Pmt_Amt_tfm + Avg_Pmt_Amt_tfm:WOE_OVER_PMT + 
    Max_Bill_Amt_tfm:WOE_Avg_Util + Max_Pmt_Amt_tfm:WOE_OVER_PMT + 
    WOE_Freq_PAY:WOE_OVER_LI?IT + WOE_REPAY_PATTERN:WOE_Util_Growth_6mo + 
    WOE_Util_Growth_6mo:WOE_UTIL_PATTERN, data = train_WOE, method="glm", family="binomial", trControl = ctrl, tuneLength = 5)

pred_stepwise_2 <- predict(model_fit_stepwise_2, newdata=test_WOE)
caret::confusio?Matrix(data=pred_stepwise_2, test_WOE$DEFAULT, positive = "1")

model_fit_stepwise_subj <- caret::train(DEFAULT ~ SEX + MAX_Util_ratio +
    WOE_Avg_Util + WOE_LIMIT_BAL + WOE_MAX_DLQ + WOE_OVER_LIMIT + 
    WOE_OVER_PMT + WOE_REPAY_PATTERN + WOE_Util_Growth_6mo + 
    Pmt_Ratio_6 + 
    Util_2:MAX_Util_ratio + Util_3:Avg_Pmt_Amt_tfm + Util_3:Max_Pmt_Amt_tfm + 
    MAX_Util_ratio:WOE_Freq_PAY + Max_Util:sqrt_LIMIT_BAL + Max_Util:WOE_Avg_Util + 
    Avg_Pmt_Amt_tfm:Max_Pmt_Amt_tfm + Avg_Pmt_Amt_tfm:WOE_OVER_PMT + 
    Max_Bill_Amt_tfm:WOE_Avg_Util + Max_Pmt_Amt_tfm:WOE_OVER_PMT + 
    WOE_Freq_PAY:WOE_OVER_LIMIT + WOE_REPAY_PATTERN:WOE_Util_Growth_6mo + 
    WOE_Util_Growth_6mo:WOE_UTIL_PATTERN, data = train_WOE, method="glm", family="binomial", trControl = ctrl, tuneLength = 5)

pred_stepwise_subj<- predict(model_fit_stepwise_subj, newdata=test_WOE)
caret::confusionMatrix(data=pred_stepwise_subj, test_WOE$DEFAULT, positive = "1")
```
# FORWARD VARIABLE SELECTION
```{r}
upper.lm<- glm(DEFAULT ~ SEX + EDUCATION + MARRIAGE + Pmt_Ratio_2 + Pmt_Ratio_3 + 
    Pmt_Ratio_4 + Pmt_Ratio_5 + Pmt_Ratio_6 + Avg_Pmt_Ratio + 
    Util_1 + Util_2 + Util_3 + Util_4 + Util_5 + Util_6 + Max_Util + 
    Min_Util + MAX_Util_ratio + sqrt_LIMIT_BAL + Avg_Bill_Amt_tfm + 
    Avg_Pmt_Amt_tfm + Max_Bill_Amt_tfm + Max_Pmt_Amt_tfm + WOE_AGE + 
    WOE_Freq_PAY + WOE_Avg_Util + WOE_Bal_Growth_6mo + WOE_LIMIT_BAL + 
    WOE_MAX_DLQ + WOE_OVER_LIMIT + WOE_OVER_PMT + WOE_REPAY_PATTERN + 
    WOE_Util_Growth_6mo + WOE_UTIL_PATTERN+SEX:WOE_MAX_DLQ+?DUCATION:WOE_Freq_PAY+MARRIAGE:WOE_Bal_Growth_6mo+ MARRIAGE:WOE_OVER_LIMIT+Pmt_Ratio_3:WOE_Util_Growth_6mo+Avg_Pmt_Ratio:WOE_Avg_Util+Util_2:MAX_Util_ratio+Util_2:sqrt_LIMIT_BAL+Util_2:WOE_Avg_Util+Util_3:MAX_Util_ratio+Util_3:Avg_Pmt_Amt_tfm+
Util_3:Max_Pmt_Amt_tfm+Util_4:sqrt_LIMIT_BAL+ MAX_Util_ratio:WOE_Freq_PAY+ MAX_Util_ratio:WOE_Avg_Util+
Util_4:WOE_MAX_DLQ+Max_Util:sqrt_LIMIT_BAL+Max_Util:WOE_Avg_Util+sqrt_LIMIT_BAL:WOE_Freq_PAY+
sqrt_LIMIT_BAL:WOE_OVER_LIMIT + Avg_Bill_Amt_tfm:WOE_OVER_PMT +Avg_Pmt_Amt_tfm:Max_Pmt_Amt_tfm+
Avg_Pmt_Amt_tfm:WOE_OVER_PMT +
Max_Bill_Amt_tfm:WOE_Avg_Util+
Max_Bill_Amt_tfm:WOE_OVER_PMT+
Max_Pmt_Amt_tfm:WOE_OVER_PMT+
WOE_AGE:WOE_REPAY_PATTERN +       
WOE_Freq_PAY:WOE_LIMIT_BAL +          
WOE_Freq_PAY:WOE_OVER_LIMIT +       
WOE_LIMIT_BAL:WOE_UTIL_PATTERN +        
WOE_OVER_LIMIT:WOE_Util_Growth_6mo +     
WOE_REPAY_PATTERN:WOE_Util_Growth_6mo +  
WOE_Util_Growth_6mo:WOE_UTIL_PATTERN, data = train_WOE, family = binomial)
summary(upper.lm)

lower.lm <- glm(DEFAULT~ 1, data = train_WOE, family=binomial)
summary(lower.lm)

forward.lm <- stepAIC(object=lower.lm,scope=list(upper=upper.lm,lower=lower.lm),
                      direction=c('forward'));
summary(forward.lm)

forward.lm_2<-stepAIC(object=lower.lm,scope=list(upper=step?isemodel,lower=lower.lm),
                      direction=c('forward'));
summary(forward.lm_2)
```
#Forward variable selection models. Cross - validation
```{r}

model_fit_forward <- caret::train(DEFAULT ~ WOE_MAX_DLQ + Avg_Pmt_Amt_tfm + WOE_Freq_PAY + 
   WOE_Avg_Util + WOE_LIMIT_BAL + MAX_Util_ratio + Util_1 + 
    MARRIAGE + Util_5 + SEX + WOE_OVER_PMT + Pmt_Ratio_6 + WOE_REPAY_PATTERN + 
    Max_Bill_Amt_tfm + WOE_Util_Growth_6mo + Util_2 + Max_Pmt_Amt_tfm + 
    WOE_Freq_PAY:WOE_LIMIT_BAL + WOE_Avg_Ut?l:Max_Bill_Amt_tfm + 
    WOE_Avg_Util:MAX_Util_ratio + WOE_OVER_PMT:Max_Bill_Amt_tfm + 
    WOE_REPAY_PATTERN:WOE_Util_Growth_6mo + WOE_Avg_Util:Util_2 + 
    MAX_Util_ratio:Util_2, data = train_WOE, method="glm", family="binomial", trControl = ctrl, tuneLength = 5)

pred_forward <- predict(model_fit_forward, newdata=test_WOE)
caret::confusionMatrix(data=pred_forward, test_WOE$DEFAULT, positive = "1")


model_fit_forward_2 <- caret::train(DEFAULT ~ WOE_MAX_DLQ + Avg_Pmt_Amt_tfm + WOE_Freq_PAY + 
    WOE_Avg?Util + WOE_LIMIT_BAL + MAX_Util_ratio + Util_1 + 
    MARRIAGE + Util_5 + WOE_REPAY_PATTERN + SEX + WOE_OVER_PMT + 
    Pmt_Ratio_6 + Max_Bill_Amt_tfm + Util_2, data = train_WOE, method="glm", family="binomial", trControl = ctrl, tuneLength = 5)

pred_forward_2 <- predict(model_fit_forward_2, newdata=test_WOE)
caret::confusionMatrix(data=pred_forward_2, test_WOE$DEFAULT, positive = "1")

```
# BACKWARD VARIABLE SELECTION
```{r}
backward.lm <- stepAIC(object=upper.lm,direction=c('backward'));
summary(backward.lm)
backward.lm_2<- stepAIC(object=stepwisemodel,direction=c('backward'))
summary(backward.lm_2)

```
#Backward variable selection models. Cross-Validation 
```{r}
model_fit_backward <- caret::train(DEFAULT ~ SEX + EDUCATION + MARRIAGE + Util_2 + 
    Util_3 + Util_4 + Max_Util + MAX_Util_ratio + sqrt_LIMIT_BAL + 
    Avg_Bill_Amt_tfm + Avg_Pmt_Amt_tfm + Max_Bill_Amt_tfm + Max_Pmt_Amt_tfm + 
    WOE_Freq_PAY + WOE_Avg_Util + WOE_LIMIT_BAL + WOE_MAX_DLQ + 
    WOE_OVER_LIMIT + WOE_OVER_PMT + WOE_REPAY_PATTERN + WOE_Util_Growth_6mo + 
    WOE_UTIL_PATTERN + EDUCATION:WOE_Freq_PAY + Util_2:MAX_Util_ratio + 
    Util_3:Avg_Pmt_Amt_tfm + Util_3:Max_Pmt_Amt_tfm + MAX_Util_ratio:WOE_Freq_PAY + 
    Util_4:WOE_MAX_DLQ + Max_Util:sqrt_LIMIT_BAL + Max_Util:WOE_Avg_Util + 
    Avg_Bill_Amt_tfm:WOE_OVER_PMT + Avg_Pmt_Amt_tfm:Max_Pmt_Amt_tfm + 
    Avg_Pmt_Amt_tfm:WOE_OVER_PMT + Max_Bill_Amt_tfm:WOE_Avg_Util + 
    Max_Pmt_Amt_tfm:WOE_OVER_PMT + WOE_Freq_PAY:WOE_LIMIT_BAL + 
    WOE_Freq_PAY:WOE_OVER_LIMIT + WOE_REPAY_PATTERN:WOE_Util_Growth_6mo + 
    WOE_Util_Growth_6mo:WOE_UTIL_PATTERN, data = train_WOE, method="glm", family="binomial", trControl = ctrl, tuneLength = 5)

pred_backward <- predict(model_fit_backward, newdata=test_WOE)
caret::confusionMatrix(data=pred_backward, test_WOE$DEFAULT, positive = "1")

# stepwise model with no interaction terms included 
model_fit_backward_2 <- caret::train(DEFAULT ~ SEX + MARRIAGE + Avg_Pmt_Ratio + Util_1 + 
    Util_2 + Util_5 + MAX_Util_ratio + Avg_Bill_Amt_tfm + Avg_Pmt_Amt_tfm + 
    Max_Bill_Amt_tfm + Max_Pmt_Amt_tfm + WOE_Freq_PAY + WOE_Avg_Util + 
    WOE_LIMIT_BAL + WOE_MAX_DLQ + WOE_OVER_PMT + WOE_REPAY_PATTERN, data = train_WOE, method="glm", family="binomial", trControl = ctrl, tuneLength = 5)

pred_backward_2 <- predict(model_fit_backward_2, newdata=test_WOE)
caret::confusionMatrix(data=pred_backward_2, test_WOE$DEFAULT, positive = "1")
```
# Subjective Logistic regression model advised by insights from feature importance plots 
# and investigation of statistically significant interaction terms 
```{r}
base_subj <- glm(DEFAULT ~ MARRIAGE + Pmt_Ratio_2 + Pmt_Ratio_3 + 
    Pmt_Ratio_4 + Pmt_Ratio_5 + Pmt_Ratio_6 + Avg_Pmt_Ratio + 
    Util_1 + Util_2 + Util_3 + Util_4 + Util_5 + Util_6 + Max_Util + 
    Min_Util + MAX_Util_ratio + sqrt_LIMIT_BAL + Avg_Bill_Amt_tfm + 
    Avg_Pmt_Amt_tfm + Max_Bill_Amt_tfm + Max_Pmt_Amt_tfm+ WOE_AGE + 
    WOE_Freq_PAY + WOE_Avg_Util + WOE_Bal_Growth_6mo + WOE_LIMIT_BAL + 
    WOE_MAX_DLQ + WOE_OVER_PMT + WOE_REPAY_PATTERN + 
    WOE_Util_Growth_6mo+ EDUCATION:WOE_Freq_PAY + 
    Util_2:MAX_Util_ratio + 
    Util_3:Avg_Pmt_Amt_tfm +
    Util_3:Max_Pmt_Amt_tfm +
    MAX_Util_ratio:WOE_Freq_PAY + 
    MAX_Util_ratio:WOE_Avg_Util+            
    Max_Util:sqrt_LIMIT_BAL +
    Avg_Bill_Amt_tfm:WOE_OVER_PMT + 
    Avg_Pmt_Amt_tfm:Max_Pmt_Amt_tfm + 
    Avg_Pmt_Amt_tfm:WOE_OVER_PMT +
    Max_Bill_Amt_tfm:WOE_Avg_Util + 
    Max_Pmt_Amt_tfm:WOE_OVER_PMT +
    WOE_REPAY_PATTERN:WOE_Util_Growth_6mo + 
    WOE_Freq_PAY:WOE_OVER_LIMIT+           
    WOE_Util_Growth_6mo:WOE_UTIL_PATTERN, data =train_WOE, family = binomial)
summary(base_subj)

stepwise_subj<- stepAIC(base_subj, direction = "both")
summary(stepwise_subj)

stepw_subj <- glm(DEFAULT ~ MARRIAGE + Pmt_Ratio_2 + Pmt_Ratio_3 + 
    Pmt_Ratio_4 + Pmt_Ratio_5 + Pmt_Ratio_6 + Avg_Pmt_Ratio + 
    Util_1 + Util_2 + Util_3 + Util_4 + Util_5 + Util_6 + Max_Util + 
    Min_Util + MAX_Util_ratio + sqrt_LIMIT_BAL + Avg_Bill_Amt_tfm + 
    Avg_Pmt_Amt_tfm  + WOE_AGE + Max_Bill_Amt_tfm + Avg_Pmt_Amt_tfm +
    WOE_Freq_PAY + WOE_Avg_Util + WOE_Bal_Growth_6mo + WOE_LIMIT_BAL + 
    WOE_MAX_DLQ + WOE_OVER_PMT+ WOE_REPAY_PATTERN + 
    WOE_Util_Growth_6mo, data =train_WOE, family = binomial)

summary(stepw_subj)

stepw_subj <- stepAIC(stepw_subj, direction = "both")
summary(stepw_subj)
```
#Subjective model with interaction terms. Cross-validation 
```{r}
model_fit_subj<- caret::train(DEFAULT ~ MARRIAGE + Pmt_Ratio_2 + Pmt_Ratio_3 + 
    Pmt_Ratio_4 + Pmt_Ratio_5 + Pmt_Ratio_6 + Avg_Pmt_Ratio + 
    Util_1 + Util_2 + Util_3 + Util_4 + Util_5 + Util_6 + Max_Util + 
    Min_Util + MAX_Util_ratio + sqrt_LIMIT_BAL + Avg_Bill_Amt_tfm + 
    Avg_Pmt_Amt_tfm + Max_Bill_Amt_tfm + Max_Pmt_Amt_tfm + WOE_AGE + 
    WOE_Freq_PAY + WOE_Avg_Util + WOE_Bal_Growth_6mo + WOE_LIMIT_BAL + 
    WOE_MAX_DLQ + WOE_OVER_PMT + WOE_REPAY_PATTERN + WOE_Util_Growth_6mo + 
    EDUCATION:WOE_Freq_PAY + Util_2:MAX_Util_ratio + Util_3:Avg_Pmt_Amt_tfm + 
    Util_3:Max_Pmt_Amt_tfm + MAX_Util_ratio:WOE_Freq_PAY + MAX_Util_ratio:WOE_Avg_Util + 
    Max_Util:sqrt_LIMIT_BAL + Avg_Bill_Amt_tfm:WOE_OVER_PMT + 
    Avg_Pmt_Amt_tfm:Max_Pmt_Amt_tfm + Avg_Pmt_Amt_tfm:WOE_OVER_PMT + 
    Max_Bill_Amt_tfm:WOE_Avg_Util + Max_Pmt_Amt_tfm:WOE_OVER_PMT + 
    WOE_REPAY_PATTERN:WOE_Util_Growth_6mo + WOE_Freq_PAY:WOE_OVER_LIMIT + 
    WOE_Util_Growth_6mo:WOE_UTIL_PATTERN, data = train_WOE, method="glm", family="binomial", trControl = ctrl, tuneLength = 5)

pred_base_subj <- predict(model_fit_subj, newdata=test_WOE)
caret::confusionMatrix(data=pred_base_subj, test_WOE$DEFAULT, positive = "1")
```
#Subjective model no interaction terms. Cross-validation 
```{r}
# Stepwise_subjective model NO INTERACTION TERMS 

model_fit_stepwise_subj_2 <- caret::train(DEFAULT ~ MARRIAGE + Pmt_Ratio_2 + Pmt_Ratio_3 + 
    Pmt_Ratio_4 + Pmt_Ratio_5 + Pmt_Ratio_6 + Avg_Pmt_Ratio + 
    Util_1 + Util_2 + Util_3 + Util_4 + Util_5 + Util_6 + Max_Util + 
    Min_Util + MAX_Util_ratio + sqrt_LIMIT_BAL + Avg_Bill_Amt_tfm + 
    Avg_Pmt_Amt_tfm + Max_Bill_Amt_tfm + Max_Pmt_Amt_tfm + WOE_AGE + 
    WOE_Freq_PAY + WOE_Avg_Util + WOE_Bal_Growth_6mo + WOE_LIMIT_BAL + 
    WOE_MAX_DLQ + WOE_OVER_PMT + WOE_REPAY_PATTERN + WOE_Util_Growth_6mo, data = train_WOE, method="glm", family="binomial", trControl = ctrl, tuneLength = 5)

pred_stepwise_subj_2 <- predict(model_fit_stepwise_subj_2, newdata=test_WOE)
caret::confusionMatrix(data=pred_stepwise_subj_2, test_WOE$DEFAULT, positive = "1")
```
#Stepwise_subjective model out-of-sample performance

```{r}
pred_stepwise <- predict(stepw_subj, newdata=test_WOE, type = "response")
summ<- jtools::summ(stepw_subj, vifs = TRUE, confint = TRUE, digits = 4, pvals = TRUE)
InformationValue::confusionMatrix(test_WOE$DEFAULT, pred_stepwise, threshold = 0.30)

prediction_stepwise <- prediction (pred_stepwise,test_WOE$DEFAULT)
# get data for ROC Curve
rocData.stepwise <- ROCR::performance (prediction_stepwise, "tpr", "fpr")
# Calculate Area Under ROC Curve
auc.stepwise <- ROCR::performance(prediction_stepwise, "auc")
auc.stepwise <- unlist(slot(auc.stepwise, "y.values"))
print(auc.stepwise)

file.name= 'coef_table_0' 
stargazer(summ$coeftable, type=c('html'),out=paste(out.path,file.name,sep=''),
	title=c('Table : The Coefficient table. VIF values included'),
	align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, summary = FALSE)
```
#Stepwise_subjective model no MARRIAGE variable. Out-of-sample performance

```{r}

stepwise_subj_2 <- glm(DEFAULT ~ Pmt_Ratio_2 + Pmt_Ratio_3 + 
    Pmt_Ratio_4 + Pmt_Ratio_5 + Pmt_Ratio_6 + Avg_Pmt_Ratio + 
    Util_1 + Util_2 + Util_3 + Util_4 + Util_5 + Util_6 + Max_Util + 
    Min_Util + MAX_Util_ratio + sqrt_LIMIT_BAL + Avg_Bill_Amt_tfm + 
    Avg_Pmt_Amt_tfm  + WOE_AGE + Max_Bill_Amt_tfm + Avg_Pmt_Amt_tfm +
    WOE_Freq_PAY + WOE_Avg_Util + WOE_Bal_Growth_6mo + WOE_LIMIT_BAL + 
    WOE_MAX_DLQ + WOE_OVER_PMT+ WOE_REPAY_PATTERN + 
    WOE_Util_Growth_6mo, data =train_WOE, family = binomial)

summary(stepwise_subj_2)

stepwise_subj_2 <- stepAIC(stepwise_subj_2, direction = "both")
summary(stepwise_subj_2)

pred_stepwise <- predict(stepwise_subj_2, newdata=test_WOE, type = "response")
summ<- jtools::summ(stepwise_subj_2, vifs = TRUE, confint = TRUE, digits = 4, pvals = TRUE)
InformationValue::confusionMatrix(test_WOE$DEFAULT, pred_stepwise, threshold = 0.22)

file.name= 'coef_table' 
stargazer(summ$coeftable, type=c('html'),out=paste(out.path,file.name,sep=''),
	title=c('Table : The Coefficient table. VIF values included'),
	align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, summary = FALSE)
```
Cross-validation stepwise_subjective model, some statistically insignificant variables excluded 
```{r}

model_fit_stepwise_subj_2 <- caret::train(DEFAULT ~ Util_1 + Util_2 + Util_5 + MAX_Util_ratio + 
    Avg_Pmt_Amt_tfm + Max_Bill_Amt_tfm + WOE_Freq_PAY + WOE_Avg_Util + 
    WOE_LIMIT_BAL + WOE_MAX_DLQ + WOE_OVER_PMT + WOE_REPAY_PATTERN + 
    Pmt_Ratio_6, data = train_WOE, method="glm", family="binomial", trControl = ctrl, tuneLength = 5)

pred_stepwise_subj_2 <- predict(model_fit_stepwise_subj_2, newdata=test_WOE)
caret::confusionMatrix(data=pred_stepwise_subj_2, test_WOE$DEFAULT, positive = "1")
```
# Try to improve logistic regression performance 

```{r}
# Stepwise_subjective model NO INTERACTION TERMS 
test_model<- glm(formula = DEFAULT ~ MARRIAGE +Util_1 + Util_2+ Util_5 + MAX_Util_ratio + 
    Avg_Pmt_Amt_tfm + Max_Bill_Amt_tfm + WOE_Freq_PAY + WOE_Avg_Util + 
    WOE_LIMIT_BAL + WOE_MAX_DLQ + WOE_OVER_PMT + WOE_REPAY_PATTERN + 
    Pmt_Ratio_6, family="binomial", data =train_WOE)


test_model <- glm(formula = DEFAULT ~ Pmt_Ratio_6 +Util_1+ WOE_OVER_PMT+MAX_Util_ratio+ WOE_MAX_DLQ+       WOE_Freq_PAY +Avg_Pmt_Amt_tfm, family = binomial, data = train_WOE)
test_model <- glm(formula = DEFAULT ~ Util_2+ MAX_Util_ratio+ WOE_MAX_DLQ+WOE_Freq_PAY+Avg_Pmt_Amt_tfm+Max_Bill_Amt_tfm, family = binomial, data = train_WOE)

pred_test <- predict(test_model, newdata=test_WOE, type = "response")
jtools::summ(test_model, vifs = TRUE, confint = TRUE, digits = 4, pvals = TRUE)
InformationValue::confusionMatrix(test_WOE$DEFAULT, pred_test, threshold = 0.20)
pr<_predict(stack_lrn_av, trainTask_enc)
mlr::performance(pr, 'tpr','tnr','f1','auc')

prediction <- prediction (pred_test,test_WOE$DEFAULT)
# get data for ROC Curve
rocData <- ROCR::performance (prediction, "tpr", "fpr")
# Calculate Area Under ROC Curve
auc.stepwise <- ROCR::performance(prediction, "auc")
auc.stepwise <- unlist(slot(auc.stepwise, "y.values"))
print(auc.stepwise)

ks(test$DEFAULT,pred_test)

ctrl<- makeFeatSelControlSequential(tune.threshold = TRUE, method = 'sbs' )
sf<- selectFeatures(logreg,trainTask,rdesc, measures = list(mlr::auc,f1), control= ctrl)

logreg <- makeLearner("classif.logreg", predict.type = "prob")

honor1<-train_bin %>%
        group_by(as.numeric(DEFAULT, Util_1, Util_2, Util_5, MAX_Util_ratio,  
    Avg_Pmt_Amt_tfm, Max_Bill_Amt_tfm,Freq_PAY_bin,  Avg_Util_bin,  
    LIMIT_BAL_bin, Max_DLQ, OVER_PMT,REPAY_PATTERN_bin, 
    Pmt_Ratio_6)) %>%
        summarise(freq=n()) %>%
        mutate(all=sum(freq),prob=freq/all,odds=prob/(1-prob),logodds=log(odds)) %>%
        round(.,5)

pander(honor1)
```
# Lift chart 
```{r}

predict_stepwise.in_sample <- predict(stepwise_subj_2, newdata=train_WOE, type = "response")
InformationValue::confusionMatrix(train_WOE$DEFAULT,predict_stepwise.in_sample, threshold = 0.2129569)

prediction_stepwise <- prediction (predict_stepwise.in_sample,train_WOE$DEFAULT)
      # get data for ROC Curve
rocData.stepwise <- ROCR::performance (prediction_stepwise, "tpr", "fpr")
      #Calculate Area Under ROC Curve
auc.stepwise <- ROCR::performance(prediction_stepwise, "auc")
auc.stepwise <- unlist(slot(auc.stepwise, "y.values"))
print(auc.stepwise)

ks(train$DEFAULT, predict_stepwise.in_sample)
InformationValue::ks_plot(actuals=train$DEFAULT, predictedScores=predict_stepwise.in_sample)
InformationValue::ks_stat(actuals=train$DEFAULT, predictedScores=predict_stepwise.in_sample)

unlist(slot(prediction_stepwise, "predictions"))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  prediction_stepwise <- prediction (pred_stepwise,test_WOE$DEFAULT)
      # get data for ROC Curve
rocData.stepwise <- ROCR::performance (prediction_stepwise, "tpr", "fpr")
      #Calculate Area Under ROC Curve
auc.stepwise <- ROCR::performance(prediction_stepwise, "auc")
auc.stepwise <- unlist(slot(auc.stepwise, "y.values"))
print(auc.stepwise)

  pred_stepwise <- predict(stepwise_subj_2, newdata=test_WOE, type = "response")
summ<- jtools::summ(stepwise_subj_2, vifs = TRUE, confint = TRUE, digits = 4, pvals = TRUE)
InformationValue::confusionMatrix(test_WOE$DEFAULT, pred_stepwise, threshold = 0.22)
```

# In - sample performance of Champion Logistic Model 
```{r}
predict_stepwise.in_sample <- predict(stepwise_subj_2, newdata=train_WOE, type = "response")
InformationValue::confusionMatrix(train_WOE$DEFAULT,predict_stepwise.in_sample, threshold = 0.2129569)

prediction_stepwise <- prediction (pred_stepwise,test_WOE$DEFAULT)
# get data for ROC Curve
rocData.stepwise <- ROCR::performance (prediction_stepwise, "tpr", "fpr")
# Calculate Area Under ROC Curve
auc.stepwise <- ROCR::performance(prediction_stepwise, "auc")
auc.stepwise <- unlist(slot(auc.stepwise, "y.values"))
print(auc.stepwise)

ks(train$DEFAULT, predict_stepwise.in_sample)
InformationValue::ks_plot(actuals=train$DEFAULT, predictedScores=predict_stepwise.in_sample)
InformationValue::ks_stat(actuals=train$DEFAULT, predictedScores=predict_stepwise.in_sample)
```
# ROC Curve and Area under ROC. IN-SAMPLE
```{r}
       # Create prediction object for the training dataset
       
prediction_stepwise <- prediction (predict_stepwise.in_sample,train_WOE$DEFAULT)
      # get data for ROC Curve
rocData.stepwise <- ROCR::performance (prediction_stepwise, "tpr", "fpr")
      #Calculate Area Under ROC Curve
auc.stepwise <- ROCR::performance(prediction_stepwise, "auc")
auc.stepwise <- unlist(slot(auc.stepwise, "y.values"))
print(auc.stepwise)

      # Plot ROC Curve
      
plot(rocData.stepwise, colorize = T,
main = "ROC Curve for Logistic Regression Model.\n In-sample performance",
ylab = "Sensitivity (TPR)",
xlab = "1-Specificity (FPR)")
legend (0.8, 0.4, round(auc.stepwise,4), title = "AUC", cex = 1.0, col = "blue")
      
      # Draw line in the middle
abline(a=0, b = 1)
      
       ## Precision vs Recall (TPR)
prec_recall <- ROCR::performance (prediction_stepwise, "prec", "rec")
plot(prec_recall, colorize = T,
main = "Precision-Recall Curve for Logistic Regression.\n In - sample performance",
ylab = "Precision", xlab = "Recall (TPR)")
legend (0.2, 0.5, round(auc.stepwise,4), title = "AUC", cex = 1.0, col = "blue")

         ## KS Statistic
TPR.stepwise <- unlist(slot(rocData.stepwise, "y.values"))
FPR.stepwise<- unlist(slot(rocData.stepwise, "x.values"))
diffTPR.FPR <- TPR.stepwise-FPR.stepwise
KS.stepwise<- max(diffTPR.FPR)
```
# ROC Curve and Area under ROC. Out-of-sample 
```{r}

       # Create prediction object for the training dataset
       
prediction_stepwise <- prediction (pred_stepwise,test_WOE$DEFAULT)
      # get data for ROC Curve
rocData.stepwise <- ROCR::performance (prediction_stepwise, "tpr", "fpr")
      #Calculate Area Under ROC Curve
auc.stepwise <- ROCR::performance(prediction_stepwise, "auc")
auc.stepwise <- unlist(slot(auc.stepwise, "y.values"))
print(auc.stepwise)

      # Plot ROC Curve
      
plot(rocData.stepwise, colorize = T,
main = "ROC Curve for Logistic Regression Model. \n Out-of-sample performance",
ylab = "Sensitivity (TPR)",
xlab = "1-Specificity (FPR)")
legend (0.8, 0.4, round(auc.stepwise,4), title = "AUC", cex = 1.0, col = "blue")
      
      # Draw line in the middle
abline(a=0, b = 1)
      
       ## Precision vs Recall (TPR)
prec_recall <- ROCR::performance (prediction_stepwise, "prec", "rec")
plot(prec_recall, colorize = T,
main = "Precision-Recall Curve for Logistic Regression Model.\n Out-of-sample performance",
ylab = "Precision", xlab = "Recall (TPR)")
legend (0.2, 0.5, round(auc.stepwise,4), title = "AUC", cex = 1.0, col = "blue")

         ## KS Statistic
TPR.stepwise <- unlist(slot(rocData.stepwise, "y.values"))
FPR.stepwise<- unlist(slot(rocData.stepwise, "x.values"))
diffTPR.FPR <- TPR.stepwise-FPR.stepwise
KS.stepwise<- max(diffTPR.FPR)

```
# Naive Bayes. Train and evaluate 

```{r}

#Initialize the Naive Bayes classifier
naive_model = makeLearner("classif.naiveBayes", predict.type = "prob")
 
#Train the model
NB_mlr = train(naive_model, trainTask)
 
#Read the model learned  
NB_mlr$learner.model

#in- sample performance 

predict_naive.in_sample <- predict(NB_mlr, trainTask)
tuneThreshold(predict_naive.in_sample, f1)

predict_naive.in_sample <- setThreshold(predict_naive.in_sample,0.6437918)

calculateConfusionMatrix(predict_naive.in_sample, relative = TRUE, sums = TRUE, set = "both")


# Calculate Area Under ROC Curve
performance(predict_naive.in_sample, list(auc,f1,acc))
## KS Statistic
rocData.naive<- ROCR::performance (asROCRPrediction(predict_naive.in_sample), "tpr", "fpr")
TPR <- unlist(slot(rocData.naive, "y.values"))
FPR <- unlist(slot(rocData.naive, "x.values"))
diffTPR<- TPR-FPR
KS<- max(diffTPR)
KS

#Cross- validation 

cv_naive<- resample(learner = naive_model, task = trainTask, resampling = rdesc, measures = list(tpr,fpr,tnr,fnr,acc, ppv, auc), show.info = T)


# Out-of-sample performance 

predict.naive <- predict(NB_mlr, testTask)
calculateConfusionMatrix(predict.naive, relative = TRUE, sums = TRUE, set = "both")
tuneThreshold(predict.naive, f1)
predict.naive<- setThreshold(predict.naive,0.4118521)


calculateConfusionMatrix(predict.naive, relative = TRUE, sums = TRUE, set = "both")

# Calculate Area Under ROC Curve
performance(predict.naive, list(auc,f1,acc))
```

# Naive Bayes. ROC and Precsion Recall. KS -statistic 

```{r}

# Create prediction object for the training dataset
prediction_naive <- asROCRPrediction(predict.naive)

# get data for ROC Curve
rocData.naive<- ROCR::performance (prediction_naive, "tpr", "fpr")

# Calculate Area Under ROC Curve
auc.naive<- ROCR::performance(prediction_naive, "auc")
auc.naive<- unlist(slot(auc.naive, "y.values"))
print(auc.naive)

# Plot ROC curve
plot(rocData.naive, colorize = T,
main = "ROC Curve Naive Bayes model",
ylab = "Sensitivity (TPR)",
xlab = "1-Specificity (FPR)")
legend (0.8, 0.4, round(auc.naive,4), title = "AUC", cex = 1.0, col = "blue")
# Draw line in the middle
abline(a=0, b = 1)

## Precision vs Recall (TPR)
prec_recall_naive <- ROCR::performance (predition_naive, "prec", "rec")
plot(prec_recall_naive, colorize = T,
main = "Precision- Recall Curve Naive Bayes model",
ylab = "Precision", xlab = "Recall (TPR)")
legend (0.2, 0.5, round(auc.naive,4), title = "AUC", cex = 1.0, col = "blue")


## KS Statistic
TPR.naive <- unlist(slot(rocData.naive, "y.values"))
FPR.naive <- unlist(slot(rocData.naive, "x.values"))
diffTPR<- TPR.naive-FPR.naive
KS.naive<- max(diffTPR)
KS.naive

```

# Decile Model Scores;
###############################################################################
```{r}

decile.pts <- quantile(predict_stepwise.in_sample,
			probs=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5, 0.55, 0.6, 0.65, 0.7, 0.75,0.8,0.85,0.9,0.95))

###############################################################################
# Assign Model Scores to Deciles;
###############################################################################


my.df <- data.frame(predict_stepwise.in_sample, train_WOE$DEFAULT)
my.df$model.decile <- cut(my.df$predict_stepwise.in_sample,breaks=c(0,decile.pts,1),
			labels=rev(c('01','02','03','04','05','06','07','08','09','10',"11","12","13","14","15","16","17","18","19","20")))

head(my.df)


# Note that we need to add the end points to our decile points to properly
# apply the cut() function;

# Note that we want the 'top decile' to be the highest model scores so we
# will reverse the labels.

# Check the min score in each model decile;
aggregate(my.df$predict_stepwise.in_sample,by=list(Decile=my.df$model.decile),FUN=min)


table(my.df$model.decile)

table(my.df$model.decile,my.df$train_WOE.DEFAULT)

    

ks.table <- as.data.frame(list(Y0=table(my.df$model.decile,my.df$train_WOE.DEFAULT)[,1],
		Y1=table(my.df$model.decile,my.df$train_WOE.DEFAULT)[,2],
		Decile=rev(c('01','02','03','04','05','06','07','08','09','10',"11","12","13","14","15","16","17","18","19","20"))
		))


# Sort the data frame by decile;
ks.table[order(ks.table$Decile),]


################ KS for test data 

decile.pts <- quantile(pred_stepwise,
			probs=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5, 0.55, 0.6, 0.65, 0.7, 0.75,0.8,0.85,0.9,0.95))

###############################################################################
# Assign Model Scores to Deciles;
###############################################################################


my.df <- data.frame(pred_stepwise, test_WOE$DEFAULT)
my.df$model.decile <- cut(my.df$pred_stepwise,breaks=c(0,decile.pts,1),
			labels=rev(c('01','02','03','04','05','06','07','08','09','10',"11","12","13","14","15","16","17","18","19","20")))

head(my.df)

table(my.df$model.decile)

table(my.df$model.decile,my.df$test_WOE.DEFAULT)

    

ks.table <- as.data.frame(list(Y0=table(my.df$model.decile,my.df$test_WOE.DEFAULT)[,1],
		Y1=table(my.df$model.decile,my.df$test_WOE.DEFAULT)[,2],
		Decile=rev(c('01','02','03','04','05','06','07','08','09','10',"11","12","13","14","15","16","17","18","19","20"))
		))


# Sort the data frame by decile;
ks.table[order(ks.table$Decile),]

```
# Validate the model
#1.Create validation dataset
```{r}
validate<-credit_card_default[credit_card_default$data.group==3,index_2] 

head(validate)

#relevel validate Data
contrasts(validate$MARRIAGE)

validate <- validate %>%
  mutate(MARRIAGE = relevel(MARRIAGE, ref = "3"))

summary(validate$MARRIAGE)

# relevel EDUCATION variable 

contrasts(validate$EDUCATION)
summary(validate$EDUCATION)

validate <- validate %>%
  mutate(EDUCATION = relevel(EDUCATION, ref = "4"))
glm(DEFAULT~ EDUCATION, data = validate, family = binomial)

summary(validate$EDUCATION)

validate$DEFAULT<- as.factor(validate$DEFAULT)
```
Output the validate summury statistics 

```{r}
file.name <- 'validate.html'
out.path = 'C:\\Users\\yanin\\Desktop\\Capstone\\Project Output\\'

stargazer(validate, type=c('html'),out=paste(out.path,file.name,sep=''),
	title=c('Table : Summary Statistics for Validate data'),
	align=TRUE, digits=2, digits.extra=2, median=TRUE)

```
#  Create subsets of validate sets 

```{r}

validate_original<- validate[-c(33,36:57)]
validate_bin<- validate[-c(2,7,8,21,24,25,29,30,32,34,35,47:57)]# 25 - sqrt_LIM_BAL
validate_WOE<- validate[-c(2,6,7,8,21,22,23,24,25,29,30,31,32,33,34,35,37,40,43,44:46)]#20 - sqrt_LIM_BAL

```
#2. Validate the model on KS and AUC value 

```{r}
predict_logreg_validate <- predict(stepwise_subj_2, newdata=validate_WOE, type = "response")

opt_cut_off_validate <- optimalCutoff(validate$DEFAULT, predict_logreg_validate, optimiseFor = "Both")

InformationValue::confusionMatrix(validate_WOE$DEFAULT,predict_logreg_validate, threshold = 0.264386)

prediction_validate <- prediction(predict_logreg_validate,validate_WOE$DEFAULT)
      # get data for ROC Curve
rocData.validate <- ROCR::performance (prediction_validate, "tpr", "fpr")
      #Calculate Area Under ROC Curve
auc_V <- ROCR::performance(prediction_validate, "auc")
auc_V <- unlist(slot(auc_V, "y.values"))
print(auc_V)

ks(validate$DEFAULT, predict_logreg_validate)
InformationValue::ks_plot(validate$DEFAULT, predict_logreg_validate)
InformationValue::ks_stat(validate$DEFAULT, predict_logreg_validate)


               # Decile Model Scores


decile.pts <- quantile(predict_logreg_validate,
			probs=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5, 0.55, 0.6, 0.65, 0.7, 0.75,0.8,0.85,0.9,0.95))

            # Assign Model Scores to Deciles


my.df <- data.frame(predict_logreg_validate, validate_WOE$DEFAULT)
my.df$model.decile <- cut(my.df$predict_logreg_validate,breaks=c(0,decile.pts,1),
			labels=rev(c('01','02','03','04','05','06','07','08','09','10',"11","12","13","14","15","16","17","18","19","20")))

head(my.df)


table(my.df$model.decile)

table(my.df$model.decile,my.df$validate_WOE.DEFAULT)   

ks.table <- as.data.frame(list(Y0=table(my.df$model.decile,my.df$validate_WOE.DEFAULT)[,1],
		Y1=table(my.df$model.decile,my.df$validate_WOE.DEFAULT)[,2],
		Decile=rev(c('01','02','03','04','05','06','07','08','09','10',"11","12","13","14","15","16","17","18","19","20"))))


# Sort the data frame by decile;
ks.table[order(ks.table$Decile),]

```

