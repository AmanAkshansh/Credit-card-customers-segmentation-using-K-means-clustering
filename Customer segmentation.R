                       #  Project 1 - Customer segmentation

   # Getting the Data
library(readxl)
mydata<- read_xlsx("E:\\Analytixlabs\\Module 6 (Data science using R)\\Case Studies\\Case study 1 - Segmentation\\CC GENERAL.xlsx")
View(mydata)
str(mydata)
names(mydata)
library(dplyr)
mydata<- mutate(mydata,LIMIT_USAGE=BALANCE/CREDIT_LIMIT,
                P_MP_RATIO=PAYMENTS/MINIMUM_PAYMENTS,
                MONTHLY_AVG_PURCHASES = PURCHASES/TENURE,
                MONTHLY_AVG_CASH_ADVANCE = CASH_ADVANCE/TENURE,
                AVG_AMT_PER_PURCHASE = PURCHASES/PURCHASES_TRX,
                AVG_CASH_ADV_PER_TRX = CASH_ADVANCE/CASH_ADVANCE_TRX)

mydata$AVG_AMT_PER_PURCHASE[is.nan(mydata$AVG_AMT_PER_PURCHASE)]<- 0
mydata$AVG_CASH_ADV_PER_TRX[is.nan(mydata$AVG_CASH_ADV_PER_TRX)]<- 0 
mydata$ONEOFF_PURCHASES[mydata$PURCHASES==0]<- 0
mydata$INSTALLMENTS_PURCHASES[mydata$PURCHASES==0]<- 0

summary(mydata[c("LIMIT_USAGE","P_MP_RATIO","MONTHLY_AVG_PURCHASES",
                 "MONTHLY_AVG_CASH_ADVANCE","AVG_AMT_PER_PURCHASE",
                 "AVG_CASH_ADV_PER_TRX")])

df<-filter(mydata,AVG_AMT_PER_PURCHASE==Inf)
View(df)
# We can see that three observations have 0 'Purchases Trx' value even though they have 
# used credit card for purchasing. So it is an error in the data
mydata$AVG_AMT_PER_PURCHASE[mydata$AVG_AMT_PER_PURCHASE==Inf]<- 0

  # Creating variable "PURCHASE TYPE"
mydata$PURCHASE_TYPE[mydata$INSTALLMENTS_PURCHASES==0 & mydata$ONEOFF_PURCHASES>0]<- "One-Off"
mydata$PURCHASE_TYPE[mydata$ONEOFF_PURCHASES==0 & mydata$INSTALLMENTS_PURCHASES>0]<- "Installments"
mydata$PURCHASE_TYPE[mydata$INSTALLMENTS_PURCHASES==mydata$ONEOFF_PURCHASES]<- "No Purchase"
mydata$PURCHASE_TYPE[mydata$ONEOFF_PURCHASES!= 0 & mydata$INSTALLMENTS_PURCHASES!=0]<- "Both"
count(mydata,PURCHASE_TYPE)

  # Getting all the numeric variables
vars<- sapply(mydata,is.numeric)
View(vars)

  # User defined function for calculating the descriptives:-

udf<- function(x){
  n<-length(x)
  nmiss<-sum(is.na(x))
  a<-x[!is.na(x)]
  m<- mean(a)
  max<- max(a)
  min<- min(a)
  p1<-quantile(a,0.01)
  p5<- quantile(a,0.05)
  p95<- quantile(a,0.95)
  p99<-quantile(a,0.99)
  
  return(c(count=n,nmiss=nmiss,mean=m,max=max,min=min,P1=p1,P5=p5,P95=p95,P99=p99))  
}
options(scipen = 999)
desc_stats<- data.frame(t(apply(mydata[vars],2,udf)))
write.csv(desc_stats,"Stats.csv")

  #  Outlier treatment:

udf2<- function(x){
  p5<- quantile(x,0.05,na.rm = T)
  p95<- quantile(x,0.95,na.rm = T)
  x[x<p5]<- p5   # Any value less than p5 are treated as Outlier
  x[x>p95]<- p95 # Any value greater than p95 are treated as Outlier
  return(x)
}
mydata[vars]<- data.frame(apply(mydata[vars],2,udf2))

   #  Missing value treatment:
# (i) Missing value treatment
mydata$CREDIT_LIMIT[is.na(mydata$CREDIT_LIMIT)]<- 4494.44945
mydata$MINIMUM_PAYMENTS[is.na(mydata$MINIMUM_PAYMENTS)]<- 864.2065423
mydata$LIMIT_USAGE[is.na(mydata$LIMIT_USAGE)]<- 0.388926409
mydata$P_MP_RATIO[is.na(mydata$P_MP_RATIO)]<- 9.350070124


#######################################################################################

                      #  Factor Analysis
                      #_ _ _ _ _ _ _ _ _ _
names(mydata[vars])
df<- mydata[vars]

# Correlation Matrix
com<- cor(df)
View(com)


#  Finding the number of factors:
library(psych)
scree(com,factors = T, pc=T, main = "Scree plot",hline = NULL, add = F)
eigen(com)$values

# Putting the variables into factors
FA<- fa(r=com,6,rotate = "varimax",fm="ml")
FA_SORT<- fa.sort(FA)
Loadings<-data.frame(FA_SORT$loadings[1:ncol(df),])
View(Loadings)
write.csv(Loadings,"Loadings.csv")
######################################################################################
names(df)
clus<- c("ONEOFF_PURCHASES","ONEOFF_PURCHASES_FREQUENCY","CREDIT_LIMIT",
         "PURCHASES_INSTALLMENTS_FREQUENCY","PURCHASES_TRX","BALANCE","LIMIT_USAGE",
         "CASH_ADVANCE","AVG_CASH_ADV_PER_TRX","CASH_ADVANCE_TRX")
# clus contains all the variables that will be used for clustering
final<- df[clus]
final_data<- data.frame(scale(final)) # standardizing data

# Building Clusters using K-means algorithm

cluster3<- kmeans(final_data,3)
cluster4<- kmeans(final_data,4)
cluster5<- kmeans(final_data,5)
cluster6<- kmeans(final_data,6)

mydata<- cbind(mydata,km_cluster3=cluster3$cluster,km_cluster4=cluster4$cluster,
               km_cluster5=cluster5$cluster,km_cluster6=cluster6$cluster)
View(mydata)
library(cluster)
clusplot(final_data, #dataframe
         cluster4$cluster, #clusterdata
         color = TRUE, #color
         #shade = TRUE, # Lines in clusters
         lines =6, # lines connecting centroids
         labels = 2 # Labels clusters and cases
)

#  Converting to factors:
mydata$km_cluster3<- factor(mydata$km_cluster3)
mydata$km_cluster4<- factor(mydata$km_cluster4)
mydata$km_cluster5<- factor(mydata$km_cluster5)
mydata$km_cluster6<- factor(mydata$km_cluster6)
str(mydata)

                   ########  PROFILING  #########

library(tables)
names(mydata)
options(scipen = 111)

profile<- tabular(BALANCE+BALANCE_FREQUENCY+PURCHASES+ONEOFF_PURCHASES+INSTALLMENTS_PURCHASES+
                    CASH_ADVANCE+PURCHASES_FREQUENCY+ONEOFF_PURCHASES_FREQUENCY+PURCHASES_INSTALLMENTS_FREQUENCY+
                    CASH_ADVANCE_FREQUENCY+CASH_ADVANCE_TRX+PURCHASES_TRX+CREDIT_LIMIT+PAYMENTS+
                    MINIMUM_PAYMENTS+PRC_FULL_PAYMENT+TENURE+LIMIT_USAGE+P_MP_RATIO+MONTHLY_AVG_PURCHASES+
                    MONTHLY_AVG_CASH_ADVANCE+AVG_AMT_PER_PURCHASE+AVG_CASH_ADV_PER_TRX~mean +(mean*km_cluster3)+
                    (mean*km_cluster4)+(mean*km_cluster5)+(mean*km_cluster6),
                  data=mydata)
profile1<-as.matrix(profile)
profile1<-data.frame(profile1)
write.csv(profile1,"Profile 1.csv")

mydata$PURCHASE_TYPE<- as.factor(mydata$PURCHASE_TYPE)
profile<- tabular(1+PURCHASE_TYPE ~ length + (length*km_cluster3)+(length*km_cluster4)+(length*km_cluster5)+(length*km_cluster6),
                  data=mydata)
profile2<-as.matrix(profile)
profile2<-data.frame(profile2)
write.csv(profile2,"Profile 2.csv")

###########++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++############

