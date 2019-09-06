# Telecom
library(caret)
library(dplyr)
library(car)
library(ROCR)
library(viridis)
library(data.table)
library(corrplot)
#data
telecom_org<-read.csv('C:\\DS Full stack\\Graded Assignments\\09 - Capstone Project  and Certitication\\telecomfinal.csv')
telecom<- read.csv('C:\\DS Full stack\\Graded Assignments\\09 - Capstone Project  and Certitication\\telecomfinal.csv')
summary(telecom)
colSums(is.na(telecom))
########################################################################
#########*********analysing with the structure of data*********#########
########################################################################
# # check for unique values in the dataset
# # no of unique variable
apply(telecom, 2, function(x) length(unique(x)))
########################################################################
# treating retdays
telecom$retdays[is.na(telecom$retdays)]<-0
range(telecom$retdays)
telecom$retdays<- replace(telecom$retdays, telecom$retdays>'0',1)
range(telecom$retdays)
telecom$retdays<- as.factor(telecom$retdays)
str(telecom$retdays)

# treating age1
sum(is.na(telecom$age1))  # here we get missing values 1152, now we gonna create missing as another level and replace it with 0 as it doesnt signifies anything 
telecom$age1[is.na(telecom$age1)]<- 0
range(telecom$age1)  ##### range 0-99
# Taking some categories in age1**** [0,20):A [20,40):B [40,60):C [60,80):D [80,100):E 
telecom$catage1<- cut(telecom$age1, breaks = c(0,20,40,60,80, 100),labels = c("A","B","C","D","E"), right = F)
telecom$catage1[1:10]

#treating age2
sum(is.na(telecom$age2))  # here we get missing values 1152, now we gonna create missing as another level and replace it with 0 as it doesnt signifies anything 
telecom$age2[is.na(telecom$age2)]<- 0
range(telecom$age2)  ##### range 0-99
# Taking some categories in age2**** [0,20):A [20,40):B [40,60):C [60,80):D [80,100):E 
telecom$catage2<- cut(telecom$age2, breaks = c(0,20,40,60,80, 100),labels = c("A","B","C","D","E"), right = F)
telecom$catage2[1:10]

#treating hnd_price
unique(telecom$hnd_price)   ###there are 18 unique values so i would like to convert factor
sum(is.na(telecom$hnd_price))  ### there are 636 missing values which i would replace with MISSING
telecom$hnd_price<- ifelse(is.na(telecom$hnd_price), "MISSING", as.character(telecom$hnd_price))
telecom$hnd_price<- as.factor(telecom$hnd_price)
levels(telecom$hnd_price)

##removing columns 'age1' and 'age2'
telecom<- telecom[-c(39,40)]
 

#converting numeric miss analysis to factors
telecom$actvsubs<- as.factor(telecom$actvsubs)
telecom$uniqsubs<- as.factor(telecom$uniqsubs)
telecom$forgntvl<- as.factor(telecom$forgntvl)
telecom$income<- as.factor(telecom$income)
telecom$models<- as.factor(telecom$models)
telecom$mtrcycle<- as.factor(telecom$mtrcycle)
telecom$truck<- as.factor(telecom$truck)
telecom$retdays<- as.factor(telecom$retdays)
telecom$numbcars<- as.factor(telecom$numbcars)
telecom$catage1<- as.factor(telecom$catage1)
telecom$catage2<- as.factor(telecom$catage2)
##############################################################################################################################################
#For character variables i created another level in place of NA for better analysis
#I can also say that I imputed the missing values by creating a new level for character variables
telecom$income<- ifelse(is.na(telecom$income), "MISSING", as.character(telecom$income))
telecom$area<- ifelse(is.na(telecom$area), "MISSING", as.factor(telecom$area))
telecom$hnd_webcap<- ifelse(is.na(telecom$hnd_webcap), "MISSING", as.factor(telecom$hnd_webcap))
telecom$ethnic<- ifelse(is.na(telecom$ethnic), "MISSING", as.factor(telecom$ethnic))
telecom$forgntvl<- ifelse(is.na(telecom$forgntvl), "MISSING", as.factor(telecom$forgntvl))
telecom$dwllsize<- ifelse(is.na(telecom$dwllsize), "MISSING", as.factor(telecom$dwllsize))
telecom$occu1<- ifelse(is.na(telecom$occu1), "MISSING", as.factor(telecom$occu1))
telecom$numbcars<- ifelse(is.na(telecom$numbcars), "MISSING", as.factor(telecom$numbcars))
telecom$truck<- ifelse(is.na(telecom$truck), "MISSING", as.factor(telecom$truck))
telecom$solflag<- ifelse(is.na(telecom$solflag), "MISSING", as.factor(telecom$solflag))
telecom$mailresp<- ifelse(is.na(telecom$mailresp), "MISSING", as.factor(telecom$mailresp))
telecom$car_buy<- ifelse(is.na(telecom$car_buy), "MISSING", as.factor(telecom$car_buy))
telecom$csa<- ifelse(is.na(telecom$csa), "MISSING", as.factor(telecom$csa))
telecom$prizm_social_one<- ifelse(is.na(telecom$prizm_social_one), "MISSING", as.factor(telecom$prizm_social_one))
telecom$refurb_new<- ifelse(is.na(telecom$refurb_new), "MISSING", as.factor(telecom$refurb_new))
telecom$marital<- ifelse(is.na(telecom$marital), "MISSING", as.factor(telecom$marital))
telecom$models<- ifelse(is.na(telecom$models), "MISSING", as.factor(telecom$models))
telecom$dwlltype<- ifelse(is.na(telecom$dwlltype), "MISSING", as.factor(telecom$dwlltype))
telecom$mailordr<- ifelse(is.na(telecom$mailordr), "MISSING", as.factor(telecom$mailordr))
telecom$mtrcycle<- ifelse(is.na(telecom$mtrcycle), "MISSING", as.factor(telecom$mtrcycle))
telecom$wrkwoman<- ifelse(is.na(telecom$wrkwoman), "MISSING", as.factor(telecom$wrkwoman))
telecom$proptype<- ifelse(is.na(telecom$proptype), "MISSING", as.factor(telecom$proptype))
telecom$cartype<- ifelse(is.na(telecom$cartype), "MISSING", as.factor(telecom$cartype))
telecom$children<- ifelse(is.na(telecom$children), "MISSING", as.factor(telecom$children))
telecom$div_type<- ifelse(is.na(telecom$div_type), "MISSING", as.factor(telecom$div_type))
###############################################################################################################################
telecom$income<- as.factor(telecom$income)
telecom$area<- as.factor(telecom$area)
telecom$hnd_webcap<- as.factor(telecom$hnd_webcap)
telecom$ethnic<- as.factor(telecom$ethnic)
telecom$forgntvl<- as.factor(telecom$forgntvl)
telecom$dwllsize<- as.factor(telecom$dwllsize)
telecom$occu1<- as.factor(telecom$occu1)
telecom$numbcars<- as.factor(telecom$numbcars)
telecom$truck<- as.factor(telecom$truck)
telecom$solflag<- as.factor(telecom$solflag)
telecom$mailresp<- as.factor(telecom$mailresp)
telecom$car_buy<- as.factor(telecom$car_buy)
telecom$csa<- as.factor(telecom$csa)
telecom$prizm_social_one<- as.factor(telecom$prizm_social_one)
telecom$refurb_new<- as.factor(telecom$refurb_new)
telecom$marital<- as.factor(telecom$marital)
telecom$models<- as.factor(telecom$models)
telecom$dwlltype<- as.factor(telecom$dwlltype)
telecom$mailordr<- as.factor(telecom$mailordr)
telecom$mtrcycle<- as.factor(telecom$mtrcycle)
telecom$wrkwoman<- as.factor(telecom$wrkwoman)
telecom$proptype<- as.factor(telecom$proptype)
telecom$cartype<- as.factor(telecom$cartype)
telecom$children<- as.factor(telecom$children)
telecom$div_type<- as.factor(telecom$div_type)
#####################################################
# #######################################################
var_num<- select_if(telecom, is.numeric) ##48 numeric variables
var_char<- select_if(telecom, is.factor) ##33 factor variables
 
colSums(is.na(var_num))  #missing values to be imputed later 
colSums(is.na(var_char)) #missing values imputed 

############################################################################################################
############# Data Exploration====> Profiling(dat-continuous variabe, var_char1- categorical variables)#############
##### Deciling continuous varibles basis target variable churn############

#<1> variable "mou_Mean"
telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat1
dat1$N<-unclass(telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat1$churn_perc<-dat1$n/dat1$N
dat1$varname<-rep("mou_Mean",nrow(dat1))
ggplot(dat1, aes(x=dec, y=churn_perc))+ geom_boxplot()
ggplot(dat1, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)
#*** we concluded that the people who are 
#above 0.30 would definitely leave
#the persons lying between 0.225 to 0.275 can stay if some actions are taken by the company 
#the persons who are below 0.225 will definitely stay  ***#

#<2> variable "totmrc_Mean"
telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat2
dat2$N<-unclass(telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat2$churn_perc<-dat2$n/dat2$N
dat2$varname<-rep("totmrc_Mean",nrow(dat2))
ggplot(dat2, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<3> variable "rev_Range"
telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat3
dat3$N<-unclass(telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat3$churn_perc<-dat3$n/dat3$N
dat3$varname<-rep("rev_Range",nrow(dat3))
ggplot(dat3, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<4> variable "mou_Range"
telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat4
dat4$N<-unclass(telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat4$churn_perc<-dat4$n/dat4$N
dat4$varname<-rep("mou_Range",nrow(dat4))
ggplot(dat4, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<5> variable "change_mou"
telecom%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat5
dat5$N<-unclass(telecom%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
dat5$churn_perc<-dat5$n/dat5$N
dat5$varname<-rep("change_mou",nrow(dat5))
ggplot(dat5, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<6> variable "drop_blk_Mean"
telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat6
dat6$N<-unclass(telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat6$churn_perc<-dat6$n/dat6$N
dat6$varname<-rep("drop_blk_Mean",nrow(dat6))
ggplot(dat6, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<7> variable "drop_vce_Range"
telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat7
dat7$N<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat7$churn_perc<-dat7$n/dat7$N
dat7$varname<-rep("drop_vce_Range",nrow(dat7))
ggplot(dat7, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<8> variable "owylis_vce_Range"
telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat8
dat8$N<-unclass(telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat8$churn_perc<-dat8$n/dat8$N
dat8$varname<-rep("owylis_vce_Range",nrow(dat8))
ggplot(dat8, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<9> variable "mou_opkv_Range"
telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat9
dat9$N<-unclass(telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat9$churn_perc<-dat9$n/dat9$N
dat9$varname<-rep("mou_opkv_Range",nrow(dat9))
ggplot(dat9, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<10> variable "months"
telecom%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat10
dat10$N<-unclass(telecom%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
dat10$churn_perc<-dat10$n/dat10$N
dat10$varname<-rep("months",nrow(dat10))
ggplot(dat10, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<11> variable "totcalls"
telecom%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat11
dat11$N<-unclass(telecom%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
dat11$churn_perc<-dat11$n/dat11$N
dat11$varname<-rep("totcalls",nrow(dat11))
ggplot(dat11, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<12> variable "eqpdays"
telecom%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat12
dat12$N<-unclass(telecom%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%unname())[[2]]
dat12$churn_perc<-dat12$n/dat12$N
dat12$varname<-rep("eqpdays",nrow(dat12))
ggplot(dat12, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<13> variable "custcare_Mean"
telecom%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat13
dat13$N<-unclass(telecom%>%mutate(dec=ntile(custcare_Mean,n=7))%>%count(dec)%>%unname())[[2]]
dat13$churn_perc<-dat13$n/dat13$N
dat13$varname<-rep("custcare_Mean",nrow(dat13))
ggplot(dat13, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<14> variable "callwait_Mean"
telecom%>%mutate(dec=ntile(callwait_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat14
dat14$N<-unclass(telecom%>%mutate(dec=ntile(callwait_Mean,n=8))%>%count(dec)%>%unname())[[2]]
dat14$churn_perc<-dat14$n/dat14$N
dat14$varname<-rep("callwait_Mean",nrow(dat14))
ggplot(dat14, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)


#<15> variable "iwylis_vce_Mean"
telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat15
dat15$N<-unclass(telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=9))%>%count(dec)%>%unname())[[2]]
dat15$churn_perc<-dat15$n/dat15$N
dat15$varname<-rep("iwylis_vce_Mean",nrow(dat15))
ggplot(dat15, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)


#<16> variable "callwait_Range"
telecom%>%mutate(dec=ntile(callwait_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat16
dat16$N<-unclass(telecom%>%mutate(dec=ntile(callwait_Range,n=7))%>%count(dec)%>%unname())[[2]]
dat16$churn_perc<-dat16$n/dat16$N
dat16$varname<-rep("callwait_Range",nrow(dat16))
ggplot(dat16, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)


#<17> variable "ccrndmou_Range"
telecom%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat17
dat17$N<-unclass(telecom%>%mutate(dec=ntile(ccrndmou_Range,n=7))%>%count(dec)%>%unname())[[2]]
dat17$churn_perc<-dat17$n/dat17$N
dat17$varname<-rep("ccrndmou_Range",nrow(dat17))
ggplot(dat17, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)


#<18> variable "adjqty"
telecom%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat18
dat18$N<-unclass(telecom%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
dat18$churn_perc<-dat18$n/dat18$N
dat18$varname<-rep("adjqty",nrow(dat18))
ggplot(dat18, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)


# #<19> variable "ovrrev_Mean"
# telecom%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat19
# dat19$N<-unclass(telecom%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
# dat19$churn_perc<-dat19$n/dat19$N
# dat19$varname<-rep("ovrrev_Mean",nrow(dat19))

#<20> variable "rev_Mean"
telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat20
dat20$N<-unclass(telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat20$churn_perc<-dat20$n/dat20$N
dat20$varname<-rep("rev_Mean",nrow(dat20))
ggplot(dat20, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)


# #<21> variable "ovrmou_Mean"
# telecom%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat21
# dat21$N<-unclass(telecom%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
# dat21$churn_perc<-dat21$n/dat21$N
# dat21$varname<-rep("ovrmou_Mean",nrow(dat21))

#<22> variable "comp_vce_Mean"
telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat22
dat22$N<-unclass(telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat22$churn_perc<-dat22$n/dat22$N
dat22$varname<-rep("comp_vce_Mean",nrow(dat22))
ggplot(dat22, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<23> variable "plcd_vce_Mean"
telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat23
dat23$N<-unclass(telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat23$churn_perc<-dat23$n/dat23$N
dat23$varname<-rep("plcd_vce_Mean",nrow(dat23))
ggplot(dat23, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<24> variable "avg3mou"
telecom%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat24
dat24$N<-unclass(telecom%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
dat24$churn_perc<-dat24$n/dat24$N
dat24$varname<-rep("avg3mou",nrow(dat24))
ggplot(dat24, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<25> variable "avgmou"
telecom%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat25
dat25$N<-unclass(telecom%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
dat25$churn_perc<-dat25$n/dat25$N
dat25$varname<-rep("avgmou",nrow(dat25))
ggplot(dat25, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<26> variable "avg3qty"
telecom%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat26
dat26$N<-unclass(telecom%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
dat26$churn_perc<-dat26$n/dat26$N
dat26$varname<-rep("avg3qty",nrow(dat26))
ggplot(dat26, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<27> variable "avgqty"
telecom%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat27
dat27$N<-unclass(telecom%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
dat27$churn_perc<-dat27$n/dat27$N
dat27$varname<-rep("avgqty",nrow(dat27))

ggplot(dat27, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<28> variable "avg6mou"
telecom%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat28
dat28$N<-unclass(telecom%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
dat28$churn_perc<-dat28$n/dat28$N
dat28$varname<-rep("avg6mou",nrow(dat28))
ggplot(dat28, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<29> variable "avg6qty"
telecom%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat29
dat29$N<-unclass(telecom%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
dat29$churn_perc<-dat29$n/dat29$N
dat29$varname<-rep("avg6qty",nrow(dat29))
ggplot(dat29, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

# #<31> variable "opk_dat_Mean"==============> LESS THAN 4 DECILES (3): DROP
# telecom%>%mutate(dec=ntile(opk_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat31
# dat31$N<-unclass(telecom%>%mutate(dec=ntile(opk_dat_Mean,n=10))%>%count(dec)%>%unname())[[2]]
# dat31$churn_perc<-dat31$n/dat31$N
# dat31$varname<-rep("opk_dat_Mean",nrow(dat31))

#<32> variable "roam_Mean"
telecom%>%mutate(dec=ntile(roam_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat32
dat32$N<-unclass(telecom%>%mutate(dec=ntile(roam_Mean,n=6))%>%count(dec)%>%unname())[[2]]
dat32$churn_perc<-dat32$n/dat32$N
dat32$varname<-rep("roam_Mean",nrow(dat32))
ggplot(dat32, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

# 
# #<33> variable "recv_sms_Mean" =========> less than 4 deciles (3): DROP
# telecom%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat33
# dat33$N<-unclass(telecom%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%count(dec)%>%unname())[[2]]
# dat33$churn_perc<-dat33$n/dat33$N
# dat33$varname<-rep("recv_sms_Mean",nrow(dat33))

# #<34> variable "blck_dat_Mean"==========> DROP after treatment
# telecom%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat34
# dat34$N<-unclass(telecom%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%count(dec)%>%unname())[[2]]
# dat34$churn_perc<-dat34$n/dat34$N
# dat34$varname<-rep("blck_dat_Mean",nrow(dat34))

# #<35> variable "recv_sms_Mean" =============> less than 4 decile (3): DROP
# telecom%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat35
# dat35$N<-unclass(telecom%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%count(dec)%>%unname())[[2]]
# dat35$churn_perc<-dat35$n/dat35$N
# dat35$varname<-rep("recv_sms_Mean",nrow(dat35))

# #<36> variable "mou_pead_Mean" =============> less than 4 decile (3): DROP
# telecom%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat36
# dat36$N<-unclass(telecom%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%count(dec)%>%unname())[[2]]
# dat36$churn_perc<-dat36$n/dat36$N
# dat36$varname<-rep("mou_pead_Mean",nrow(dat36))

#<37> variable "da_Mean"
telecom%>%mutate(dec=ntile(da_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat37
dat37$N<-unclass(telecom%>%mutate(dec=ntile(da_Mean,n=8))%>%count(dec)%>%unname())[[2]]
dat37$churn_perc<-dat37$n/dat37$N
dat37$varname<-rep("da_Mean",nrow(dat37))
ggplot(dat37, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)


#<38> variable "da_Range"
telecom%>%mutate(dec=ntile(da_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat38
dat38$N<-unclass(telecom%>%mutate(dec=ntile(da_Range,n=8))%>%count(dec)%>%unname())[[2]]
dat38$churn_perc<-dat38$n/dat38$N
dat38$varname<-rep("da_Range",nrow(dat38))
ggplot(dat38, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

# #<39> variable "datovr_Mean"========> drop after treatment
# telecom%>%mutate(dec=ntile(datovr_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat39
# dat39$N<-unclass(telecom%>%mutate(dec=ntile(datovr_Mean,n=10))%>%count(dec)%>%unname())[[2]]
# dat39$churn_perc<-dat39$n/dat39$N
# dat39$varname<-rep("datovr_Mean",nrow(dat39))

# #<40> variable "datovr_Range"=========> Less than 4 deciles(3): DROP
# telecom%>%mutate(dec=ntile(datovr_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat40
# dat40$N<-unclass(telecom%>%mutate(dec=ntile(datovr_Range,n=10))%>%count(dec)%>%unname())[[2]]
# dat40$churn_perc<-dat40$n/dat40$N
# dat40$varname<-rep("datovr_Range",nrow(dat40))

# #<41> variable "drop_dat_Mean" =======> Drop after treatment
# telecom%>%mutate(dec=ntile(drop_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat41
# dat41$N<-unclass(telecom%>%mutate(dec=ntile(drop_dat_Mean,n=10))%>%count(dec)%>%unname())[[2]]
# dat41$churn_perc<-dat41$n/dat41$N
# dat41$varname<-rep("drop_dat_Mean",nrow(dat41))

#<42> variable "drop_vce_Mean"
telecom%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat42
dat42$N<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat42$churn_perc<-dat42$n/dat42$N
dat42$varname<-rep("drop_vce_Mean",nrow(dat42))
ggplot(dat42, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<43> variable "adjmou"
telecom%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat43
dat43$N<-unclass(telecom%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
dat43$churn_perc<-dat43$n/dat43$N
dat43$varname<-rep("adjmou",nrow(dat43))
ggplot(dat43, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<44> variable "totrev"
telecom%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat44
dat44$N<-unclass(telecom%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dat44$churn_perc<-dat44$n/dat44$N
dat44$varname<-rep("totrev",nrow(dat44))
ggplot(dat44, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)


#<45> variable "adjrev"
telecom%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat45
dat45$N<-unclass(telecom%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
dat45$churn_perc<-dat45$n/dat45$N
dat45$varname<-rep("adjrev",nrow(dat45))
ggplot(dat45, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<46> variable "avgrev"
telecom%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat46
dat46$N<-unclass(telecom%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
dat46$churn_perc<-dat46$n/dat46$N
dat46$varname<-rep("avgrev",nrow(dat46))
ggplot(dat46, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

#<47> variable "Customer_ID"
telecom%>%mutate(dec=ntile(Customer_ID,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat47
dat47$N<-unclass(telecom%>%mutate(dec=ntile(Customer_ID,n=10))%>%count(dec)%>%unname())[[2]]
dat47$churn_perc<-dat47$n/dat47$N
dat47$varname<-rep("Customer_ID",nrow(dat47))
ggplot(dat47, aes(dec, churn_perc))+geom_point(aes(color= churn_perc, size= churn_perc))+
  geom_smooth(aes(color= churn_perc, fill= churn_perc, size= churn_perc),method= 'loess')+
  scale_color_viridis(discrete = F, option = 'D')+ scale_fill_viridis(discrete = F)

# #<48> variable "comp_dat_Mean"========> drop after treatment
# telecom%>%mutate(dec=ntile(comp_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat48
# dat48$N<-unclass(telecom%>%mutate(dec=ntile(comp_dat_Mean,n=10))%>%count(dec)%>%unname())[[2]]
# dat48$churn_perc<-dat48$n/dat48$N
# dat48$varname<-rep("comp_dat_Mean",nrow(dat48))

# #<49> variable "plcd_dat_Mean" ========> drop after treatment
# telecom%>%mutate(dec=ntile(plcd_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat49
# dat49$N<-unclass(telecom%>%mutate(dec=ntile(plcd_dat_Mean,n=10))%>%count(dec)%>%unname())[[2]]
# dat49$churn_perc<-dat49$n/dat49$N
# dat49$varname<-rep("plcd_dat_Mean",nrow(dat49))

#####################*********** treating the factor variables*************#################################
#<C1> income
summary(telecom$income)
telecom%>%count(churn,levels= income)%>%filter(churn==1)-> datc1
datc1$N<- unclass(telecom%>%filter(income%in% datc1$levels)%>%count(income))[[2]]
datc1$ChurnPerc<- datc1$n/datc1$N
datc1$Var.Name<- rep("income", nrow(datc1))

#<C2> crclscod ============> drop
summary(telecom$crclscod)
telecom%>%count(churn,levels= crclscod)%>%filter(churn==1)-> datC2
datC2$N<- unclass(telecom%>%filter(crclscod%in% datC2$levels)%>%count(crclscod))[[2]]
datC2$ChurnPerc<- datC2$n/datC2$N
datC2$Var.Name<- rep("crclscod", nrow(datC2))

#<C3> asl_flag
summary(telecom$asl_flag)
telecom%>%count(churn,levels= asl_flag)%>%filter(churn==1)-> datc3
datc3$N<- unclass(telecom%>%filter(asl_flag%in% datc3$levels)%>%count(asl_flag))[[2]]
datc3$ChurnPerc<- datc3$n/datc3$N
datc3$Var.Name<- rep("asl_flag", nrow(datc3))

#<C4> prizm_social_one
summary(telecom$prizm_social_one)
telecom%>%count(churn,levels= prizm_social_one)%>%filter(churn==1)-> datc4
datc4$N<- unclass(telecom%>%filter(prizm_social_one%in% datc4$levels)%>%count(prizm_social_one))[[2]]
datc4$ChurnPerc<- datc4$n/datc4$N
datc4$Var.Name<- rep("prizm_social_one", nrow(datc4))

#<C5> area
summary(telecom$area)
telecom%>%count(churn,levels= area)%>%filter(churn==1)-> datc5
datc5$N<- unclass(telecom%>%filter(area%in% datc5$levels)%>%count(area))[[2]]
datc5$ChurnPerc<- datc5$n/datc5$N
datc5$Var.Name<- rep("area", nrow(datc5))

#<C7> refurb_new
summary(telecom$refurb_new)
telecom%>%count(churn,levels= refurb_new)%>%filter(churn==1)-> datc7
datc7$N<- unclass(telecom%>%filter(refurb_new%in% datc7$levels)%>%count(refurb_new))[[2]]
datc7$ChurnPerc<- datc7$n/datc7$N
datc7$Var.Name<- rep("refurb_new", nrow(datc7))

#<C8> hnd_webcap
summary(telecom$hnd_webcap)
telecom%>%count(churn,levels= hnd_webcap)%>%filter(churn==1)-> datc8
datc8$N<- unclass(telecom%>%filter(hnd_webcap%in% datc8$levels)%>%count(hnd_webcap))[[2]]
datc8$ChurnPerc<- datc8$n/datc8$N
datc8$Var.Name<- rep("hnd_webcap", nrow(datc8))

#<C9> marital
summary(telecom$marital)
telecom%>%count(churn,levels= marital)%>%filter(churn==1)-> datc9
datc9$N<- unclass(telecom%>%filter(marital%in% datc9$levels)%>%count(marital))[[2]]
datc9$ChurnPerc<- datc9$n/datc9$N
datc9$Var.Name<- rep("marital", nrow(datc9))

#<C10> ethnic
summary(telecom$ethnic)
telecom%>%count(churn,levels= ethnic)%>%filter(churn==1)-> datc10
datc10$N<- unclass(telecom%>%filter(ethnic%in% datc10$levels)%>%count(ethnic))[[2]]
datc10$ChurnPerc<- datc10$n/datc10$N
datc10$Var.Name<- rep("ethnic", nrow(datc10))

#<C11> models
summary(telecom$models)
telecom%>%count(churn,levels= models)%>%filter(churn==1)-> datc11
datc11$N<- unclass(telecom%>%filter(models%in% datc11$levels)%>%count(models))[[2]]
datc11$ChurnPerc<- datc11$n/datc11$N
datc11$Var.Name<- rep("models", nrow(datc11))

#<C12> actvsubs
summary(telecom$actvsubs)
telecom%>%count(churn,levels= actvsubs)%>%filter(churn==1)-> datc12
datc12$N<- unclass(telecom%>%filter(actvsubs%in% datc12$levels)%>%count(actvsubs))[[2]]
datc12$ChurnPerc<- datc12$n/datc12$N
datc12$Var.Name<- rep("actvsubs", nrow(datc12))

#<C13> uniqsubs
summary(telecom$uniqsubs)
telecom%>%count(churn,levels= uniqsubs)%>%filter(churn==1)-> datc13
datc13$N<- unclass(telecom%>%filter(uniqsubs%in% datc13$levels)%>%count(uniqsubs))[[2]]
datc13$ChurnPerc<- datc13$n/datc13$N
datc13$Var.Name<- rep("uniqsubs", nrow(datc13))

#<C14> forgntvl
summary(telecom$forgntvl)
telecom%>%count(churn,levels= forgntvl)%>%filter(churn==1)-> datc14
datc14$N<- unclass(telecom%>%filter(forgntvl%in% datc14$levels)%>%count(forgntvl))[[2]]
datc14$ChurnPerc<- datc14$n/datc14$N
datc14$Var.Name<- rep("forgntvl", nrow(datc14))

#<C15> dwlltype
summary(telecom$dwlltype)
telecom%>%count(churn,levels= dwlltype)%>%filter(churn==1)-> datc15
datc15$N<- unclass(telecom%>%filter(dwlltype%in% datc15$levels)%>%count(dwlltype))[[2]]
datc15$ChurnPerc<- datc15$n/datc15$N
datc15$Var.Name<- rep("dwlltype", nrow(datc15))

#<C16> dwllsize
summary(telecom$dwllsize)
telecom%>%count(churn,levels= dwllsize)%>%filter(churn==1)-> datc16
datc16$N<- unclass(telecom%>%filter(dwllsize%in% datc16$levels)%>%count(dwllsize))[[2]]
datc16$ChurnPerc<- datc16$n/datc16$N
datc16$Var.Name<- rep("dwllsize", nrow(datc16))

#<C17> mailordr
summary(telecom$mailordr)
telecom%>%count(churn,levels= mailordr)%>%filter(churn==1)-> datc17
datc17$N<- unclass(telecom%>%filter(mailordr%in% datc17$levels)%>%count(mailordr))[[2]]
datc17$ChurnPerc<- datc17$n/datc17$N
datc17$Var.Name<- rep("mailordr", nrow(datc17))

#<C18> occu1
summary(telecom$occu1)
telecom%>%count(churn,levels= occu1)%>%filter(churn==1)-> datc18
datc18$N<- unclass(telecom%>%filter(occu1%in% datc18$levels)%>%count(occu1))[[2]]
datc18$ChurnPerc<- datc18$n/datc18$N
datc18$Var.Name<- rep("occu1", nrow(datc18))

#<C19> mtrcycle
summary(telecom$mtrcycle)
telecom%>%count(churn,levels= mtrcycle)%>%filter(churn==1)-> datc19
datc19$N<- unclass(telecom%>%filter(mtrcycle%in% datc19$levels)%>%count(mtrcycle))[[2]]
datc19$ChurnPerc<- datc19$n/datc19$N
datc19$Var.Name<- rep("mtrcycle", nrow(datc19))

#<C20> numbcars
summary(telecom$numbcars)
telecom%>%count(churn,levels= numbcars)%>%filter(churn==1)-> datc20
datc20$N<- unclass(telecom%>%filter(numbcars%in% datc20$levels)%>%count(numbcars))[[2]]
datc20$ChurnPerc<- datc20$n/datc20$N
datc20$Var.Name<- rep("numbcars", nrow(datc20))

#<C21> retdays
summary(telecom$retdays)
telecom%>%count(churn,levels= retdays)%>%filter(churn==1)-> datc21
datc21$N<- unclass(telecom%>%filter(retdays%in% datc21$levels)%>%count(retdays))[[2]]
datc21$ChurnPerc<- datc21$n/datc21$N
datc21$Var.Name<- rep("retdays", nrow(datc21))

#<C22> truck
summary(telecom$truck)
telecom%>%count(churn,levels= truck)%>%filter(churn==1)-> datc22
datc22$N<- unclass(telecom%>%filter(truck%in% datc22$levels)%>%count(truck))[[2]]
datc22$ChurnPerc<- datc22$n/datc22$N
datc22$Var.Name<- rep("truck", nrow(datc22))

#<C23> wrkwoman
summary(telecom$wrkwoman)
telecom%>%count(churn,levels= wrkwoman)%>%filter(churn==1)-> datc23
datc23$N<- unclass(telecom%>%filter(wrkwoman%in% datc23$levels)%>%count(wrkwoman))[[2]]
datc23$ChurnPerc<- datc23$n/datc23$N
datc23$Var.Name<- rep("wrkwoman", nrow(datc23))

#<C24> solflag
summary(telecom$solflag)
telecom%>%count(churn,levels= solflag)%>%filter(churn==1)-> datc24
datc24$N<- unclass(telecom%>%filter(solflag%in% datc24$levels)%>%count(solflag))[[2]]
datc24$ChurnPerc<- datc24$n/datc24$N
datc24$Var.Name<- rep("solflag", nrow(datc24))

#<C25> proptype
summary(telecom$proptype)
telecom%>%count(churn,levels= proptype)%>%filter(churn==1)-> datc25
datc25$N<- unclass(telecom%>%filter(proptype%in% datc25$levels)%>%count(proptype))[[2]]
datc25$ChurnPerc<- datc25$n/datc25$N
datc25$Var.Name<- rep("proptype", nrow(datc25))

#<C26> mailresp
summary(telecom$mailresp)
telecom%>%count(churn,levels= mailresp)%>%filter(churn==1)-> datc26
datc26$N<- unclass(telecom%>%filter(mailresp%in% datc26$levels)%>%count(mailresp))[[2]]
datc26$ChurnPerc<- datc26$n/datc26$N
datc26$Var.Name<- rep("mailresp", nrow(datc26))

#<C27> cartype
summary(telecom$cartype)
telecom%>%count(churn,levels= cartype)%>%filter(churn==1)-> datc27
datc27$N<- unclass(telecom%>%filter(cartype%in% datc27$levels)%>%count(cartype))[[2]]
datc27$ChurnPerc<- datc27$n/datc27$N
datc27$Var.Name<- rep("cartype", nrow(datc27))

#<C28> car_buy
summary(telecom$car_buy)
telecom%>%count(churn,levels= car_buy)%>%filter(churn==1)-> datc28
datc28$N<- unclass(telecom%>%filter(car_buy%in% datc28$levels)%>%count(car_buy))[[2]]
datc28$ChurnPerc<- datc28$n/datc28$N
datc28$Var.Name<- rep("car_buy", nrow(datc28))

#<C29> children
summary(telecom$children)
telecom%>%count(churn,levels= children)%>%filter(churn==1)-> datc29
datc29$N<- unclass(telecom%>%filter(children%in% datc29$levels)%>%count(children))[[2]]
datc29$ChurnPerc<- datc29$n/datc29$N
datc29$Var.Name<- rep("children", nrow(datc29))

#<C30> csa==============> drop
summary(telecom$csa)
telecom%>%count(churn,levels= csa)%>%filter(churn==1)-> datc30
datc30$N<- unclass(telecom%>%filter(csa%in% datc30$levels)%>%count(csa))[[2]]
datc30$ChurnPerc<- datc30$n/datc30$N
datc30$Var.Name<- rep("csa", nrow(datc30))

#<C31> div_type
summary(telecom$div_type)
telecom%>%count(churn,levels= div_type)%>%filter(churn==1)-> datc31
datc31$N<- unclass(telecom%>%filter(div_type%in% datc31$levels)%>%count(div_type))[[2]]
datc31$ChurnPerc<- datc31$n/datc31$N
datc31$Var.Name<- rep("div_type", nrow(datc31))

#<C32> catage1
summary(telecom$catage1)
telecom%>%count(churn,levels= catage1)%>%filter(churn==1)-> datc32
datc32$N<- unclass(telecom%>%filter(catage1%in% datc32$levels)%>%count(catage1))[[2]]
datc32$ChurnPerc<- datc32$n/datc32$N
datc32$Var.Name<- rep("catage1", nrow(datc32))

#<C33> catage2
summary(telecom$catage2)
telecom%>%count(churn,levels= catage2)%>%filter(churn==1)-> datc33
datc33$N<- unclass(telecom%>%filter(catage2%in% datc33$levels)%>%count(catage2))[[2]]
datc33$ChurnPerc<- datc33$n/datc33$N
datc33$Var.Name<- rep("catage2", nrow(datc33))

#<C34> hnd_price
summary(telecom$hnd_price)
telecom%>%count(churn,levels= hnd_price)%>%filter(churn==1)-> datc34
datc34$N<- unclass(telecom%>%filter(hnd_price%in% datc34$levels)%>%count(hnd_price))[[2]]
datc34$ChurnPerc<- datc34$n/datc34$N
datc34$Var.Name<- rep("hnd_price", nrow(datc34))

################################################################################################################### # delete plcd_dat_Mean and plcd_vce_Mean
###**** treating data to minimise correlation without loss of data****###
telecom$attempt_mean<- telecom$plcd_dat_Mean+ telecom$plcd_vce_Mean
which(colnames(telecom)=="plcd_vce_Mean")
telecom<- telecom[-c(79,24)]

#delete comp_dat_Mean and comp_vce_Mean

telecom$complete_mean<- telecom$comp_dat_Mean+ telecom$comp_vce_Mean
which(colnames(telecom)=="comp_vce_Mean")
telecom<- telecom[-c(77,23)]

#DROP_BLK_MEAN=BLCK_DAT_MEAN + DROP_DAT_MEAN + DROP_VCE_MEAN
#so we would remove blck_dat_mean, drop_dat_mean+drop_vce_mean
telecom<- telecom[-c(54,69,70)]

telecom$ovrrev_Mean<- telecom$datovr_Mean
#so we would drop datovr_Mean
telecom<- telecom[-65]

# we would calculate the calls per minute of 3 months as we are more 
# interested in recent data
telecom$call_per_min3<- telecom[,"avg3qty"]/ telecom[,"avg3mou"]
# as we are not interested in 6 month data we will drop 
# avg6qty, avg6mou, avg3qty, avg3mou, avgqty,avgmou
telecom<- telecom[-c(28,25,23,27,24,26)]

#########################################################################################################
# Outlier treatment:
# Method used: Capping
capoutlier<- function(x){
    qnt<- quantile(x,probs = c(.25, .75), na.rm = T)
    caps<- quantile(x, probs = c(0.10, .90), na.rm = T )
    H<- 1.5*IQR(x, na.rm = T)
    x[x< (qnt[1] - H)]<- caps[1]
    x[x> (qnt[2] + H)]<- caps[2]
    return(x)
}
telecom$mou_Mean= capoutlier(telecom$mou_Mean)
boxplot(telecom$mou_Mean)

telecom$rev_Range= capoutlier(telecom$rev_Range)
boxplot(telecom$rev_Range)

telecom$change_mou= capoutlier(telecom$change_mou)
boxplot(telecom$change_mou)

telecom$drop_vce_Range= capoutlier(telecom$drop_vce_Range)
boxplot(telecom$drop_vce_Range)

telecom$mou_opkv_Range= capoutlier(telecom$mou_opkv_Range)
boxplot(telecom$mou_opkv_Range)

telecom$totcalls= capoutlier(telecom$totcalls)
boxplot(telecom$totcalls)

telecom$iwylis_vce_Mean= capoutlier(telecom$iwylis_vce_Mean)
boxplot(telecom$iwylis_vce_Mean)

telecom$opk_dat_Mean= capoutlier(telecom$opk_dat_Mean)
boxplot(telecom$opk_dat_Mean)

telecom$recv_sms_Mean= capoutlier(telecom$recv_sms_Mean)
boxplot(telecom$recv_sms_Mean)

telecom$mou_pead_Mean= capoutlier(telecom$mou_pead_Mean)
boxplot(telecom$mou_pead_Mean)

telecom$adjmou= capoutlier(telecom$adjmou)
boxplot(telecom$adjmou)

telecom$adjrev= capoutlier(telecom$adjrev)
boxplot(telecom$adjrev)

telecom$totmrc_Mean= capoutlier(telecom$totmrc_Mean)
boxplot(telecom$totmrc_Mean)

telecom$owylis_vce_Range= capoutlier(telecom$owylis_vce_Range)
boxplot(telecom$owylis_vce_Range)

telecom$drop_blk_Mean= capoutlier(telecom$drop_blk_Mean)
boxplot(telecom$drop_blk_Mean)

telecom$months= capoutlier(telecom$months)
boxplot(telecom$months)

telecom$eqpdays= capoutlier(telecom$eqpdays)
boxplot(telecom$eqpdays)

telecom$callwait_Range= capoutlier(telecom$callwait_Range)
boxplot(telecom$callwait_Range)

telecom$adjqty= capoutlier(telecom$adjqty)
boxplot(telecom$adjqty)

telecom$rev_Mean= capoutlier(telecom$rev_Mean)
boxplot(telecom$rev_Mean)

telecom$totrev= capoutlier(telecom$totrev)
boxplot(telecom$totrev)

telecom$avgrev= capoutlier(telecom$avgrev)
boxplot(telecom$avgrev)

telecom$attempt_mean= capoutlier(telecom$attempt_mean)
boxplot(telecom$attempt_mean)

telecom$complete_mean= capoutlier(telecom$complete_mean)
boxplot(telecom$complete_mean)

telecom$call_per_min3= capoutlier(telecom$call_per_min3)
boxplot(telecom$call_per_min3)

telecom$mou_Range= capoutlier(telecom$mou_Range)
boxplot(telecom$mou_Range)

telecom$callwait_Mean= capoutlier(telecom$callwait_Mean)
boxplot(telecom$callwait_Mean)  # changes 0.90 to 0.85 and 0.10 to 0.15  

telecom$da_Range= capoutlier(telecom$da_Range)
boxplot(telecom$da_Range) #changes 0.90to 0.85 and 0.10 to 0.15

telecom$datovr_Range= capoutlier(telecom$datovr_Range)
boxplot(telecom$datovr_Range) #changes 0.90to 0.85 and 0.10 to 0.15

telecom$custcare_Mean= capoutlier(telecom$custcare_Mean)
boxplot(telecom$custcare_Mean)  #changes 0.90to 0.85 and 0.10 to 0.15

telecom$ovrrev_Mean= capoutlier(telecom$ovrrev_Mean)
boxplot(telecom$ovrrev_Mean)  #changes 0.90to 0.85 and 0.10 to 0.15

telecom$ovrmou_Mean= capoutlier(telecom$ovrmou_Mean)
boxplot(telecom$ovrmou_Mean)  #changes 0.90to 0.85 and 0.10 to 0.15

telecom$da_Mean= capoutlier(telecom$da_Mean)
boxplot(telecom$da_Mean)  #changes 0.90to 0.85 and 0.10 to 0.15

telecom$roam_Mean= capoutlier(telecom$roam_Mean)
boxplot(telecom$roam_Mean)  #changes 0.90to 0.80 and 0.10 to 0.20

telecom$ccrndmou_Range= capoutlier(telecom$ccrndmou_Range)
boxplot(telecom$ccrndmou_Range)  #changes 0.90to 0.80 and 0.10 to 0.20

##############################################################################################
# Missing value treatment
# Method Used: median Imputation
var_num<- select_if(telecom, is.numeric)
var_num1<- names(var_num)

for (k in names(telecom)) {
  
  if (k %in% var_num1) {
     
     #impute numeric variables with median
     med<- median(telecom[[k]], na.rm=T)
     set(x= telecom, which(is.na(telecom[[k]])),k,med)
   }
}
colSums(is.na(var_num))
colSums(is.na(telecom))

####################################################################################
######****** model building******######
#<1> fixing the randomness by set.seed
#<2> creating train and test dataset
#<3> checking the class imbalance 
#<4> checking for correlation from correlation matrix
#<5> running model with all variables 
#<6> running model by removing each insignificant variable one by one 
#<7> creating dummies to pass each significant factor level
#<8> checking the degree of imporance of each variable
#<9> checking the VIF(variance influence factor) if greater than 5 we remove that variable for removing collinearity
set.seed(333)
index<- sample(nrow(telecom), 0.80*nrow(telecom), replace = FALSE)
train<- telecom[index,]
test<- telecom[-index,]
nrow(train)
nrow(test)
table(train$churn)/53037
table(test$churn)/13260

#############################################################################################
##*** to drop numeric variables we need to create corelation matrix and drop the variables with
###highly correlated variables are removed *** here i took 0.7 as the cutoff for highly correlated variables****###
########creating the correlation matrix
#remove the zero variance predictors
zv<- apply(var_num,2, function(x) length(unique(x))==1)
dfr<- var_num[,!zv]
n= length(colnames(dfr)) ###32 variables without zero variance 
corescore= round(cor(dfr, use = "complete.obs"),2) 
highlyCorrelated<- findCorrelation(corescore, cutoff = (0.7), verbose = F)
var_high_corr=colnames(var_num[,highlyCorrelated])
important_var= colnames(var_num[, -highlyCorrelated])

### checking the correlation plot 
corrplot(corescore, type = "upper", order= "hclust", tl.col = "black", tl.srt = 45)
#positive correlation are displayed in blue and negative correlation in red colour.
#colour intensity and size of the circle are proportional to the correlation coeeficients.
#in right side of the correlogram, the legend color shows the correlation coeeficients and the corresponding colour

############################################################################################
mod1<- glm(churn~ totmrc_Mean + mou_Range + change_mou + drop_blk_Mean + drop_vce_Range  
           + mou_opkv_Range + months + adjmou + eqpdays + iwylis_vce_Mean + ovrmou_Mean + adjrev 
           + avgrev  + complete_mean + call_per_min3 + asl_flag + prizm_social_one + area            
           + refurb_new + marital + ethnic + models + uniqsubs + dwlltype + retdays + truck 
           + mailresp + catage1  , train, family = binomial)

summary(mod1)
####################
#checking the variable importance
imp<- as.data.frame(varImp(mod1))
imp<- data.frame(overall= imp$Overall, names= rownames(imp))
variable_imp<-imp[order(imp$overall, decreasing = T),]
#####****** this gives me the following info******#####
# <1> eqpdays is affecting the model most and likewise other variables as shown
#     in decreasing order  
# <2> as we go down we see the factor levels which are least importance can be removed 
# to do that we need to create dummies for the factor variables and remove the insignificant levels

#####****** checking the vif for the model******#####
vif(mod1)  # Error in vif.default(mod1) : there are aliased coefficients in the model
# aliased coefficients signifies collinearity is present 
# let me check the vif after removing the factor levels 
fit<- lm(mod1)
ld.vars<- attributes(alias(fit)$Complete)$dimnames[[1]]
ld.vars
# "ethnicMISSING" "modelsMISSING" "truckMISSING" these are factor levels which can be removed by creating dummies
# if these would be numeric variables then after removing them from the model we would run the below command
# mod1_new<- as.formula(paste(paste(deparse(mod1), collapse = ""),paste(ld.vars,collapse = "-"),sep = "-"))
# fit.new<- lm(mod1_new)
# vif(fit.new)
#######################################################################################################
#########********* taking all significant variables and creating data*********#########

select_vars<- select(telecom,totmrc_Mean , mou_Range , change_mou , drop_blk_Mean , drop_vce_Range  
                    , mou_opkv_Range , months , eqpdays , iwylis_vce_Mean , ovrmou_Mean , adjrev 
                    , avgrev , adjmou , complete_mean , call_per_min3 , asl_flag , prizm_social_one , area            
                    , refurb_new , marital , ethnic , models , uniqsubs , dwlltype , retdays , truck 
                    , mailresp , catage1 , churn)

#######################################################################################################
#########********* creating dummy variables for test, train, telecom*********#########
#?dummyVars
dmy<- dummyVars("~ .-csa-crclscod", data = train)
trsf_df_tr<- data.frame(predict(dmy, newdata = train))

dmy1<- dummyVars("~ .-csa-crclscod", data = test)
trsf_df_tst<- data.frame(predict(dmy, newdata = test))

dmy2<- dummyVars("~ .-csa-crclscod", data = telecom)
trsf_df_tele<- data.frame(predict(dmy, newdata = telecom))


####################################################################################################
#######******* passing the significant levels to the model*******#######
mod_final<- glm(churn~ totmrc_Mean + mou_Range + change_mou + drop_blk_Mean + drop_vce_Range  
                + mou_opkv_Range + months + eqpdays+ ovrmou_Mean + iwylis_vce_Mean + ovrmou_Mean  
                + avgrev + adjmou  + complete_mean + call_per_min3 + asl_flag.Y + prizm_social_one.2 
                + prizm_social_one.4 + area.11 + area.12 + area.14 + area.17 + refurb_new.2 
                + marital.MISSING + ethnic.11 + ethnic.17 + ethnic.2 + uniqsubs.2 + uniqsubs.3 
                + uniqsubs.4  + uniqsubs.5 + uniqsubs.7 + dwlltype.2 + retdays.1 + mailresp.MISSING 
                + catage1.C + catage1.D , trsf_df_tr , family = binomial)
summary(mod_final)
summary_glm<-summary(mod_final)
####****checking the varable importance of mod_final****####
imp1<- as.data.frame(varImp(mod_final))
imp1<- data.frame(overall= imp1$Overall, names= rownames(imp1))
variable_imp1<-imp1[order(imp1$overall, decreasing = T),]

####****checking the vif of mod_final****####
vif(mod_final) 
# all vif's are less than 5 => no multicollinearity => the model can be accepted
# we got aliased coefficients in mod1 which was due to the insignificant factor levels which were correlated 
# after removing the insignificant levels we got rid of the colinearity in variables and the vif ran successfully

####################****** verifying my model*******########################
# we check two things for this model
#<1> p-values: values below 0.05 indicates significance, which means the coefficiennt or so called parameters that are estimated by our model are reliable.
#<2> pseudo R square: this value ranging from 0 to 1 indicates how much variance is explained by our model

list(summary_glm$coefficients, round(1-(summary_glm$deviance / summary_glm$null.deviance),2))
# variables with probability greater than 0.05 are removed from the model
# A fast check on all p-values of the model indicates significance,
# meaning that our model is a legitimate one.
# A pseudo R square of 0.04 tells only 4% of the variance is explained.
# in other words it is telling that the model isn't powerfull enough 
# to predict the customers that left with high reliability.
# this is due to the dataset imbalance. 
###################################################################################################################

### confusion matrix and prediction test ###
pred<- predict(mod_final,trsf_df_tst, type = "response")
head(pred)
range(pred)
hist(pred)
table(trsf_df_tr$churn)
table(trsf_df_tst$churn)
pred<- ifelse(pred>=0.21,1,0)
pred1<- as.factor(pred)
trsf_df_tst$churn <- as.factor(trsf_df_tst$churn)
confusionMatrix(pred1, trsf_df_tst$churn, positive = '1')  

###################################################################################################
p1<- predict(mod_final, trsf_df_tele, type = "response")
head(p1)
hist(p1)
p1<- prediction(p1, trsf_df_tele$churn)
eval<- performance(p1,"acc")
plot(eval)

#identifying best cutoff and accuracy
max<- which.max(slot(eval,"y.values")[[1]])
acc<- slot(eval, 'y.values')[[1]][[max]]
cut<- slot(eval, 'x.values')[[1]][[max]]
print(c(Accuracy=acc, Cutoff=cut))
# Accuracy    Cutoff 
# 0.7609696 0.5462516 

#visualizing optimal cutoff
plot(unlist(performance(p1, "sens")@x.values),
     unlist(performance(p1,"sens")@y.values),
    lwd=1, ylab= "sensitivity", xlab= "cutoff")
par(new= TRUE)
plot(unlist(performance(p1, "spec")@x.values),
     unlist(performance(p1,"spec")@y.values),
     lwd=1, col= "red", ylab= "", xlab= "")
axis(4, at= seq(0,1,0.2))
mtext("specificity", side = 4, padj = -2, col = 'red')
grid()
abline(v=0.21, untf = F)   # adjusted cutoff for better senitivity
abline(v=0.241, untf = F)  # optimal cutoff where sensitivity and specificity are maximum
################
roc<- performance(p1, 'tpr', 'fpr')
plot(roc, main= "Roc Curve", ylab= "Sensitivity", xlab= "1-Specificity")
abline(a=0, b=1)    # 50% line that divides the plot into 2 halfs

#auc
auc<- performance(p1,"auc")@y.values[[1]]
auc
round(auc,2)
legend(.5,.2,round(auc,4),title = "AUC", cex = 0.5)

###################################################################################################
########***** problem questions *****########

# <1> What are the top five factors driving likelihood of churn at Mobicom?

modcoef<- summary(mod_final)[["coefficients"]]
head(modcoef[order(modcoef[,4]),])
# From the summary of mod_final by arranging the probability in decending order  
# we can get top 5 factors driving likelihood of churn
#                       Estimate    Std. Error    z value      Pr(>|z|)
# eqpdays             1.322194e-03 5.739701e-05  23.035946 2.034658e-117
# retdays.1           7.832697e-01 5.205975e-02  15.045591  3.690229e-51
# months             -2.216305e-02 1.724644e-03 -12.850802  8.512286e-38
# complete_mean      -2.957511e-03 2.469398e-04 -11.976648  4.709827e-33
# call_per_min3       9.281897e-01 9.127791e-02  10.168831  2.731677e-24

# We got the same result using varImp function. 
# To specify the coefficient values i did this.

#************************************************************************************************************************#

# <2>  Validation of survey findings.
# <a> Whether "cost and billing" and "network and service quality" are important factors influencing churn behaviour.
# <Ans> The factors affecting "cost and network" are totmrc_Mean and avgrev
#       seeing the beta coefficients of these two variables
list(summary_glm$coefficients)
#       totmrc_Mean   -0.005983219
#       avgrev          0.002507759
# variable totmrc_Mean has beta coefficient of 0.005983219 meaning a unit increase in this variable is causing 
# decrease in churn by 0.005983219/unit

# variable avgrev has beta coefficient of 0.002507759 meaning a unit increase in this variable is causing 
# increase in churn by 0.002507759/unit

# So from the above we can conclude that "cost and billing" is not NOT very important factors here that influence
# churn behaviour at Mobicom

# The variables that explains "network and service quality"
# VARIABLES         BETA COEFFICIENTS
# mou_Range           0.0004129735
# change_mou         -0.0006244418
# drop_blk_Mean       0.009055601
# drop_vce_Range      0.01442898
# mou_opkv_Range     -0.0003691965
# iwylis_vce_Mean    -0.01282256
# ovrmou_Mean         0.003932938
# complete_mean      -0.002957511
# call_per_min3       0.9281897
# retdays.1           0.7832697
# months             -0.02216305
# from the above statistics, data explains the following:

# variables mou_Range i.e. with a unit increase in "Range of number of minutes of use", 
# there is a increase in churn by 0.0004129735

# variables change_mou i.e. with a unit increase in "Percentage change in monthly minutes of use vs previous three month average",
# there is a decrease in churn by 0.0006244418

# variables drop_blk_Mean i.e. with a unit increase in "mean number of dropped or blocked calls", 
# there is a increase in churn by 0.009055601
 
# variables drop_vce_Range i.e. with a unit increase in "Range of number of dropped voice calls", 
# there is a increase in churn by 0.01442898
 
# variables mou_opkv_Range i.e. with a unit increase in "Range of unrounded minutes of use of off-peak voice calls"
# there is a decrease in churn by 0.0003691965
 
# variables iwylis_vce_Mean i.e. with a unit increase in "Mean number of inbounded wireless to wireless voice calls",
# there is a decrease in churn by 0.01282256
 
# variables complete_mean i.e. with a unit increase in "Mean number of completed voice and data calls", 
# there is a decrease in churn by 0.002957511
 
# variables retdays.1 representing the values captured in the variable retdays i.e with a unit increase in
# "number of days since last retention call", there is a increase in churn by 0.7832697

# variables retdays.1 representing the values captured in the variable retdays i.e with a unit increase in
# "number of days since last retention call", there is a increase in churn by 0.7832697

# variables (call_per_min_3 = avg3qty/avg3mou) i.e. average monthly number of calls over the previous 3 months per average monthly minutes of use over the previous 3 months,
# so variable calls_per_min_3 represents calls made per minute in the previous 3 months, i.e. with a unit increase in "calls per minutes" there is a increase in churn by 0.9281897

# variable ovrmou_Mean i.e. with a unit incrwase in "Mean overage minutes of use", 
# there is increase in churn by  0.003932938.

# variable months i.e. with a unit incrwase in "total number of months in service", 
# there is decrease in churn by  0.02216305.

# Of the above variables, the beta coefficeient of variable retdays.1 and months is expressing a very important
# factors influencing Churn behaviour.
# That is with the increase in the number of days since a customer makes a retention call, the customer's chances of churning is very high. 
# And with the increase in the number of months in service, the customer,s chances of churning is very high.
# Special attention is to be given to these customers and they should be given offers and their complaints and queries should be attented efficiently.

# <2b> Are data usage connectivity issues turning out to be costly? In other words, is it leading to churn?

# comp_dat_Mean- Mean no.of completed data calls.
# plcd_dat_Mean- Mean no.of attempted data calls placed 
# opk_dat_Mean- Mean no.of off-peak data Calls
# blck_dat_Mean- Mean no.of blocked /failed data calls
# datovr_Mean- mean revenue of data overage
# drop_dat_Mean- Mean no.of dropped/failed data calls
# The above variables represent data usage connectivity.

# the data quality report for all the above variables show that only 10% to 15% customers are actually 
# making data calls or using the internet
# so we can see that customers are not using the internet. So it would be good to work
# towards attaining more customers to use data and also towards providing quality network connectivity 
# and service to provide maximum customer satisfaction and reduce churn.
# Since there is not enough usable data for the above variables they are not showing any influence
# on the churn behaviour at Mobicom

#************************************************************************************************************************************************************************************************#

# <3> Would you recommend rate plan migration as a proactive retention strategy?
summary(mod_final)
# <sol> yes, but not for all
# Reason: variable ovrmou_Mean has beta coefficient 0.003933; its the mean overage overage minutes of use 
# coefficient not strong to show Churn activity, so we consider for few set of customers who have some
# case basis validation success. But rate plan migration as a proactive retention strategy will not work for all 

#************************************************************************************************************************************************************************************************#

# <4> What would be your recommendation on how to use this churn model for prioritasation
# of customers for a proavtive retention campaingns in the future?

# <sol>
dmy1<- dummyVars("~ .-csa-crclscod", data = test)
trsf_df_tst<- data.frame(predict(dmy, newdata = test))
library(gains)
gains(trsf_df_tst$churn, predict(mod_final, type = "response", trsf_df_tst), groups=10)
dt=gains(trsf_df_tst$churn, predict(mod_final, type = "response", trsf_df_tst), groups=10)
graphics::plot(dt$depth,dt$cume.lift, type="l", ylab="cumulative lift", xlab="bucket")
grid()

#extracting the gain table
gaintable<- data.frame(dt$depth, dt$obs, dt$cume.obs,dt$mean.resp,dt$cume.mean.resp,dt$cume.pct.of.total, dt$lift, dt$cume.lift, dt$mean.prediction, dt$min.prediction, dt$max.prediction)
# top 20% probability shows 30.03827% are more likely to churn 

trsf_df_tst$prob<- predict(mod_final, type = "response", newdata = trsf_df_tst)
quantile(trsf_df_tst$prob, prob= seq(0.10,1.00,0.10))
# 10%       20%       30%       40%       50%       60%       70%       80%       90%          100% 
# 0.1376621 0.1657367 0.1878866 0.2090296 0.2299393 0.2511773 0.2749553 0.3049566 0.3520554  0.8425431
# Above % shows 20% of the probability contains <from 80 to 100%> - 0.3049566 to 0.8425431
# This notifies us it may highly likely churn

# Predict a Customer who will churn
## apply cutoff

pred2<- predict(mod_final, type = "response", newdata = trsf_df_tst)
pred3<- ifelse(pred2>=0.3049566,1,0) 
table(pred3, trsf_df_tst$churn) #942

# take upto <from 80 to 100> as per quantile result
# and consider churn value(0 or 1: here 1) then to predict customer use unique variable to identify via customer_id
exp_prediction<- trsf_df_tst[trsf_df_tst$prob>0.3049566 & trsf_df_tst$prob<= 0.8425431 & trsf_df_tst$churn=='1', "Customer_ID" ]
exp_prediction<- as.data.frame(exp_prediction)
nrow(exp_prediction)  #942
write.csv(exp_prediction,"expected prediction.csv", row.names = F)

### model mod_final is used to predict list of Mobicom customers who are highly likely to churn
# unique list can be prepared using customer id


#*******************************************************************************************************************************************#

# <5> What would be the target segment for proactive retention campaign> Fallinf ARPU forecast is also a 
# concern and therefore, Telecom company would like to save their high revenue customers besides managing
# churn. Given a budget constraint of a contact list of 20% of the subscriber pool, which subscribers should
# prioritized if "revenue saves" is also a priority besides controlling churn. IN other words, controlling churn
# is the primary objective and revenue saves is the secondary objective.

####***** probabilty score and revenue value rate *****####

saveRev_Cust<- predict(mod_final, type = "response", newdata = trsf_df_tst)
trsf_df_tst$highRev_Cust<- predict(mod_final, type = "response", newdata = trsf_df_tst)

quantile(trsf_df_tst$highRev_Cust, prob= c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00))

saveHighRev_Cust<- ifelse(saveRev_Cust<0.20, "Low Probability",
                   ifelse(saveRev_Cust>=0.20 & saveRev_Cust<0.30, "Medium Probability", "High Probability"))
table(saveHighRev_Cust, trsf_df_tst$churn)
str(trsf_df_tst$totrev)
quantile(trsf_df_tst$totrev, prob= c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00))
rev_Rate<-  ifelse(trsf_df_tst$totrev< 681.558, "Low Revenue",
                   ifelse(trsf_df_tst$totrev>=681.558 & trsf_df_tst$totrev<1158.847, "Medium Revenue", "High Revenue"))
table(saveHighRev_Cust, rev_Rate)                   
# This result can help to select the level of customer need to be targeted

###*** Extract List ***###

summary(saveHighRev_Cust)
summary(rev_Rate)

# put it in test dataset
trsf_df_tst$probabilityRange<- ifelse(saveRev_Cust<0.20, "Low Probability",
                                      ifelse(saveRev_Cust>=0.20 & saveRev_Cust<0.30, "Medium Probability", "High Probability"))
trsf_df_tst$RevenueRange<- ifelse(trsf_df_tst$totrev< 681.558, "Low Revenue",
                                  ifelse(trsf_df_tst$totrev>=681.558 & trsf_df_tst$totrev<1158.847, "Medium Revenue", "High Revenue"))

Target_Cust_High_Rev<- trsf_df_tst[trsf_df_tst$probabilityRange== "High Probability" & trsf_df_tst$RevenueRange=="High Revenue", "Customer_ID"]
Target_Cust_High_Rev<- as.data.frame(Target_Cust_High_Rev)
nrow(Target_Cust_High_Rev) #1081

#pedicted customers who likely to be targeted with offers, services, etc
write.csv(Target_Cust_High_Rev,"predicted Customers who likely to retain for High Revenue.csv", row.names=F)

