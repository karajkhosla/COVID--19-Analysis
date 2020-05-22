library(dplyr)
library(highcharter)
library(ggplot2)
library(readr)
library(tidyr)


###IMPORTING THE DATA

urlConfirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

urlDeaths<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

urlRecoveries<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

latestConf<-read.csv(url(urlConfirmed))

latestDeaths<-read.csv(url(urlDeaths))

latestRecoveries<-read.csv(url(urlRecoveries))


head(latestConf)

#### LIST OF ALL COUNTRIES AFFECTED

Countries<-unique(latestConf["Country.Region"])

Countryname="China"


###SELECT A COUNTRY  

#latestConf<-subset(latestConf,Country.Region==Countryname)
#latestDeaths<-subset(latestDeaths,Country.Region==Countryname)
#latestRecoveries<-subset(latestRecoveries,Country.Region==Countryname)


###CLEANING THE DATASET

latestConf_long<- gather(latestConf, Date, Count, `X1.22.20`:ncol(latestConf))

latestDeaths_long<- gather(latestDeaths, Date, Count, `X1.22.20`:ncol(latestDeaths) )

latestRecoveries_long<- gather(latestRecoveries, Date, Count, `X1.22.20`:ncol(latestRecoveries) )

Date_latestConf_long <- latestConf_long %>% 
  group_by(Date) %>%
  summarise(nConfirmed=sum(Count)) %>% 
  arrange((nConfirmed))


Date_latestDeaths_long_date <- latestDeaths_long %>% 
  group_by(Date) %>%
  summarise(nDeaths=sum(Count)) %>% 
  arrange((nDeaths))


Date_latestRecoveries_long_date <- latestRecoveries_long %>% 
  group_by(Date) %>%
  summarise(nRecoveries=sum(Count)) %>% 
  arrange((nRecoveries))


Y<-Date_latestConf_long
for (i in 2:nrow(Y))
{
  Y[i,2] = Y[i,2] - Date_latestConf_long[i-1,2]
  
}

Date_latestConf_long<-Y
#####
Y<-Date_latestDeaths_long_date
for (i in 2:nrow(Y))
{
  Y[i,2] = Y[i,2] - Date_latestDeaths_long_date[i-1,2]
  
}

Date_latestDeaths_long_date<-Y

#######

Y<-Date_latestRecoveries_long_date
for (i in 2:nrow(Y))
{
  Y[i,2] = Y[i,2] - Date_latestRecoveries_long_date[i-1,2]
  
}

Date_latestRecoveries_long_date<-Y

head(Date_latestConf_long)




hchart(Date_latestConf_long, "spline", hcaes(x = Date,y = nConfirmed), name="Confirmed cases:",color="black") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Increase in number of COVID-19 cases over time",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 





hchart(Date_latestDeaths_long_date, "spline", hcaes(x = Date,y = nDeaths), name="Confirmed cases:",color="red") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Increase in number of COVID-19 deaths over time",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 





hchart(Date_latestRecoveries_long_date, "spline", hcaes(x = Date,y = nRecoveries), name="Confirmed cases:",color="Green") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Increase in number of COVID-19 Recoveries over time",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 




highchart() %>% 
  hc_xAxis(categories=Date_latestConf_long$Date) %>% 
  hc_add_series(name="Deaths", data=Date_latestDeaths_long_date$nDeaths) %>% 
  hc_add_series(name="Recoveries",data=Date_latestRecoveries_long_date$nRecoveries) %>% 
  hc_add_series(name="Confirmed Cases", data=Date_latestConf_long$nConfirmed) %>% 
  hc_colors(c("red","green","black")) %>% 
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Analysis of count of deaths,recoveries and cases for COVID-19",align="center")

#### COUNTRY with Most Confirmed ,Death,Recoveries from ( X1.22.20 to Now )

X<-latestDeaths
select1<-subset(X,X[5]== max(X[5]))
dat<-as.Date("2020-01-22")
dat1<-data.frame(dat)

for (i in 6:ncol(X)){
  dat2<-dat+(i-5)  
  dat2<-data.frame(dat2)           
  dat1<-cbind(dat1,dat2)
  select2<-subset(X,X[i]== max(X[i]))
  select1<-rbind(select1,select2)
}
dat<-data.frame(t(dat1))
select<-select1[2]
MostDeaths_byDate<-cbind(dat,select)
colnames(MostDeaths_byDate) <- c("Date", "Country")


#################
X<-latestConf
select1<-subset(X,X[5]== max(X[5]))
dat<-as.Date("2020-01-22")
dat1<-data.frame(dat)

for (i in 6:ncol(X)){
  dat2<-dat+(i-5)  
  dat2<-data.frame(dat2)           
  dat1<-cbind(dat1,dat2)
  select2<-subset(X,X[i]== max(X[i]))
  select1<-rbind(select1,select2)
}
dat<-data.frame(t(dat1))
select<-select1[2]
MostConfirmed_byDate<-cbind(dat,select)
colnames(MostConfirmed_byDate) <- c("Date", "Country")

#################
X<-latestRecoveries
select1<-subset(X,X[5]== max(X[5]))
dat<-as.Date("2020-01-22")
dat1<-data.frame(dat)

for (i in 6:ncol(X)){
  dat2<-dat+(i-5)  
  dat2<-data.frame(dat2)           
  dat1<-cbind(dat1,dat2)
  select2<-subset(X,X[i]== max(X[i]))
  select1<-rbind(select1,select2)
}
dat<-data.frame(t(dat1))
select<-select1[2]
MostRecoveries_byDate<-cbind(dat,select)
colnames(MostRecoveries_byDate) <- c("Date", "Country")









