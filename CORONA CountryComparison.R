Countryname1="Australia"
Countryname2="Pakistan"
Countryname3="France"
Countryname4="Italy"





latestConf1<-subset(latestConf,Country.Region==Countryname1)
latestConf2<-subset(latestConf,Country.Region==Countryname2)
latestConf3<-subset(latestConf,Country.Region==Countryname3)
latestConf4<-subset(latestConf,Country.Region==Countryname4)



latestConf_long1<- gather(latestConf1, Date, Count, `X1.22.20`:ncol(latestConf))
latestConf_long2<- gather(latestConf2, Date, Count, `X1.22.20`:ncol(latestConf))
latestConf_long3<- gather(latestConf3, Date, Count, `X1.22.20`:ncol(latestConf))
latestConf_long4<- gather(latestConf4, Date, Count, `X1.22.20`:ncol(latestConf))

###################################################


Date_latestConf_long1 <- latestConf_long1 %>% 
  group_by(Date) %>%
  summarise(nConfirmed=sum(Count)) %>% 
  arrange((nConfirmed))

Date_latestConf_long2 <- latestConf_long2 %>% 
  group_by(Date) %>%
  summarise(nConfirmed=sum(Count)) %>% 
  arrange((nConfirmed))


Date_latestConf_long3 <- latestConf_long3 %>% 
  group_by(Date) %>%
  summarise(nConfirmed=sum(Count)) %>% 
  arrange((nConfirmed))

Date_latestConf_long4 <- latestConf_long4 %>% 
  group_by(Date) %>%
  summarise(nConfirmed=sum(Count)) %>% 
  arrange((nConfirmed))
#########################################################




Y<-Date_latestConf_long1
for (i in 2:nrow(Y))
{
  Y[i,2] = Y[i,2] - Date_latestConf_long1[i-1,2]
  
}

Date_latestConf_long1<-Y

Y<-Date_latestConf_long2
for (i in 2:nrow(Y))
{
  Y[i,2] = Y[i,2] - Date_latestConf_long2[i-1,2]
  
}

Date_latestConf_long2<-Y

Y<-Date_latestConf_long3
for (i in 2:nrow(Y))
{
  Y[i,2] = Y[i,2] - Date_latestConf_long3[i-1,2]
  
}

Date_latestConf_long3<-Y

Y<-Date_latestConf_long4
for (i in 2:nrow(Y))
{
  Y[i,2] = Y[i,2] - Date_latestConf_long4[i-1,2]
  
}

Date_latestConf_long4<-Y


highchart() %>% 
  hc_xAxis(categories=Date_latestConf_long1$Date) %>% 
  hc_add_series(name=Countryname1, data=Date_latestConf_long1$nConfirmed) %>% 
  hc_add_series(name=Countryname2, data=Date_latestConf_long2$nConfirmed) %>% 
  hc_add_series(name=Countryname3, data=Date_latestConf_long3$nConfirmed) %>% 
  hc_add_series(name=Countryname4, data=Date_latestConf_long4$nConfirmed) %>% 
 
  hc_colors(c("red","green","black","blue")) %>% 
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Country Comparison cases for COVID-19",align="center")



