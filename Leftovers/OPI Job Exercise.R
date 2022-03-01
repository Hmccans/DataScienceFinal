library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(lubridate)
library(readr)
library(zoo)
library(ggthemes)


#Reading in data, changing file_date and disp_date to Date format
df <- read_csv("~/Downloads/Justice Data Analyst - small claims cases 2016-2020.csv", 
                                    col_types = cols(file_date = col_date(format = "%m/%d/%Y"),
                                    disp_date = col_date(format = "%m/%d/%Y")))
#Filtering to just eviction cases
df2<-df %>% filter(iss_desc=="FORCIBLE ENTRY & DETAINER")
#Only working with Tulsa and Oklahoma, so we'll filter to just those as well
df3<-df2 %>% filter((court=="OKLAHOMA") | court=="TULSA")
#Grouping by month, so that i can create the counts by county court and month
df4<-df3%>% group_by(file_month=floor_date(file_date, "month"))
df5<- df4 %>% count(file_month, court)
df6<- df5 %>% rename(County=court)
#Plotting time
ggplot(data=df6, aes(x=file_month, y=n, group=County, color=County))+
        geom_line()+
        scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month")+
        labs(x="Month", y="Number of Evictions Filed", title="Eviction Cases Filed in Oklahoma And Tulsa County Courts")
        theme_economist_white()
        
     
##Data Exploration Time
        
#I kinda wanna look at file to disp time
dg<-df2 %>%filter(!is.na(disp_date))
dg1<-dg
dg1$date_diff<- difftime(dg1$disp_date, dg1$file_date, units="days")
dg12<-dg1 %>% group_by(court) %>% summarise(metm=mean(date_diff))
#Plotting to see who's most efficient
ggplot(data=dg12, aes(x=reorder(court, metm),y=metm))+
      geom_bar(stat="identity")+
      scale_x_discrete(guide = guide_axis(n.dodge=2))+
      theme_economist()+
      labs(x="County", y="Mean Time (Days)", title = "Average Wait Time by County: File Date to Dispositon")


#Redoing while removing those without close dates, may be outliers
dg2<-dg %>% filter(!is.na(close_date))
dg2$date_diff <- difftime(dg2$disp_date, dg2$file_date, units="days")
dg3<-dg2 %>% group_by(court) %>% summarise(metm=mean(date_diff))


ggplot(data=dg3, aes(x=reorder(court, metm),y=metm))+
  geom_bar(stat="identity")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme_economist()+
  labs(x="County", y="Mean Time (Days)", title = "Average Wait Time by County: File Date to Dispositon*")


#Comparing pre and post covid
dt<-subset(dg2, file_date<"2020-03-01")
dt2<-dt %>% group_by(court) %>% summarise(premetm=mean(date_diff))


#post covid
dp<-subset(dg2, file_date>"2020-03-01")
dp2<-dp %>% group_by(court) %>% summarise(postmetm=mean(date_diff))

dtp<-merge(x=dt2,y=dp2,by="court", all.x=TRUE)
dtp$covid_diff<-dtp$postmetm-dtp$premetm

ggplot(data=dtp, aes(x=reorder(court, covid_diff),y=covid_diff))+
  geom_bar(stat="identity")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme_economist()+
  labs(x="County", y="Mean Time (Days)", title = "Change in Average Disposition Wait Due to Covid-19")


