library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lubridate)


Justice_Data_Analyst_small_claims_cases_2016_2020_2 <- read_csv("~/Downloads/Justice Data Analyst - small claims cases 2016-2020 2.csv", 
                                                                +     col_types = cols(file_date = col_date(format = "%m/%d/%Y"), 
                                                                                       +         plaintiff = col_character(), disp_date = col_date(format = "%m/%d/%Y"), 
                                                                                       +         close_date = col_date(format = "%m/%d/%Y")))
df<-Justice_Data_Analyst_small_claims_cases_2016_2020_2

head(df)

df2<-df %>% filter(grepl('FORCIBLE', iss_desc))

dy<- df2%>% count(iss_desc, court)


ggplot(data=dy, aes(x=reorder(court, n),y=n))+
  geom_bar(stat="identity")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme_economist()+
  labs(x="County", y="Total Evictions", title = "Total Eviction Filings per County 2016-2020")

dfTulsa<- df2 %>% filter(court=="TULSA")


dfTulsaC3<-dfTulsa %>%
  mutate(plaintiff=replace(plaintiff, plaintiff=="HUNTINGTON HOLLOW APARTMENTS", "HUNTINGTON HOLLOWS APARTMENTS")) %>%
  mutate(plaintiff=replace(plaintiff, plaintiff=="LINCOLN GLEN APARTMENTS", "LINCOLN GLENS APARTMENTS")) %>%
  mutate(plaintiff=replace(plaintiff, plaintiff=="JA AVANDALE LLC", "JA AVONDALE LLC")) %>%
  mutate(plaintiff=replace(plaintiff, plaintiff=="JA AVONDALE", "JA AVONDALE LLC")) %>%
  mutate(plaintiff=replace(plaintiff, plaintiff=="CITY R. GROUP @ VISTA SHADOW LLC DBA", "CITY R GROUP @ VISTA SHADOW LLC DBA")) %>%
  mutate(plaintiff=replace(plaintiff, plaintiff=="CITY R GROUP @ VISTA SHADOW LLC D/B/A", "CITY R GROUP @ VISTA SHADOW LLC DBA")) %>%
  mutate(plaintiff=replace(plaintiff, plaintiff=="CITY R GROUP AT VISTA SHADOW LLC", "CITY R GROUP @ VISTA SHADOW LLC DBA")) %>%
  mutate(plaintiff=replace(plaintiff, plaintiff=="LONDON SQUARE APTS", "LONDON SQUARE APARTMENTS")) %>%
  mutate(plaintiff=replace(plaintiff, plaintiff=="THE GREENS AT BATTLE CREEK A LIMITED PARTNERS", "GREENS AT BATTLE CREEK A LIMITED PARTNERSHIP")) %>%
  mutate(plaintiff=replace(plaintiff, plaintiff=="BJ AND L RED RIVER LLC DBA", "BJ &AMP; L RED RIVER LLC DBA")) %>%
  mutate(plaintiff=replace(plaintiff, plaintiff=="STONERIDGE@36TH APARTMENTS", "STONERIDGE @ 36TH APARTMENTS")) %>%
  mutate(plaintiff=replace(plaintiff, plaintiff=="STONERIDGE @36TH APARTMENTS", "STONERIDGE @ 36TH APARTMENTS"))

dfTulsaC <- dfTulsaC3 %>%
  filter(!is.na(plaintiff)) %>%
  group_by(plaintiff) %>%
  count(iss_desc, plaintiff) %>%
  arrange(desc(n))

dfTulsaC2<-head(dfTulsaC, n=20)

  
  
dfTulsaC4<-head(dfTulsaC,n=12)
##### PLOTTING PLAINTIFS BY TOTAL EVICTIONS
ggplot(data=dfTulsaC4, aes(x=reorder(plaintiff, -n), y=n))+
  geom_bar(stat="identity")+
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 19,), guide = guide_axis(n.dodge=3)) +
  theme_economist()+
  labs(x="Apartment Complex", y="Total Evictions", title = "Largest Eviction Filers in Tulsa County")
###
dfGrant<-df2 %>%
  group_by(court) %>%
  count(grepl('JUDGEMENT', disp_case), court)

#compare evictions


dfGrant2<-dfGrant %>%
  group_by(court) %>%
  mutate(grant=n/sum(n))

dfGrant3<-dfGrant2 %>%
  filter(`grepl("JUDGEMENT", disp_case)`=="TRUE")


#ggplot here for county comparisons

ggplot(data=dfGrant3, aes(x=reorder(court, n), y=grant))+
  geom_bar(stat="identity")+
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 19,), guide = guide_axis(n.dodge=3)) +
  labs(x="Counties",y="Grant Rate", title= "Grant Rate by County")+
  theme_economist()



#Now lets do pre and pos



# Lets do based on COVID19

dfTaf<-subset(dfTulsa, file_date>="2020-03-01" & file_date <"2021-01-01")

dfTaf2<-dfTaf %>%
  count(grepl('JUDGEMENT', disp_case), court) %>%
  mutate(grant=n/sum(n)) %>%
  filter(`grepl("JUDGEMENT", disp_case)`=="TRUE")



dfTbf<-subset(dfTulsa, file_date>="2019-03-01" & file_date <"2020-01-01")

dfTbf2<-dfTbf %>%
  count(grepl('JUDGEMENT', disp_case), court) %>%
  mutate(grant=n/sum(n)) %>%
  filter(`grepl("JUDGEMENT", disp_case)`=="TRUE")


#by plaintiff

dfplangrant<-dfTulsaC3 %>%
  group_by(plaintiff) %>%
  count(grepl('JUDGEMENT', disp_case), court) %>%
  mutate(grant=n/sum(n)) %>%
  filter(`grepl("JUDGEMENT", disp_case)`=="TRUE")

dtp<-merge(x=dfTulsaC4,y=dfplangrant,by="plaintiff", all.x=TRUE)


#Just Tulsa
dfTulsanewguy<- df2 %>% filter(court=="TULSA") %>%
  count(grepl('JUDGEMENT', disp_case), court) %>%
  mutate(grant=n/sum(n)) %>%
  filter(`grepl("JUDGEMENT", disp_case)`=="TRUE")




# Tulsa Grant 
  
ggplot(data=dy, aes(x=reorder(court, n),y=n))+
  geom_bar(stat="identity")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme_economist()+
  labs(x="County", y="Total Evictions", title = "Total Eviction Filings per County 2016-2020")


dtp<-merge(x=dfTulsaC4,y=dfplangrant,by="plaintiff", all.x=TRUE)
#Landlord Grant Rate
ggplot(data=dtp, aes(x=reorder(plaintiff, -n.x), y=grant))+
  geom_bar(stat="identity")+
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 19,), guide = guide_axis(n.dodge=3)) +
  geom_hline(yintercept = .562, linetype = "dashed") +
  labs(x="Apartment Complexes",y="Grant Rate", title= "Grant Rate of Highest Filing Apartments")+
  theme_economist()

ggplot(data=)  
                  
dfCOVID<- dfTulsa %>% subset(file_date>="2019-01-01" & file_date<"2021-01-01")

dfCOVID2<-dfCOVID%>% mutate(file_month=floor_date(file_date, "month"))%>%
  mutate(file_year=as.character(file_year))%>%
  group_by(file_year)
dfCOVID3<- dfCOVID2 %>% count(file_month, file_year)

ggplot(dfCOVID3, aes(x = month(file_month, label=TRUE), y = n,color=file_year)) +geom_point()+
  theme_economist()+
  labs(x="Month",y="Eviction Filings",color="Year",title="Eviction Filings, 2019 vs 2020")+
  geom_vline(xintercept="March")


ggplot(dfCOVID3, aes(x = month(file_month, label=TRUE),y=n, group=factor(file_year)),color=factor(file_year)) + 
  geom_line()+
  labs(x="Month",colour="Year")


ggplot(dataset, aes(x = month, y = value, color = year)) + geom_line()



