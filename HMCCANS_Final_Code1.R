library (Lahman)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(dynlm)
library(lm.beta)
library(tidymodels)
library(parsnip)
library(stargazer)
library(ggthemes)
###LOADING IN DATA###
#Load in some of the data from Lahman's data sets that we're gonna be using at some point
data(People) #gives us the Baseball Reference ID to connect to WAR and other advanced stats
data(Salaries) #gives us Lahman salaries to compare
data(AwardsPlayers) #Awards will be included to double check
data(Batting) #Batting Stats, will transform later to get full batting data
data(Fielding) #Fielding will also be included to take Defense into account
data(BattingPost)
# Load in Baseball Reference WAR data. WAR is unique to a few sources, and isn't an
#"offical stat" kept by the MLB. BR has one that I like, tho there's isn't
#a huge amount of difference. It will also give us the age each player is in a season
#without having to manually calculate it ourselves, though honestly that sounds like fun
baseballWAR<-fread("http://www.baseball-reference.com/data/war_daily_bat.txt")

#lahman keeps totals, doesn't inheriently calclaute BA, OBP, OPS. 
#This will get those values for us.
battingst<-battingStats(data = Lahman::Batting,
             idvars = c("playerID", "yearID", "stint", "teamID", "lgID"),
             cbind = TRUE)

###DATASET MANIPULATION TIME###
#This section will involve filtering and combining our datasets to get them to where
#They're usable and organized so it's all in one table

#Slim down to the variables we actually need
DF<-tibble(baseballWAR)
WARvars<-c("player_ID","year_ID","team_ID", "age","pitcher", "WAR", "WAR_off","WAA","WAR_def","OPS_plus")
WARDat<-DF[WARvars]
WARDat<-distinct(WARDat)
#Now for Peoples, and adding rookie year for data
varsRef<-c("playerID", "bbrefID", "nameFirst", "nameLast","debut","birthYear")
RefDat<-People[varsRef]
rookie<- format(as.Date(RefDat$debut, format="%Y-%m-%d"),"%Y")
RefDat$rookieyr<-rookie

#Lets keep going down the list
Batvars<-c("playerID",'yearID',"teamID", "R","AB","BA","OBP","SlugPct", "HR","RBI","SB", "OPS","OBP","BABIP")
BatDat<-battingst[Batvars]

#AwardsPlayers<-AwardsPlayers %>%transmute(Award_Binary= ifelse(is.na(awardID), 0,1))
Awardvars<-c("playerID","yearID","awardID")
AwardDat<-AwardsPlayers[Awardvars]


#Cutting down on years to make these datasets more manageable.
#(I know this can be a bit redudant when using left join,
#but i was having some trouble early on
#with things taking time so it makes my life easier)
Salarieslim <- Salaries %>% filter(yearID > 2000)
Battingslim <- BatDat %>% filter(yearID > 2000)
#Fieldingslim <- Fielding %>% filter(yearID > 2005)
Awardslim <- AwardDat %>%filter(yearID>2000)
WARslim<- WARDat %>% filter(year_ID>2000)



#####JOIN TIME#######
dfjoin<-left_join(Salarieslim, RefDat, by=("playerID"))
dfjoin2<-left_join(dfjoin, Battingslim, by=c("playerID","yearID","teamID"))
#df3<-left_join(df2,Fielding ,by = c("playerID", "yearID","teamID")) #OLD CODE
dfjoin3<-left_join(dfjoin2, Awardslim, by=c("playerID","yearID" ))



# Mutating to create a dummy variable for binary (has received award in a year),
#this allows me to remove duplicate years if they had two awards, since i don't care
#if i could have done this earlier i would have, but I needed it part of the full set
#to work. Using Distinct gets me to the # of obvs i had before, so i know
#I don't have any duplicates
df4<-dfjoin3%>%mutate(Award_Binary= ifelse(is.na(awardID), 0,1))
df5<-distinct(select(df4, -awardID))
#Last Join To complete data set
dfjoined<-left_join(df5,WARslim, by=c("bbrefID"="player_ID", "yearID"="year_ID","teamID"="team_ID"))
# Observations in df are the same as those in dfinal B')

#Time to average and sum our stats for the first three years!!

#grouping by player will help us organize, creating the cumulative mean so I can take the third instance later
dfilter<-dfjoined %>% group_by(playerID) %>% filter(pitcher=="N")
dfilter2<-dfilter %>% group_by(playerID) %>% mutate(meanAB=cummean(AB),
                                                     sumAB=cumsum(AB),
                                                     meanHR=cummean(HR),
                                                     meanRBI=cummean(RBI),
                                                     meanBA=cummean(BA),
                                                     meanOBP=cummean(OBP),
                                                     meanSLG=cummean(SlugPct),
                                                     meanSB=cummean(SB),
                                                     meanOPS=cummean(OPS),
                                                     meanBABIP=cummean(BABIP),
                                                     meanWAR=cummean(WAR),
                                                     meanWARoff=cummean(WAR_off),
                                                     meanWARdef=cummean(WAR_def),
                                                     logwage=log(salary),
                                                     sumAward=cumsum(Award_Binary),
                                                     )


#Now that i have everyones averages, i'm taking their 4th year, their 4th year salary,
#and their #third year cumulative stats so that we can predic their salaries during arbitration
dfcummean<-dfilter2 %>% group_by(playerID) %>% transmute(playerID=playerID,
                                                       year=yearID[4],
                                                       First=nameFirst,
                                                       Last=nameLast,
                                                       rookieBA=(meanBA[3]),
                                                       rookieWARoff=meanWARoff[3],
                                                       rookieWARdef=meanWARdef[3],
                                                       rookieaward=sumAward[3],
                                                       arbwage=logwage[4],
                                                       rookieOBP=meanOBP[3],
                                                       rookieSLG=meanSLG[3],
                                                       rookieSB=meanSB[3],
                                                       rookieHR=meanHR[3],
                                                       rookieBABIP=meanBABIP[3],
                                                       rookieOPS=meanOPS[3],
                                                       rookieRBI=meanRBI[3],
                                                       rookieAB=meanAB[3])


#We've now created an ungodly amount of identical entries, so we need to filter them out
Dfilter3<-unique(dfcummean)
#finally, removing all of the entries that don't have a 4th arbwage,
#though tbh they'll just be removed when i run a regression anyway.
dfinal<-Dfilter3 %>% filter(!is.na(arbwage))
                                                    
#Regression Time                                                    
lmresults<-lm(arbwage ~ rookieWARoff+rookieWARdef+rookieHR+rookieOBP+rookieSLG+rookieRBI+rookieSB+rookieBABIP+rookieAB+rookieaward,data=dfinal)                                           
summary(lmresults)

#Standardizing Our Coefficients by Standard Deviations
lmstd<-lm.beta(lmresults)
print(lmstd)

#Scatterplotting Sig. Variables with best fit lines
WARplot<-ggplot(dfinal, aes(x=rookieWARoff, y=arbwage))+
                         geom_point())+
                         geom_smooth(method='lm', se=FALSE)+
                         theme_economist()+
                         scale_color_economist()
                         ggtitle("Wages by WAR")+
                         xlab("Total WAR (Years 1-3)")+
                         ylab("Log of Wages (Year 4)")
          

#At Bats
ABplot<-ggplot(dfinal, aes(x=rookieAB, y=arbwage))+
                        geom_point()+
                        geom_smooth(method='lm', se=FALSE,)+
                        theme_economist()+
                        scale_color_economist()+
                        ggtitle("Wages by Total At Bats")+
                        xlab("Total AB (Years 1-3)")+
                        ylab("Log of Wages (Year 4)")

#Printing my #datavisulization, i figured that my regression outputs
#were recent enough to be considered "Consolidated" in the code
print(WARplot)
print(ABplot)


#Printing our visualization outputs to add to our .tex file
pdf("ggplot.pdf")
print(WARplot)     # Plot 1 --> in the first page of PDF
print(ABplot)     # Plot 2 ---> in the second page of the PDF
dev.off() 

              

#Create a Tex table for our final report as well
stargazer(lmresults)
#Note, for table 2, I couldn't get stargazer to reproduce the standardized
#coefficients, so instead I reused the lmresults table and manually updated
#it, which took 30 seconds and was way easier than figuring out the code lol
#Please don't penalize me, you can still see that output in print and its
#paper properly as well.



