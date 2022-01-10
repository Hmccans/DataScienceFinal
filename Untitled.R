library(dplyr)
library(ggplot2)
library(stargazer)
#i just fucking made the dataset myself, that's life! IPUMS is a pain in the ass, data took me 10 minutes

Tulsadf<-Tulsa_Bullshit %>% mutate(time=ifelse(Tulsa_Bullshit$Year >= 2018, 1, 0))
Tulsadf<-Tulsadf %>% mutate(treated=ifelse(City=="Tulsa",1,0 ))
Tulsadf<-Tulsadf %>% mutate(did=time*treated)

didreg = lm(New_OOS_POP ~ treated + time + did, data = Tulsadf)
summary(didreg)

didreg1 = lm(New_OOS_POP ~ treated*time, data = Tulsadf)
summary(didreg1)

ggplot(data=Tulsadf, aes(x=Year, group=City))+
      geom_line(aes(y=New_OOS_POP,City="Tulsa"))+
      geom_vline(xintercept=2018, linetype="dotted")+
      labs(y="New Out of State Population",x="Year", title="DiD in Population Growth of OK cities")

ggplot(data=Tulsadf, aes(x=Year, y=New_OOS_POP, group=City, color=City))+
  geom_line()+
  geom_vline(xintercept=2018, linetype="dotted")+
  labs(y="New Out of State Population",x="Year", title="DiD in Population Growth of OK cities")
stargazer(didreg)
ggsave

#Maybe a Fixed Effects? Why the fuck not