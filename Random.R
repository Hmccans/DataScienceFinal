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