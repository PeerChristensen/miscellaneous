library(tidyverse)
library(lme4)
library(lmerTest)

setwd('/Users/peerchristensen/Desktop/RT_logs/')
files = list.files(pattern="csv")
df=tibble()

for (file in files){
  p = read_csv2(file)
  df = rbind(df,p)
}

df$RT = as.numeric(df$RT)
df$participant = factor(df$participant)


df = df %>% 
  filter(RT>=.2, RT<=.65) %>%
  mutate(RT = RT * 1000,
         correct = ifelse((sound == "high" & key == "k") |
                           (sound == "low" & key == "s"), "yes", "no"),
         congruent = ifelse(((visual=="high" | visual =="small")  & sound=="high") |
                              ((visual=="low" | visual =="big") & sound=="low"),T,F))

#accuracy
totalRows=nrow(df)
correctRows=nrow(df[df$correct=="yes",])
correctRows/totalRows

prop.table(table(df$correct))
#by condition
table(df$correct,df$condition)

#reaction time table
react = df %>% 
  filter(correct=="yes") %>%
  group_by(condition,congruent) %>%
  summarise(n=n(),mean = mean(RT), sd = sd(RT))
react 

#boxplot: RT ~ condition
df %>%
  ggplot(aes(x=condition,y=RT)) +
  geom_boxplot()

#boxplot: RT ~ congruency + condition
df %>%
  ggplot(aes(x=congruent,y=RT,fill=congruent)) +
  geom_boxplot() +
  #geom_violin() +
  #geom_point(aes(y=mean(RT)),shape=23, fill="white", color="black", size=5)
  facet_wrap(~condition)

##boxplot: RT ~ congruency + pitch + condition
df %>%
  ggplot(aes(x=congruent,y=RT,fill=sound)) +
  geom_boxplot() +
  #geom_point(aes(y=mean(RT)),shape=23, fill="white", color="darkred", size=5)
  #geom_violin() +
  facet_wrap(~condition)

#
ggplot(df, aes(x=RT, fill=congruent)) + geom_density(alpha=.3) +
  facet_wrap(~condition)

#test
fit =lmer(RT ~ condition+congruent+ (1|participant),data=df)
summary(fit)
