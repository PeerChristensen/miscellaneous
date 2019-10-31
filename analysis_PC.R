#install.packages(c("tidyverse", "lme4", "jtools"))
library(tidyverse)
library(lme4)
library(jtools)

# a nice theme for presenting figures
my_theme <- function() {
  theme_apa() +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_blank()) +
    theme(panel.border = element_blank()) +                       # facet border
    theme(strip.background = element_blank()) +                  # facet title background
    theme(strip.text.x = element_text(size = 20)) +
    theme(axis.title.x = element_text(size = 20,margin = margin(t = 25, r = 0, b = 0, l = 0))) +
    theme(axis.title.y = element_text(size = 20,margin = margin(t = 0, r = 25, b = 0, l = 0))) +
    theme(axis.text.y = element_text(size = 20)) +
    theme(axis.text.x =  element_text(size = 20)) +
    theme(panel.spacing = unit(2, "lines")) +
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
}

# Load data
df =read.table("RESULTS.txt",header=T,sep="\t",stringsAsFactors=T)

df = rename(df, "Condition" = "Music")
df$ANSWER = factor(df$ANSWER)

###################################################
# CONDITION

tab =table(df$Condition,df$ANSWER)
tab

prop.table(tab,1) # proportions, 1 means row-wise

# overall analysis with only Condition (Music variable) as predictor
# This is a so-called mixed-effects logistic regression model
# "Mixed" simply means that we take individual differences and variance related to the test items into account besides "fixed" ones, e.g. condition
# If I understand correctly, the Target variable corresponds to test item?
# if not, just remove "+ (1|Target)" from the analyses

# For simple analyses, t-tests are ok, but less precise.
# Also, you won't be able to control for other factors 
# or find interactions between individual factors

fit = glmer(ANSWER ~ Condition + (1|Name) + (1|Target),data = df, family = binomial)
summary(fit)

# plot 0
df %>%
  group_by(Condition, Name, ANSWER) %>%
  summarise(n = n()) %>% # number correct by condition
  mutate(freq = n / sum(n)) %>% # gets proportions
  complete(ANSWER, nesting(Name), fill = list(n = 0, freq = 0)) %>% # so each participant has a proportion for accurate and inaccurate answers
  group_by(Condition, ANSWER) %>%
  summarise(mean = mean(freq),
            se = sqrt(sd(freq))/sqrt(length(unique(Name)))) %>% #standard error
  filter(ANSWER == "1") %>% # Easier to interpret since we'll include errror bars
  ggplot(aes(x = Condition, y = mean)) +
  geom_bar(stat="identity", position = "dodge", fill = "steelblue") +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=.2,position=position_dodge(.9)) +
  labs(y = "Mean proportions correct") +
  coord_cartesian(ylim=c(0,1)) +
  my_theme() +
  geom_hline(yintercept = .5, size = 2, linetype = "dashed", colour = "gold")

ggsave("plot0.png", height = 10, width = 10)


###################################################
# CONTRASTIVENESS

# H1. Through tasks T1-T4, more contrastive tasks will show better results than less contrastive tasks.

tab1 =table(df$Contrastiveness,df$ANSWER)
tab1

prop.table(tab1,1) 

fitH1 = glmer(ANSWER ~ Contrastiveness + (1|Name) + (1|Target),data = df, family = binomial)
summary(fitH1)

# plot 1
df %>%
  group_by(Contrastiveness, Name, ANSWER) %>%
  summarise(n = n()) %>% # number correct by condition
  mutate(freq = n / sum(n)) %>% # gets proportions
  complete(ANSWER, nesting(Name), fill = list(n = 0, freq = 0)) %>% # so each participant has a proportion for accurate and inaccurate answers
  group_by(Contrastiveness, ANSWER) %>%
  summarise(mean = mean(freq),
            se = sqrt(sd(freq))/sqrt(length(unique(Name)))) %>% #standard error
  filter(ANSWER == "1") %>% # Easier to interpret since we'll include errror bars
  ggplot(aes(x = Contrastiveness, y = mean)) +
  geom_bar(stat="identity", position = "dodge", fill = "steelblue") +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=.2,position=position_dodge(.9)) +
  labs(y = "Mean proportions correct") +
  coord_cartesian(ylim=c(0,1)) +
  my_theme()

ggsave("plot1.png", height = 10, width = 10)

# binomial tests   
table(df$Contrastiveness,df$ANSWER)

# 1: are correct answers in less contrastive trials greater than chance? 
x = 69 # the sum of CORRECT answers
n = 168 # total (sum of CORRECT and INCORRECT trials)
binom.test(x,n,p=0.5,alternative=c("greater"), conf.level=0.95)

# 2: are correct answers in more contrastive trials greater than chance? 
x = 132 
n = 168 
binom.test(x,n,p=0.5,alternative=c("greater"), conf.level=0.95)

###################################################
# MODALITY

# H2 For the music tasks (T1-T2), the unimodal representamina can be expected to be matched to its 
# corresponding object with more frequency than cross-modal representamina.  

# I'm not sure I understand what you did for this analysis

modal = dataf[df$Condition=="Music",]

tab2 =table(modal$Modality,modal$ANSWER)
tab2

prop.table(tab2,1) 

fitH2 = glmer(ANSWER ~ Modality + (1|Name) + (1|Target), data = modal, family = binomial)
summary(fitH2)

# plot 2
modal %>%
  group_by(Modality, Name, ANSWER) %>%
  summarise(n = n()) %>% # number correct by condition
  mutate(freq = n / sum(n)) %>% # gets proportions
  complete(ANSWER, nesting(Name), fill = list(n = 0, freq = 0)) %>% # so each participant has a proportion for accurate and inaccurate answers
  group_by(Modality, ANSWER) %>%
  summarise(mean = mean(freq),
            se = sqrt(sd(freq))/sqrt(length(unique(Name)))) %>% #standard error
  filter(ANSWER == "1") %>%
  ggplot(aes(x = Modality, y = mean)) +
  geom_bar(stat="identity", position = "dodge", fill = "steelblue") +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=.2,position=position_dodge(.9)) +
  labs(y = "Mean proportions correct") +
  coord_cartesian(ylim=c(0,1)) +
  my_theme()

ggsave("plot2.png", height = 10, width = 10)

###################################################
# L1

# H3 For the music tasks (T1-T2), Swedish L1 speakers are expected to have better results than Chinese
# L1 speakers. 

tab3 =table(modal$L1,modal$ANSWER)
tab3

prop.table(tab3,1) 

fitH3 = glmer(ANSWER ~ L1 + (1|Name) + (1|Target), data = modal, family = binomial)
summary(fitH3)

# plot 3
modal %>%
  group_by(L1, Name, ANSWER) %>%
  summarise(n = n()) %>% # number correct by condition
  mutate(freq = n / sum(n)) %>% # gets proportions
  complete(ANSWER, nesting(Name), fill = list(n = 0, freq = 0)) %>% # so each participant has a proportion for accurate and inaccurate answers
  group_by(L1, ANSWER) %>%
  summarise(mean = mean(freq),
            se = sqrt(sd(freq))/sqrt(length(unique(Name)))) %>% #standard error
  filter(ANSWER == "1") %>%
  ggplot(aes(x = L1, y = mean)) +
  geom_bar(stat="identity", position = "dodge", fill = "steelblue") +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=.2,position=position_dodge(.9)) +
  labs(y = "Mean proportions correct") +
  coord_cartesian(ylim=c(0,1)) +
  my_theme()

ggsave("plot3.png", height = 10, width = 10)

###################################################
# IMAGE (not sure if you wanted this analysis)

tabIMG =table(modal$Image,modal$ANSWER)
tabIMG

prop.table(tabIMG,1) 

fitIMG = glmer(ANSWER ~ Image + (1|Name) + (1|Target), data = modal, family = binomial)
summary(fitIMG)

# plot IMG
modal %>%
  group_by(Image, Name, ANSWER) %>%
  summarise(n = n()) %>% # number correct by condition
  mutate(freq = n / sum(n)) %>% # gets proportions
  complete(ANSWER, nesting(Name), fill = list(n = 0, freq = 0)) %>% # so each participant has a proportion for accurate and inaccurate answers
  group_by(Image, ANSWER) %>%
  summarise(mean = mean(freq),
            se = sqrt(sd(freq))/sqrt(length(unique(Name)))) %>% #standard error
  filter(ANSWER == "1") %>%
  ggplot(aes(x = Image, y = mean)) +
  geom_bar(stat="identity", position = "dodge", fill = "steelblue") +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=.2,position=position_dodge(.9)) +
  labs(y = "Mean proportions correct") +
  coord_cartesian(ylim=c(0,1)) +
  my_theme()

ggsave("plotIMG.png", height = 10, width = 10)

###################################################
# FICTIVE WORDS

# H4A. For the fictive words tasks (T3), L1 Chinese speakers are expected to have similar results to 
# L1 Swedish speakers. 

fictive = df[df$Task=="T3",]

tab4a =table(fictive$L1,fictive$ANSWER)
tab4a

prop.table(tab4a,1) 

fit4a = glmer(ANSWER ~ L1 + (1|Name) + (1|Target), data = fictive, family = binomial)
summary(fit4a)

# plot 4a
fictive %>%
  group_by(L1, Name, ANSWER) %>%
  summarise(n = n()) %>% # number correct by condition
  mutate(freq = n / sum(n)) %>% # gets proportions
  complete(ANSWER, nesting(Name), fill = list(n = 0, freq = 0)) %>% # so each participant has a proportion for accurate and inaccurate answers
  group_by(L1, ANSWER) %>%
  summarise(mean = mean(freq),
            se = sqrt(sd(freq))/sqrt(length(unique(Name)))) %>% #standard error
  filter(ANSWER == "1") %>%
  ggplot(aes(x = L1, y = mean)) +
  geom_bar(stat="identity", position = "dodge", fill = "steelblue") +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=.2,position=position_dodge(.9)) +
  labs(y = "Mean proportions correct") +
  coord_cartesian(ylim=c(0,1)) +
  my_theme()

ggsave("plot4a.png", height = 10, width = 10)

###################################################
# IDEOPHONES

# H4B. For the ideophones tasks (T4), L1 Swedish speakers are expected to have better results than 
# L1 Chinese speakers.

ideophone = df[df$Task=="T4",]

tab4b =table(ideophone$L1,ideophone$ANSWER)
tab4b

prop.table(tab4b,1) 

fit4b = glmer(ANSWER ~ L1 + (1|Name) + (1|Target), data = ideophone, family = binomial)
summary(fit4b)

# plot 4b
ideophone %>%
  group_by(L1, Name, ANSWER) %>%
  summarise(n = n()) %>% # number correct by condition
  mutate(freq = n / sum(n)) %>% # gets proportions
  complete(ANSWER, nesting(Name), fill = list(n = 0, freq = 0)) %>% # so each participant has a proportion for accurate and inaccurate answers
  group_by(L1, ANSWER) %>%
  summarise(mean = mean(freq),
            se = sqrt(sd(freq))/sqrt(length(unique(Name)))) %>% #standard error
  filter(ANSWER == "1") %>%
  ggplot(aes(x = L1, y = mean)) +
  geom_bar(stat="identity", position = "dodge", fill = "steelblue") +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=.2,position=position_dodge(.9)) +
  labs(y = "Mean proportions correct") +
  coord_cartesian(ylim=c(0,1)) +
  my_theme()

ggsave("plot4b.png", height = 10, width = 10)

###################################################
# T3 VS. T4 PERFORMANCE

# H5. Participants are expected to perform better in the fictive words tasks (T3) than ideophones
# tasks (T4). 

ling = df[df$Condition=="Linguistic",]

tab5 =table(ling$Words,ling$ANSWER)
tab5

prop.table(tab5,1) 

fit5 = glmer(ANSWER ~ Words + (1|Name) + (1|Target), data = ling, family = binomial)
summary(fit5)

# plot 5
ling %>%
  group_by(Words, Name, ANSWER) %>%
  summarise(n = n()) %>% # number correct by condition
  mutate(freq = n / sum(n)) %>% # gets proportions
  complete(ANSWER, nesting(Name), fill = list(n = 0, freq = 0)) %>% # so each participant has a proportion for accurate and inaccurate answers
  group_by(Words, ANSWER) %>%
  summarise(mean = mean(freq),
            se = sqrt(sd(freq))/sqrt(length(unique(Name)))) %>% #standard error
  filter(ANSWER == "1") %>%
  ggplot(aes(x = Words, y = mean)) +
  geom_bar(stat="identity", position = "dodge", fill = "steelblue") +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=.2,position=position_dodge(.9)) +
  labs(y = "Mean proportions correct") +
  coord_cartesian(ylim=c(0,1)) +
  my_theme()

ggsave("plot5.png", height = 10, width = 10)

###################################################
# EXAMPLE OF AN ANALYSIS WITH THREE PREDICTORS

fitx = glmer(ANSWER ~ Condition + Contrastiveness + L1 + 
               (1|Name) + (1|Target), data = df, family = binomial)
summary(fitx)

###################################################
# EXAMPLE OF AN ANALYSIS TESTING FOR INTERACTION between Condition and L1
fity = glmer(ANSWER ~ Condition * L1 +
               (1|Name) + (1|Target), data = df, family = binomial)
summary(fity)
# no significant interaction
# i.e. the effect of condition did not vary with condition

###################################################
# EXAMPLE OF VISUALISING INTERACTIONS

# modified plot 0
df %>%
  group_by(Condition, L1, Name, ANSWER) %>%
  summarise(n = n()) %>% # number correct by condition
  mutate(freq = n / sum(n)) %>% # gets proportions
  complete(ANSWER, nesting(Name), fill = list(n = 0, freq = 0)) %>% # so each participant has a proportion for accurate and inaccurate answers
  group_by(Condition, L1, ANSWER) %>%
  summarise(mean = mean(freq),
            se = sqrt(sd(freq))/sqrt(length(unique(Name)))) %>% #standard error
  filter(ANSWER == "1") %>% # Easier to interpret since we'll include errror bars
  ggplot(aes(x = Condition, y = mean)) +
  facet_wrap(~L1) +
  geom_bar(stat="identity", position = "dodge", fill = "steelblue") +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=.2,position=position_dodge(.9)) +
  labs(y = "Mean proportions correct") +
  coord_cartesian(ylim=c(0,1)) +
  my_theme()

# OR THIS WAY...

df %>%
  group_by(Condition, L1, Name, ANSWER) %>%
  summarise(n = n()) %>% # number correct by condition
  mutate(freq = n / sum(n)) %>% # gets proportions
  complete(ANSWER, nesting(Name), fill = list(n = 0, freq = 0)) %>% # so each participant has a proportion for accurate and inaccurate answers
  group_by(Condition, L1, ANSWER) %>%
  summarise(mean = mean(freq),
            se = sqrt(sd(freq))/sqrt(length(unique(Name)))) %>% #standard error
  filter(ANSWER == "1") %>% # Easier to interpret since we'll include errror bars
  ggplot(aes(x = L1, y = mean, fill = Condition)) +
  geom_bar(stat="identity", position = "dodge") +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=.2,position=position_dodge(.9)) +
  labs(y = "Mean proportions correct") +
  coord_cartesian(ylim=c(0,1)) +
  scale_fill_manual(values = c("steelblue", "blue")) +
  my_theme()
