df=read.table("Regression.csv",header=T,sep=";",stringsAsFactors=F)

library(lme4)
library(lmerTest)
library(multcomp)

df$generation.rescaled=(df$Generation-1)/14 # rescales from 1 to 15 to 0 to 1. Helps coefficient interpretation.

outcomes=c("Alv_all","Lab_all") # All the outcome variables, add more here.

# 1. Make one list with the lmer-outputs.
lmer.list=lapply(outcomes,function(outcome){
	lmer(df[,outcome]~factor(Condition)*generation.rescaled+(generation.rescaled|Chain),df)
	})
names(lmer.list)=outcomes

# Write the list to a text file.
sink("lmeroutputs.txt");lapply(lmer.list,summary);sink()
lapply(lmer.list,summary)

# 2. Make a second list with the contrasts (this is based on the first list):
contrasts.list=lapply(lmer.list,function(x){
glht(x,linfct=rbind("big-small"=c(0,0,0,0,0,0,1,0,0,-1),
			        "pointy-round"=c(0,0,0,0,0,0,0,-1,1,0),
    				"big-none"=c(0,0,0,0,0,0,-1,0,0,0),
        			"small-none"=c(0,0,0,0,0,0,0,0,0,-1),
        			"round-none"=c(0,0,0,0,0,0,0,0,-1,0),
       		 		"pointy-none"=c(0,0,0,0,0,0,0,-1,0,0)))})

# Write the contrasts to a textfile:
sink("contrasts.txt");lapply(contrasts.list,summary);sink()
