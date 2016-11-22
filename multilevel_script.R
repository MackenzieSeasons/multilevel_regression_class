library(tidyverse)

indiv.data <- read_csv("bh1996.csv")

#now need to create the group level data

indiv.grouped <- group_by(indiv.data, GRP)
group.data <- indiv.grouped %>% summarise(HRS.GRP=mean(HRS, na.rm=TRUE), N.GRP=n())
#HRS.GRP equals the mean of HRS (remove missing values, na.rm=TRUE - if it's missing, remove it)
#this results in group level data, there are 99 groups

#now we want to bring the 2 groups together to get the multilevel data
multilevel.data <- full_join(indiv.data, group.data, by="GRP")
#HRS.GRP is the mean hours for that group

#now that we've made the data, we analyze

library(nlme)

Intercept.Model.Ignoring.Groups <- gls(WBEING ~ 1, data = multilevel.data)
#this is a better method than the one in the handout

#now we look at it with the groups
Intercept.Model.With.Groups <- lme(WBEING ~ 1, random = ~1|GRP, data = multilevel.data)

#Compare these two
Intercept.Model.Ignoring.Groups <- gls(WBEING ~ 1, data = multilevel.data)
Intercept.Model.With.Groups     <- lme(WBEING ~ 1, random = ~1|GRP, data = multilevel.data)
#first line is how well does the mean of well being predict everyone's score - how well does the mean of the DV column account for variability (not much)
# the second line says take every group into account - give each group its own mean - each group can have its own mean
anova(Intercept.Model.Ignoring.Groups,Intercept.Model.With.Groups)
#are these two lines different? they are! 
#there's a sig difference when you take group into account
#group means help you account for variation in well being scores

#How much do they matter in the DV?
VarCorr(Intercept.Model.With.Groups)
# 4% of the variability in well being scores is accounted for by group means
# effect / (effect + error)
.03590077/(.03590077 + 0.7849727)
#not a ton of help due to groups, but they do. 4% of the variability in WB scores is due to the group that you're in

#how do we explain that variability?
Model.1<-lme(WBEING ~ HRS + HRS.GRP, random=~1|GRP, data=multilevel.data)

#fixed effects
summary(Model.1)
#for every hour you work as an individual your WB score will go down by .04
# for every hour your work group works, your WB score will go down by .17 (HRS + HRS.GRP)
# group average has a bigger negative impact on your well being

#random effects
VarCorr(Model.1)
# variance in the intercept is .0135 (the mean intercept was 4.74)
# whats the avg of the spread in each group? .78


