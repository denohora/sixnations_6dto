data = read.csv("sixnations_6dayto.csv")
# data is set up with one row per game  - Team is home team

## let's take out greater than 1 day difference in prep (4 games)
data = data[data$diff.Date.adv<2 & data$diff.Date.adv> -2,]

## Initial coarse look
# total Home wins, losses and draws
aggregate(GMD~Result, data, length) 

# average Home win rate
mean(data$Result=="won")

# Home win rate when Home have 6 day TOs? Higher than avg
with(data[data$diff.Date<7,], mean(Result=="won"))
# Home win rate when Opp have a 6 day TOs? Highest
with(data[data$diff.Date.Opp<7,], mean(Result=="won"))

# Average Home points diff
mean(data$Diff) # 

# Average Home points diff in when home team has 6 day TO? Below but close to avg
with(data[data$diff.Date<7,], mean(Diff))
# Average Home points diff  when Opp have a 6 day TOs? Higher than average
with(data[data$diff.Date.Opp<7,], mean(Diff))

# Consider differences in prep time 
# - 6/13 days for home vs 7/14 for opp (-1)
# - 7/14 days for home vs 7/14 for opp (0)
# - 7/14 days for home vs 6/13 for opp (+1)

# Home win rate and prep time differences? 
aggregate(Result=="won" ~ diff.Date.adv, data, mean)
# 1 Day less and 1 Day more both increase Home Win rate?

head(data[data$diff.Date.adv== -1,])
hist(data[data$diff.Date.adv== -1,]$diff.Date)

data$Year.WR.diff = data$Year.WR - data$Year.WR.Opp
hist(data$Year.WR.diff)
cor.test(data$Year.WR.diff, as.numeric(data$Result=="won"))

data$diff.Date.fac = as.factor(data$diff.Date.adv)
contrasts(data$diff.Date.fac) = matrix(c(1,0,0,0,0,1), ncol = 2) # set up no diff as reference level
colnames(contrasts(data$diff.Date.fac) ) = c("One Day Less", "One Day More")
contrasts(data$diff.Date.fac)


aggregate(Result~diff.Date.adv, data, length)
# Probability of Home Wins at levels of prep time difference
aggregate(Result=="won"~diff.Date.adv, data, mean)
aggregate(Year.WR.diff~diff.Date.adv, data, mean)

win.diffdate.glm = glm(Result=="won" ~ diff.Date.fac, data, 
                       family = binomial(link = "logit"))
summary(win.diffdate.glm)
writeLines("Odds ratios") 
round(exp(cbind(OR = coef(win.diffdate.glm), confint(win.diffdate.glm))),3) # odds ratios

# control for team
data$Team = as.factor(data$Team)

win.diffdate.glm = glm(Result=="won" ~ Team + diff.Date.fac, data, 
                       family = binomial(link = "logit"))
summary(win.diffdate.glm)
writeLines("Odds ratios") 
round(exp(cbind(OR = coef(win.diffdate.glm), confint(win.diffdate.glm))),3) # odds ratios

# control for opposition
data$Opposition = as.factor(data$Opposition)

win.diffdate.glm = glm(Result=="won" ~ Team + Opposition + diff.Date.fac, data, 
                       family = binomial(link = "logit"))
summary(win.diffdate.glm)
writeLines("Odds ratios") 
round(exp(cbind(OR = coef(win.diffdate.glm), confint(win.diffdate.glm))),3) # odds ratios

# what about 6 day turn arounds?

aggregate(Result~diff.Date.adv, data[data$diff.Date<10,], length)
# Probability of Home Wins at levels of prep time difference
aggregate(Result=="won"~diff.Date.adv, data[data$diff.Date<10,], mean)
aggregate(Year.WR.diff~diff.Date.adv, data[data$diff.Date<10,], mean)

# points diff?
points.diffdate.lm = lm(Diff ~ diff.Date.fac, data[data$diff.Date<10,])
summary(points.diffdate.lm)


## some plots - requires ggplot2
#install.packages("ggplot2")
library(ggplot2)
ggplot(data, aes(x = diff.Date.adv, y = as.numeric(Result=="won"))) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar") + 
  theme_bw() + coord_cartesian(ylim = c(.3, 1)) + 
  labs(title = "Prep Time effect on Winning Six Nations Matches", y = "Win Probablity", x = "Prep Time Advantage") +
  geom_hline(yintercept = .5, linetype = 2)


########
# Team specific look

# Total Home games in the set
aggregate(Diff~Team, data, length) 
# no of Home games with 6 Day TOs per team
aggregate(Diff~Team, data[data$diff.Date<7,], length) 

# Probability of Home Wins in all games 
aggregate(Result=="won"~Team, data, mean)

# Probability of Home Wins at levels of prep time difference
aggregate(Result=="won"~diff.Date.adv, data, mean)
aggregate(Result=="won"~diff.Date.adv + Team, data, mean)
aggregate(Result=="won"~diff.Date.adv, data, length)


ggplot(data, aes(x = diff.Date.adv, y = as.numeric(Result=="won"), group = Team, colour = Team)) +
            stat_summary(fun.y = "mean", geom = "point") +
            stat_summary(fun.y = "mean", geom = "line")

ggplot(data, aes(x = diff.Date.adv, y = as.numeric(Result=="won"), group = Opposition, colour = Opposition)) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line")



