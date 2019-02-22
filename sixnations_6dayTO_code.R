data = read.csv("sixnations_6dayto.csv")
# data is set up with one row per game  - Team is home team

## let's take out non-6day TOs (4 games)
data = data[data$diff.Date.adv<2 & data$diff.Date.adv> -2,]

## Initial coarse look
# total Home wins, losses and draws
aggregate(diff.Date.adv~Result, data, length)

# average Home win rate
mean(data$Result=="won")

# Home win rate in 6 day TOs? Higher than avg
with(data[data$diff.Date<7,], mean(Result=="won"))

# Average Home points diff
mean(data$Diff) # 

# Average Home points diff in when home team has 6 day TO? Close to avg
with(data[data$diff.Date<7,], mean(Diff))

# Consider differences in prep time 
# - 6 days for home vs 7 for opp (-1)
# - 7 days for home vs 7 for opp (0)
# - 7 days for home vs 7 for opp (+1)

# Home win rate and prep time differences? 
aggregate(Result=="won" ~ diff.Date.adv, data, mean)
# 1 Day less and 1 Day more both increase Home Win rate?

########
# Team specific look

# Total Home games in the set
aggregate(Diff~Team, data, length) 
# no of Home games with 6 Day TOs per team
aggregate(Diff~Team, data[data$diff.Date<7,], length) 

# Probability of Home Wins in all games 
aggregate(Result=="won"~Team, data, mean)

# Probability of Home Wins at levels of prep time difference
aggregate(Result=="won"~diff.Date.adv + Team, data, mean)

## some plots - requires ggplot2
install.packages("ggplot2")
library(ggplot2)
ggplot(data, aes(x = diff.Date.adv, y = as.numeric(Result=="won"))) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar") 

ggplot(data, aes(x = diff.Date.adv, y = as.numeric(Result=="won"), group = Team, colour = Team)) +
            stat_summary(fun.y = "mean", geom = "point") +
            stat_summary(fun.y = "mean", geom = "line")




