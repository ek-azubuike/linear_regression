library(tidyverse)
library(dslabs)
library(HistData)

set.seed(1989, sample.kind = "Rounding")

data("GaltonFamilies")

female.heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

female.heights

mu.x <- mean(female.heights$mother)
sd.x <- sd(female.heights$mother)
mu.y <- mean(female.heights$daughter)
sd.y <-  sd(female.heights$daughter)

r <- cor(female.heights$mother, female.heights$daughter)

# Calculate the slope and intercept of the regression line predicting 
# daughters' heights given mothers' heights. Given an increase in mother's 
# height by 1 inch, how many inches is the daughter's height expected to 
# change?
m <- r * (sd.y / sd.x)
b <- mu.y - (m * mu.x)

# What percent of the variability in daughter heights is explained by the 
# mother's height?
(r^2) * 100

# height of daughter given mother's height = 60"
pred.y <- (m * 60) + b

# Assessment: Least Squares Estimates

rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

b.1 <- seq(0, 1, length.out = nrow(galton_heights))
results <- data.frame(b.1 = b.1,
                      rss = sapply(b.1, rss, beta0 = 36))
results %>% 
  ggplot(aes(x = b.1, y = rss)) +
  geom_line() +
  geom_line(aes(x = b.1, y = rss), col = 2)

library(Lahman)

per.game.stats <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(HR.per.game = HR / G,
         R.per.game = R / G,
         SB.per.game = SB / G,
         BB.per.game = BB / G,
         AB.per.game = AB / G,
         E.per.game = E / G,
         W.per.game = W / G,
         X3B.per.game = X3B / G,
         X2B.per.game = X2B / G,
         Singles.per.game = (H-HR-X2B-X3B)/G)

fit <- lm(formula = R.per.game ~ HR.per.game + BB.per.game,
          data = per.game.stats)
summary(fit)

set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# Fit a linear regression model predicting the mothers' heights using 
# daughters' heights. 
fit <- lm(mother ~ daughter, data = female_heights)
summary(fit)

# Predict mothers' heights using the model from Question 7 and the 
# predict() function.

female_heights %>% 
  mutate(y.hat = predict(fit))

# generate two tables: one for 2002 and another for the average of 
# 1999-2001 seasons.
# define per plate appearance statistics, keeping only players with more than 
# 100 plate appearances
library(Lahman)

bat.02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

#  compute a similar table but with rates computed over 1999-2001
bat.99_01 <- Batting %>% 
  filter(yearID %in% 1999:2001) %>% 
  mutate(pa = AB + BB,
         singles = (H - X2B - X3B - HR) / pa,
         bb = BB / pa) %>% 
  filter(pa >= 100) %>% 
  select(playerID, singles, bb)

# How many players had a single rate mean_singles of greater than 0.2 per 
# plate appearance over 1999-2001?
bat.avg <- bat.99_01 %>% 
  group_by(playerID) %>% 
  summarise(mean_singles = mean(singles),
            mean_bb = mean(bb))

bat.avg

bat.avg %>% 
  filter(mean_singles> 0.2)

# How many players had a BB rate mean_bb of greater than 0.2 per plate 
# appearance over 1999-2001?
bat.avg %>% 
  filter(mean_bb > 0.2)

# Use inner_join() to combine the bat_02 table with the table of 1999-2001 rate 
# averages you created in the previous question. 

bat.all <- inner_join(bat.02, 
                      bat.avg, 
                      by = join_by(playerID == playerID))

# What is the correlation between 2002 singles rates and 1999-2001 average 
# singles rates?
cor(bat.all$singles, bat.all$mean_singles)

cor(bat.all$bb, bat.all$mean_bb)

# Make scatterplots of mean_singles versus singles and mean_bb versus bb.
# Are either of these distributions bivariate normal?
bat.all %>% 
  ggplot(aes(x = singles,
             y = mean_singles)) +
  geom_point()

bat.all %>% 
  ggplot(aes(x = bb,
             y = mean_bb)) +
  geom_point()

# Fit a linear model to predict 2002 singles given 1999-2001 mean_singles.
singles.model <- lm(singles ~ mean_singles, data = bat.all)
coef(singles.model)

bb.model <- lm(bb ~ mean_bb, data = bat.all)
coef(bb.model)

# You want to know whether the relationship between home runs and runs per 
# game varies by baseball league.
library(broom)
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R)

dat %>% 
  group_by(lgID) %>% 
  summarize(tidy(lm(R ~ HR, data = across(), conf.int = T))) %>% 
  filter(term == "HR") 

dat %>% 
  group_by(lgID) %>% 
  reframe(tidy(lm(R ~ HR, data = across()), conf.int = T)) %>% 
  filter(term == "HR")

# Galton Families
library(tidyverse)
library(HistData)
data("GaltonFamilies")

set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <-GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

head(galton)

galton %>% 
  group_by(pair) %>% 
  count()

galton %>% 
  group_by(pair) %>% 
  summarise(cor(childHeight, parentHeight))

pair.models <- galton %>% 
  group_by(pair) %>% 
  reframe(tidy(lm(childHeight ~ parentHeight), 
                 conf.int = T))

pair.models %>% 
  mutate(diff = conf.high - conf.low) %>% 
  select(pair, diff)

# which team has more predicted runs?
bb <- 0.371
singles <- 0.519
doubles <- 0.771
triples <- 1.24
hr <- 1.44

a <- 2*bb + 4*singles + 1*doubles + 0*triples + 1*hr
b <- 1*bb + 6*singles + 2*doubles + 1*triples + 0*hr

# Fit a multivariate linear regression model to obtain the effects of 
# BB and HR on Runs (R) in 1971. Use the tidy() function in the broom package 
# to obtain the results in a data frame.

Teams %>% 
  filter(yearID == 1971) %>% 
  reframe(tidy(lm(R ~ BB + HR, data = .), conf.int = T))

# Repeat the above exercise to find the effects of BB and HR on runs (R) for 
# every year from 1961 to 2018 using summarize() and the broom package. 
bb.hr.model <- Teams %>% 
  filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>% 
  reframe(tidy(lm(R ~ BB + HR), conf.int = T))

bb.hr.model %>% 
  filter(term == "BB") %>% 
  ggplot(aes(x = yearID, y = estimate)) +
  geom_point() +
  geom_smooth(method = "lm")

bb.hr.model %>% 
  filter(term == "BB") %>% 
  reframe(tidy(lm(estimate ~ yearID), conf.int = T))

library(tidyverse)
library(broom)
library(Lahman)

Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G,
         r.per.g = R / G,
         hr.per.g = HR / G)
# Use runs (R) per game to predict average attendance.
# For every 1 run scored per game, average attendance increases by how much?
Teams_small %>% 
  reframe(tidy(lm(avg_attendance ~ r.per.g), conf.int = T))

# Use home runs (HR) per game to predict average attendance.
# For every 1 home run hit per game, average attendance increases by how much?
Teams_small %>% 
  reframe(tidy(lm(avg_attendance ~ hr.per.g), conf.int = T))

# Use number of wins to predict average attendance; do not normalize for 
# number of games.
# For every game won in a season, how much does average attendance increase?
fit <- Teams_small %>% 
  reframe(tidy(lm(avg_attendance ~ W), cont.int = T))

#Suppose a team won zero games in a season.
# Predict the average attendance.
intercept <- fit$estimate[1]
slope <- fit$estimate[2]

# Use year to predict average attendance.
# How much does average attendance increase each year?
Teams_small %>% 
  reframe(tidy(lm(avg_attendance ~ yearID), conf.int = T))

# What is the correlation coefficient for runs per game and wins?
cor(Teams_small$r.per.g, Teams_small$W)

# What is the correlation coefficient for home runs per game and wins?
cor(Teams_small$hr.per.g, Teams_small$W)

# Stratify Teams_small by wins: divide number of wins by 10 and then 
# round to the nearest integer. Filter to keep only strata 5 through 10. 
# (The other strata have fewer than 20 data points, too few for our analyses).
Teams_small %>% 
  mutate(wins.strata = round(W / 10)) %>% 
  filter(wins.strata >= 5 & wins.strata <= 10) %>% 
  group_by(wins.strata) %>% 
  count

# Calculate the slope of the regression line predicting average attendance given runs per game for each of the win strata.
# Which win stratum has the largest regression line slope?
Teams_small %>% 
  mutate(wins.strata = round(W / 10)) %>% 
  filter(wins.strata >= 5 & wins.strata <= 10) %>% 
  group_by(wins.strata) %>% 
  reframe(tidy(lm(avg_attendance ~ r.per.g), conf.int = T)) %>% 
  filter(term == "r.per.g") %>% 
  arrange(desc(estimate))

# Calculate the slope of the regression line predicting average attendance given HR per game for each of the win strata.
# Which win stratum has the largest regression line slope?
Teams_small %>% 
  mutate(wins.strata = round(W / 10)) %>% 
  filter(wins.strata >= 5 & wins.strata <= 10) %>% 
  group_by(wins.strata) %>% 
  reframe(tidy(lm(avg_attendance ~ hr.per.g), conf.int = T)) %>% 
  filter(term == "hr.per.g") %>% 
  arrange(desc(estimate))

# Fit a multivariate regression determining the effects of runs per game, 
# home runs per game, wins, and year on average attendance. Use the original 
# Teams_small wins column, not the win strata from question 3.
fit <- lm(avg_attendance ~ r.per.g + hr.per.g + W + yearID,
          data = Teams_small)
new.dat <- data.frame(r.per.g = 5.0,
                      hr.per.g = 1.2,
                      W = 80,
                      yearID = 2002)
predict(fit, new.dat, se.fit = T)
new.dat <- data.frame(r.per.g = 5.0,
                      hr.per.g = 1.2,
                      W = 80,
                      yearID = 1960)
predict(fit, new.dat, se.fit = T)

per.game.stats <- Teams %>% 
  filter(yearID == 2002) %>% 
  mutate(hr.per.g = HR / G,
         r.per.g = R / G,
         SB.per.game = SB / G,
         BB.per.game = BB / G,
         AB.per.game = AB / G,
         E.per.game = E / G,
         W.per.game = W / G,
         X3B.per.game = X3B / G,
         X2B.per.game = X2B / G,
         Singles.per.game = (H-HR-X2B-X3B)/G,
         avg_attendance = attendance / G)

y.hat <- predict(fit, per.game.stats, se.fit = T)
cor(y.hat$fit, per.game.stats$avg_attendance)

# Confounding
library(dslabs)
data("research_funding_rates")
research_funding_rates

# Construct a two-by-two table of gender (men/women) by award status 
# (awarded/not) using the total numbers across all disciplines.
totals <- research_funding_rates %>% 
  mutate(yes_men = awards_men,
         no_men = applications_men - awards_men,
         yes_women = awards_women,
         no_women = applications_women - awards_women) %>% 
  select(yes_men, no_men, yes_women, no_women) %>% 
  summarise_all(sum) #%>% 

two.by.two <- with(totals, data.frame(men = c(yes_men, no_men),
                        women = c(yes_women, no_women),
                        row.names = c("yes", "no")))

two.by.two[1,1] / sum(two.by.two[,1])
two.by.two[1,2] / sum(two.by.two[,2])

tidy(chisq.test(two.by.two))

dat <- 
  research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  pivot_longer(-discipline) %>%
  separate(name, c("type", "gender")) %>%
  pivot_wider(names_from = type, values_from = value) #%>%
  filter(gender != "total")

dat

dat %>% 
  ggplot(aes(x = discipline,
             y = success,
             color = gender,
             size = applications)) +
  geom_point()
