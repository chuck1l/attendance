library(tidyverse)
library(broom)
library(Lahman)
options(digits = 3)

Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, avg_R = R/G, avg_HR = HR/G)

## Question 1: attendance based on R, HR, Wins and time

lm(avg_attendance ~ avg_R, data = Teams_small)
lm(avg_attendance ~ avg_HR, data = Teams_small)
lm(avg_attendance ~ W, data = Teams_small)
lm(avg_attendance ~ yearID, data = Teams_small)

## Correlation between HR or R, and Wins

Teams_small %>% summarize(r = cor(W, avg_R)) %>% pull(r)
Teams_small %>% summarize(r = cor(W, avg_HR)) %>% pull(r)

## Stratifying Teams_small

dat <- Teams_small %>%
  mutate(W_strata = round(W/10, 0)) %>%
  filter(W_strata >= 5 & W_strata <= 10)

dat
sum(dat$W_strata == 8)

## slope of the regression line predicting average attendance given runs per game for each strata

dat %>% 
  group_by(W_strata) %>%
  summarize(slope = cor(avg_R, avg_attendance)*sd(avg_attendance)/sd(avg_R))

dat %>% 
  group_by(W_strata) %>%
  summarize(slope = cor(avg_HR, avg_attendance)*sd(avg_attendance)/sd(avg_HR))

## Multivariate regression determining the effects of runs/game, HR/game, W and year on the average attendance 

fit <- lm(avg_attendance ~ avg_R + avg_HR + W + yearID, data = Teams_small)
fit

## Average attendance in 2002 and 1960 with 5 R/G, 1.2 HR/G, 80 wins in the season

predict(fit, data.frame(avg_R = 5, avg_HR = 1.2, W = 80, yearID = 2002))

predict(fit, data.frame(avg_R = 5, avg_HR = 1.2, W = 80, yearID = 1960))

## Question 6

results <- Teams %>% filter(yearID == 2002) %>%
  mutate(avg_attendance = attendance/G, avg_R = R/G, avg_HR = HR/G, predict_attend = predict(fit, results))
 
results
results %>% summarize(r = cor(avg_attendance, predict_attend)) %>% pull(r)





