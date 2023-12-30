#Trang Tran, ALY6010, Module 6, Practice 6
cat("\014")  # clears console
rm(list = ls())  # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(pacman)
p_load(tidyverse)
library(corrplot)
library(MASS)

#Load dataset
birthwt
summary(birthwt)

p_load(skimr)
skim(birthwt)

#correlation analysis
cor(birthwt)

#part 1
#regression model
reg <- lm(bwt ~ lwt + race, data = birthwt)
summary(reg)

#run 1st regression line
plot(bwt~lwt, birthwt, xlab = "Maternal weight (lbs)", ylab = "Birth weight (g)")
abline(reg)

#rerun the regression line with dummy variables
birthwt$race <- as.factor(birthwt$race)

reg <- lm(bwt ~ lwt + race, data = birthwt)
summary(reg)

reg.coef <- coef(reg)
reg.coef

plot(bwt~lwt, birthwt, pch=16, col = as.numeric(race)+1,
xlab = "Maternal weight (lbs)", ylab = "Birth weight (g)")
abline(a=reg.coef[1], b=reg.coef[2], col=2)
abline(a=reg.coef[1] + reg.coef[3], b=reg.coef[2], col=3)
abline(a=reg.coef[1] + reg.coef[4], b=reg.coef[2], col=4)
legend("bottomright", levels(birthwt$race), legend = c("White", "Black", "Other"),
       pch=16, lwd=1, col=2:4)

#part 2
#create scatterplot with separate regression lines for race variable
ggplot(birthwt, aes(x = lwt, y = bwt, color = factor(race))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Maternal weight (lbs)", y = "Birth weight (g)", color = "Race") +
  scale_color_discrete(labels = c("White", "Black", "Other"))


