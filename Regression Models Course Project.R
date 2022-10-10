library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(modelr)

?mtcars
head(mtcars)
mpg_by_transmission <- mtcars %>% 
  group_by(am) %>% #0 = auto, 1 = manual
  summarise(
    mean_mpg = mean(mpg, na.rm = TRUE),
    sd_mpg = sd(mpg, na.rm = TRUE),
    var_mpg = var(mpg, na.rm = TRUE)
  )
mpg_by_transmission$mean_mpg[2] - 
  mpg_by_transmission$mean_mpg[1] 
  #On average, cars with manual transmission get 7.245 
  #more miles per gallon than cars with automatic transmission

mtcars$am[mtcars$am == 0] <- "Automatic"
mtcars$am[mtcars$am == 1] <- "Manual"

mtcars %>%
  ggplot(aes(mpg)) +
  geom_histogram() +
  facet_wrap(~am) 

mtcars %>%
  ggplot(aes(y = mpg)) + 
  geom_boxplot() + 
  facet_wrap(~am)

test <- t.test(mtcars$mpg[mtcars$am == "Automatic"], mtcars$mpg[mtcars$am == "Manual"], paired = FALSE, alternative = "two.sided", conf.level = .95)
test$p.value

mtcars
y <- lm(data = mtcars, mpg ~ am + as.factor(cyl) + disp + hp + drat + wt + qsec + vs + gear + carb)
y$coefficients
confint(y)


ggplot(y, aes(y = residuals)) +

  


a <- mtcars %>%
  ggplot(aes(y = mpg)) +
  geom_boxplot() +
  facet_wrap(~cyl) +
  labs(
    title = "Cylinder"
  )

b <- mtcars %>%
  ggplot(aes(y = mpg)) +
  geom_boxplot() +
  facet_wrap(~ vs) +
  labs(
    title = "VS"
  )

c <- mtcars %>%
  ggplot(aes(y = mpg)) +
  geom_boxplot() +
  facet_wrap(~gear) +
  labs(
    title = "Gear"
  )

d <- mtcars %>%
  ggplot(aes(y = mpg)) +
  geom_boxplot() +
  facet_wrap(~carb) +
  labs(
    title = "Carb"
  )
ggarrange(a,b,c,d, ncol = 2, nrow = 2)

e <- mtcars %>%
  ggplot(aes(disp, mpg)) + 
  geom_smooth() +
  labs(title = "Disp")
f <- mtcars %>%
  ggplot(aes(hp, mpg)) + 
  geom_smooth() +
  labs(title = "HP")
g <- mtcars %>%
  ggplot(aes(drat, mpg)) + 
  geom_smooth() +
  labs(title = "Drat")
h <- mtcars %>%
  ggplot(aes(wt, mpg)) + 
  geom_smooth() +
  labs(title = "Weight")
i <- mtcars %>%
  ggplot(aes(qsec, mpg)) + 
  geom_smooth() +
  labs(title = "Qsec")
ggarrange(e, f, g, h, i, ncol = 3, nrow = 2)

cor(mtcars$mpg, mtcars$disp)
cor(mtcars$mpg, mtcars$hp)
cor(mtcars$mpg, mtcars$drat)
cor(mtcars$mpg, mtcars$wt)
cor(mtcars$mpg, mtcars$qsec)

mtcars <- mtcars %>%
  add_residuals(y)
mtcars$id <- 1:32

ggplot(mtcars, aes(id, resid)) + 
  geom_ref_line(h = 0) +
  geom_point() +
  labs(
    title = "Residuals vs Predicted"
  )



    