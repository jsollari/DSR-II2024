#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   4.3.1
#criado:     05.07.2023
#modificado: 23.08.2023

# package dplyr
library("tidyverse")

?diamonds

diamonds |>
  select(price, carat, cut) |>
  filter(carat < 3) |>
  mutate(lprice = log10(price)) |>
  group_by(cut) |>
  summarize(
    mean_lprice = mean(lprice),
    mean_carat = mean(carat)
  ) |>
  arrange(desc(mean_lprice))

# package ggplot2
set.seed(1984)

f1 <- "media/Rplot.png"
p1 <- diamonds |>                                                         #data
  filter(carat < 3) |>
  slice_sample(n = 500, by = cut) |>
  ggplot(mapping = aes(x = carat, y = price, color = cut)) +              #aes
  geom_point(alpha = 0.1, size = 1) +                                     #geom
  stat_smooth(method = "lm", formula = "y~x+I(x^2)+I(x^3)", se = FALSE) + #stat
  scale_x_continuous(trans = "log10") +                                   #scale
  scale_y_continuous(trans = "log10") +                                   #scale
  labs(x = "Weight", y = "Price", color = "Cut") +                        #scale
  theme_minimal()                                                         #theme
ggsave(f1, p1, "png", width = 17.2, height = 11.3, units = "cm", dpi = 72)

# package stats
diamonds2 <- diamonds |>                      #use data "diamonds"
  filter(
    (x > 0) & (y > 0 & y < 20) & (z > 0 & z < 10), #filter out outliers
    carat < 3) |>                             #filter in smaller diamonds
  slice_sample(n = 1000) |>                   #sample for 1000 observations
  select(price, carat, cut) |>                #select "price", "carat" and "cut"
  mutate(
    lprice = log10(price),                    #create variable "lprice"
    lcarat = log10(carat),                    #create variable "lprice"
    fct_cut = factor(cut, ordered = FALSE))   #make variable "cut" into factor

mod1 <- formula("lprice ~ lcarat + fct_cut")  #specify model
res1 <- lm(mod1, data = diamonds2)            #fit model to data

summary(res1)                                 #get summary

# package performance
library(performance)

f1 <- "media/assumptions.png"
png(f1, width = 17.2, height = 11.3, units = "cm", res = 72, type = "cairo")
check_model(res1, check = c(
  #MLR.1 The population model is linear in the parameters
  "linearity",
  #MLR.3 Random sample (e.g. no outliers, missing at random)
  "vif",
  #MLR.5 The error has constant variance given any values of the parameters
  "homogeneity",
  #MLR.6 The error is independent of the predictors and is normally distributed
  "qq"              
))
dev.off()

