#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   4.3.1
#criado:     05.07.2023
#modificado: 03.10.2024

# 0. INDEX
{
# 1. VARIATION
# 1.1. TYPICAL VALUES
# 1.2. UNUSUAL VALUES
# 2. MISSING VALUES
# 2.1. EXPLICIT MISSING VALUES
# 2.2. IMPLICIT MISSING VALUES
# 2.3. FACTORS AND EMPTY GROUPS
# 3. COVARIATION
# 3.1. CATEGORICAL AND NUMERICAL VARIABLES
# 3.2. TWO CATEGORICAL VARIABLES
# 3.3. TWO NUMERICAL VARIABLES
# 4. PATTERNS AND MODELS

}
# 1. VARIATION
{
library("tidyverse")

## 1.1. TYPICAL VALUES

# a) Explore the distribution of each of the x, y, and z variables in diamonds.
# What do you learn? Think about a diamond and how you might decide which
# dimension is the length, width, and depth.
diamonds |>
  select(x, y, z) |>                   #select columns "x", "y", "z
  summary()                            #calculate summary statistics
diamonds |>  
  ggplot(mapping = aes(x = x)) +
    geom_histogram(binwidth = 0.1)     #histogram of "x"
diamonds |>  
  ggplot(mapping = aes(x = y)) +
    geom_histogram(binwidth = 0.1)     #histogram of "y"
diamonds |>  
  ggplot(mapping = aes(x = z)) +
    geom_histogram(binwidth = 0.1)     #histogram of "z"
# 1. x and y are larger than z,
# 2. there are outliers,
# 3. they are all right skewed, and
# 4. they are multimodal or “spiky”.

diamonds |>
  filter(x == 0 | y == 0 | z == 0)     #filter observations with value 0
# Zero = missing values?

diamonds |>
  arrange(desc(y)) |>
  slice_head(n = 6)
diamonds |>
  arrange(desc(z)) |>
  slice_head(n = 6)
# Data errors?

ggplot(diamonds, aes(x = x, y = y)) +
  geom_point()
ggplot(diamonds, aes(x = x, y = z)) +
  geom_point()
ggplot(diamonds, aes(x = y, y = z)) +
  geom_point()
# Clear outliers

diamonds |>
  filter(x > 0, x < 10) |>             #filter out unusual observations
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_continuous(breaks = 1:10)
diamonds |>
  filter(y > 0, y < 10) |>             #filter out unusual observations
  ggplot(aes(x = y)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_continuous(breaks = 1:10)
diamonds |>
  filter(z > 0, z < 10) |>
  ggplot(aes(x = z)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_continuous(breaks = 1:10)

# b) Explore the distribution of price. Do you discover anything unusual or
# surprising? (Hint: Carefully think about the binwidth and make sure you try a
# wide range of values.)
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 100)

diamonds |>
  filter(price < 2500) |>              #filter for observations less than 2500
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 10)
# No diamonds with price $1,500
# Bulge in distribution around $750.

diamonds |>
  mutate(ending = price %% 10) |>      #calculate ending algorism of "price"
  ggplot(aes(x = ending)) +
  geom_histogram(binwidth = 1) + 
  scale_x_continuous(breaks = 0:9)
# No rounding to 5 or 0

# c) How many diamonds are 0.99 carat? How many are 1 carat? What do you think
# is the cause of the difference? 
diamonds |>
  #filter for observations with carat of 0.99 or 1.00
  filter(carat %in% c(0.99,1)) |>
  count(carat)
# Round up to 1?

# d) Compare and contrast coord_cartesian() vs xlim() or ylim() when zooming in
# on a histogram. What happens if you leave binwidth unset? What happens if you
# try and zoom so only half a bar shows?
ggplot(diamonds, aes(x = price)) +
  geom_histogram() +
  coord_cartesian(xlim = c(100, 5000), ylim = c(0, 3000))

ggplot(diamonds, aes(x = price)) +
  geom_histogram() +
  xlim(100, 5000) + ylim(0, 3000)

## 1.2. UNUSUAL VALUES [No exercises]

}
# 2. MISSING VALUES
{
library("nycflights13")
library("tidyverse")

## 2.1. EXPLICIT MISSING VALUES

# a) What happens to missing values in a histogram? What happens to missing
# values in a bar chart? Why is there a difference?
diamonds |>
  mutate(y = if_else(y < 3 | y > 20, NA, y)) |>
  ggplot(aes(x = y)) +
  geom_histogram()

diamonds |>
  mutate(cut = if_else(y < 3 | y > 20, NA, cut)) |>
  ggplot(aes(x = cut)) +
  geom_bar()

# b) What does na.rm = TRUE do in mean() and sum()?
diamonds |>
  mutate(carat = if_else(y < 3 | y > 20, NA, carat)) |>
  summarize(
    average_with_NA = mean(carat),
    average_without_NA = mean(carat, na.rm = TRUE)
  )
  
## 2.2. IMPLICIT MISSING VALUES

# a) Can you find any relationship between the carrier and the rows that appear
# to be missing from planes?
flights |> 
  distinct(tailnum) |>                    #obtain distinct "tailnum"
  anti_join(planes)                       #obtain absent "tailnum"

flights |> 
  mutate(
    absent = !tailnum %in% planes$tailnum #calculate if tailnum is absent
  ) |>
  group_by(carrier) |>                    #group by "carrier"
  summarize(n_absent = sum(absent),       #calculate number of "absent"
            n_total = n(),                #calculate number of observations
            p_absent = mean(absent)       #calculate probability of "absent"
  ) |>
  filter(n_absent > 0)

## 2.3. FACTORS AND EMPTY GROUPS [No exercises]

}
# 3. COVARIATION
{
library("nycflights13")
library("lvplot")
library("ggbeeswarm")
library("gridExtra")
library("tidyverse")

## 3.1. CATEGORICAL AND NUMERICAL VARIABLES

# a) Use what you’ve learned to improve the visualization of the departure times
# of cancelled vs. non-cancelled flights.  
flights |>
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) |>
  ggplot(aes(y = sched_dep_time, x = cancelled)) +
  geom_boxplot()

# b) What variable in the diamonds dataset is most important for predicting the
# price of a diamond? How is that variable correlated with cut? Why does the
# combination of those two relationships lead to lower quality diamonds being
# more expensive?

#price ~ carat 
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point()

ggplot(diamonds, aes(x = carat, y = price, group = cut_width(carat, 0.1))) +
  geom_boxplot()

#price ~ color
diamonds |>
  mutate(color = fct_rev(color)) |>
  ggplot(aes(x = color, y = price)) +
  geom_boxplot()
  
#price ~ clarity
ggplot(diamonds, aes(x = clarity, y = price)) +
  geom_boxplot()

#carat ~ cut  
ggplot(diamonds, aes(x = cut, y = carat)) +
  geom_boxplot()  

# c) One problem with boxplots is that they were developed in an era of much
# smaller datasets and tend to display a prohibitively large number of “outlying
# values”. One approach to remedy this problem is the letter value plot. Install
# the lvplot package, and try using geom_lv() to display the distribution of
# price vs. cut. What do you learn? How do you interpret the plots?

ggplot(diamonds, aes(x = cut, y = carat)) +
  geom_boxplot()

diamonds |>
  group_by(cut) |>
  mutate(
    carat_out_lower = carat < quantile(carat,0.25) - IQR(carat) * 2.0,
    carat_out_upper = carat > quantile(carat,0.75) + IQR(carat) * 2.0,
    carat_out = if_else(carat_out_lower | carat_out_upper, carat, NA)
  ) |>
  ggplot(aes(x = cut, y = carat)) +
  geom_boxplot(outlier.shape = NA, coef = 2.0) +
  geom_jitter(aes(y = carat_out),
    height = 0,
    width = 0.1,
    shape = 1,
    size = 1.5,
    alpha = 0.5
  )

ggplot(diamonds, aes(x = cut, y = price)) +
  geom_lv()
  
# d) Create a visualization of diamond prices vs. a categorical variable from
# the diamonds dataset using geom_violin(), then a faceted geom_histogram(),
# then a colored geom_freqpoly(), and then a colored geom_density(). Compare and
# contrast the four plots. What are the pros and cons of each method of
# visualizing the distribution of a numerical variable based on the levels of a
# categorical variable?

#geom_freqpoly()
gg1 <- ggplot(diamonds, aes(x = price, y = after_stat(density), color = cut)) +
  geom_freqpoly(binwidth = 500)

#geom_histogram() with facet_wrap()
gg2 <- ggplot(diamonds, aes(x = price)) +
  geom_histogram() +
  facet_wrap(~cut, ncol = 1, scales = "free_y")

#geom_violin()
gg3 <- ggplot(diamonds, aes(x = price, y = cut)) +
  geom_violin() +
  scale_y_discrete(limits=rev)

#geom_density()
gg4 <- ggplot(diamonds, aes(x = price, color = cut)) +
  geom_density(bw = 500)

grid.arrange(gg1, gg4, gg2, gg3, ncol = 2)

# e) If you have a small dataset, it’s sometimes useful to use geom_jitter() to
# avoid overplotting to more easily see the relationship between a continuous
# and categorical variable. The ggbeeswarm package provides a number of methods
# similar to geom_jitter(). List them and briefly describe what each one does.
set.seed(123)
diamonds2 <- diamonds |> slice_sample(n = 250)

#geom_point()
gg1 <- ggplot(diamonds2, aes(x = cut, y = price)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = "point")

#geom_jitter()
gg2 <- ggplot(diamonds2, aes(x = cut, y = price)) +
  geom_jitter(height = 0, width = 0.2) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = "jitter")

#geom_quasirandom() with quasirandom()
gg3 <- ggplot(diamonds2, aes(x = cut, y = price)) +
  geom_quasirandom() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = "quasirandom")

#geom_beeswarm()
gg4 <- ggplot(diamonds2, aes(x = cut, y = price)) +
  geom_beeswarm() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = "beeswarm")

grid.arrange(gg1, gg2, gg3, gg4, ncol = 2)

## 3.2. TWO CATEGORICAL VARIABLES

# a) How could you rescale the count dataset above to more clearly show the
# distribution of cut within color, or color within cut?

#color within cut
diamonds |>
  count(color, cut) |>
  group_by(color) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = color, y = cut, fill = prop)) +
  geom_tile()

#cut within color
diamonds |>
  count(color, cut) |>
  group_by(cut) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = color, y = cut, fill = prop)) +
  geom_tile()

# b) What different data insights do you get with a segmented bar chart if color
# is mapped to the x aesthetic and cut is mapped to the fill aesthetic?
# Calculate the counts that fall into each of the segments.

#geom_bar()
ggplot(diamonds, aes(x = color, fill = cut)) +
  geom_bar()

#geom_bar()
ggplot(diamonds, aes(x = color, fill = cut)) +
  geom_bar(position="fill")

#table
diamonds |> 
  count(color, cut) |>
  pivot_wider(
    names_from = color,
    values_from = n
  )

# c) Use geom_tile() together with dplyr to explore how average flight delays
# vary by destination and month of year. What makes the plot difficult to read?
# How could you improve it?

#simple plot
flights |>
  group_by(month, dest) |>
  summarise(
    dep_delay = mean(dep_delay, na.rm = TRUE)
  ) |>
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile() +
  labs(x = "Month", y = "Destination", fill = "Departure Delay")

#updated plot  
flights |>
  group_by(month, dest) |>
  summarise(
    dep_delay = if_else(n() > 5, mean(dep_delay, na.rm = TRUE), NA),
    .groups = "drop"
  ) |>
  complete(month, dest) |>
  mutate(
    dest_with_na = any(is.na(dep_delay)),
    .by = dest
  ) |>
  filter(!dest_with_na) |>
  mutate(dest = reorder(dest, dep_delay)) |>
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile() +
  labs(x = "Month", y = "Destination", fill = "Departure Delay")  +
  theme(axis.text = element_text(size=8))
  
## 3.3. TWO NUMERICAL VARIABLES

# a) Instead of summarizing the conditional distribution with a box plot, you
# could use a frequency polygon. What do you need to consider when using
# cut_width() vs cut_number()? How does that impact a visualization of the 2d
# distribution of carat and price?

#geom_freqpoly() with cut_number()
ggplot(diamonds, aes(color = cut_number(carat, 5), x = price)) +
  geom_freqpoly() +
  labs(x = "Price", y = "Count", color = "Carat")
  
#geom_freqpoly() with cut_width()
ggplot(diamonds, aes(color = cut_width(carat, 1, boundary = 0), x = price)) +
  geom_freqpoly() +
  labs(x = "Price", y = "Count", color = "Carat")

# b) Visualize the distribution of carat, partitioned by price.

#geom_boxplot() with cut_number()
ggplot(diamonds, aes(x = carat, y = cut_number(price, 10))) +
  geom_boxplot() +
  labs(y = "Price")

#geom_boxplot() with cut_width()
ggplot(diamonds, aes(x = carat, y = cut_width(price, 2000, boundary = 0))) +
  geom_boxplot(varwidth = TRUE) +
  labs(y = "Price")
  
# c) How does the price distribution of very large diamonds compare to small
# diamonds. Is it as you expect, or does it surprise you?
ggplot(diamonds, aes(x = price, y = cut_number(carat, 5))) +
  geom_boxplot() +
  labs(y = "Carat")

# d) Combine two of the techniques you’ve learned to visualize the combined
# distribution of cut, carat, and price.

#geom_hex() and facet_wrap()
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_hex() +
  facet_wrap(~cut, ncol = 1)

#geom_boxplot() and colour
ggplot(diamonds, aes(x = cut, y = price, colour = cut_number(carat, 5))) +
  geom_boxplot() +
  labs(colour = "Carat")

# e) Two dimensional plots reveal outliers that are not visible in one
# dimensional plots. For example, some points in the plot below have an unusual
# combination of x and y values, which makes the points outliers even though
# their x and y values appear normal when examined separately. Why is a
# scatterplot a better display than a binned plot for this case?
#   diamonds |> 
#     filter(between(x, 4, 11), between(y, 4, 11)) |> 
#     ggplot(aes(x = x, y = y)) +
#     geom_point()
diamonds |> 
  filter(between(x, 4, 11), between(y, 4, 11)) |> 
  ggplot(aes(x = x, y = y)) +
  geom_hex()


}
# 4. PATTERNS AND MODELS
{
library("tidymodels")
library("tidyverse")

# a) Obtain a sample with 20 observation from the dataset diamonds. Make the
# appropriate data transformation and fit a linear model between the price and 
# the weight of diamonds. Plot the observations, the fitted value and the
# residuals. Take a look at geom_segment() for plotting the residuals.
set.seed(123)
diamonds2 <- diamonds |>
  mutate(
    log_price = log(price),
    log_carat = log(carat)
  ) |>
  slice_sample(n = 20)

diamonds_fit <- linear_reg(engine = "lm") |>
  fit(log_price ~ log_carat, data = diamonds2)

augment(diamonds_fit, new_data = diamonds2) |>
  ggplot(aes(x = log_carat, y = log_price)) +
  geom_point() +
  stat_smooth(method = "lm", formula = "y ~ x", se = FALSE) +
  geom_segment(aes(xend = log_carat, yend = .pred), color = "red")

}
