#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   4.3.1
#criado:     05.07.2023
#modificado: 03.10.2024

# 0. INDEX
{
# 1. NUMERIC VECTORS
# 1.1. COUNTS
# 1.2. NUMERIC TRANSFORMATION
# 1.3. GENERAL TRANSFORMATION
# 1.4 SUMMARY STATISTICS
# 2. FACTORS
# 2.1. BASICS
# 2.2. DATASET gss_cat
# 2.3. MODIFYING FACTOR ORDER
# 2.4. MODIFYING FACTOR LEVEL
# 3. LOGICAL VECTORS
# 3.1. COMPARISONS
# 3.2. BOOLEAN ALGEBRA
# 3.3. SUMMARIES
# 3.4. CONDITIONAL TRANSFORMATION

}
# 1. NUMERIC VECTORS
{
library("nycflights13")
library("tidyverse")

## 1.1. COUNTS

# a) How can you use count() to count the number of rows with a missing value
# for a given variable?
flights |> filter(is.na(dep_time)) |> count()

flights |> filter(if_any(everything(), ~ is.na(.))) |> count()

flights |>
  select(where(~ any(is.na(.)))) |>
  summarise(across(everything(),~ sum(is.na(.))))
  
# b) Expand the following calls to count() to instead use group_by(),
# summarize(), and arrange():
#    1. flights |> count(dest, sort = TRUE)
flights |> 
  group_by(dest) |>
  summarize(n=n()) |>
  arrange(desc(n))

#    2. flights |> count(tailnum, wt = distance)
flights |> 
  group_by(tailnum) |> 
  summarize(miles = sum(distance))

## 1.2. NUMERIC TRANSFORMATION

# a) Explain in words what each line of the following code does:
#flights |> 
#  group_by(hour = sched_dep_time %/% 100) |> 
#  summarize(prop_cancelled = mean(is.na(dep_time)), n = n()) |> 
#  filter(hour > 1) |> 
#  ggplot(aes(x = hour, y = prop_cancelled)) +
#  geom_line(color = "grey50") + 
#  geom_point(aes(size = n))
#use dataset flights
flights |>
  #group flights per hour of schedule departure
  group_by(hour = sched_dep_time %/% 100) |>
  summarize(
    #calculate proportion of NAs in dep_time
    prop_cancelled = mean(is.na(dep_time)),
    #calculate total number of flights
    n = n()) |>
  #filter-out flights schedule for before 1:00
  filter(hour > 1) |>
  #select variables hour and prop_cancelled
  ggplot(aes(x = hour, y = prop_cancelled)) +
  #create lineplot of variables
  geom_line(color = "grey50") +
  #create scatterpoints of variables with size = n
  geom_point(aes(size = n))

# b) Currently dep_time and sched_dep_time are convenient to look at, but hard
# to compute with because they’re not really continuous numbers. You can see the
# basic problem by running the code below: there’s a gap between each hour.
#    flights |> 
#      filter(month == 1, day == 1) |> 
#      ggplot(aes(x = sched_dep_time, y = dep_delay)) +
#      geom_point()
#    Convert them to a more truthful representation of time (either fractional hours or minutes since midnight).
flights |>
  mutate(
    hour = sched_dep_time %/% 100,  #extract hours of schedule departure
    minute = sched_dep_time %% 100, #extract minutes of schedule departure
    time1 = hour + minute/60,       #fractional hours since midnight
#   time1 = hour*60 + minute        #minutes since midnight
  ) |>
  filter(month == 1, day == 1) |>
  ggplot(aes(x = time1, y = dep_delay)) +
  geom_point() + 
  labs(x = "fractional hours since midnight")
  
# c) Round dep_time and arr_time to the nearest five minutes
flights |>
  mutate(
    #extract hours of departure
    dep_time_hh = dep_time %/% 100,
    #extract minutes of departure in 5min interval
    dep_time_mm = round(dep_time %% 100 / 5) * 5,
    #compute departure time using 5min interval
    dep_time2 = dep_time_hh * 100 + dep_time_mm,
    .keep="used"
  )

## 1.3. GENERAL TRANSFORMATION

# a) Find the 10 most delayed flights using a ranking function. How do you want
# to handle ties? Carefully read the documentation for min_rank().
?min_rank
?base::rank
flights |>
  mutate(ranks = min_rank(desc(dep_delay))) |>
  select(tailnum,dep_delay,ranks) |>
  arrange(ranks)

flights |>
  select(tailnum,dep_delay) |>
  arrange(desc(dep_delay))
  
# b) Which plane (tailnum) has the worst on-time record?
flights |>
  filter(!is.na(dep_delay)) |>     #filter out NA values
  group_by(tailnum) |>             #group by "tailnum"
  summarize(
    dep_delay_rec = max(dep_delay) #calculate maximum delay per group
  ) |>
  arrange(desc(dep_delay_rec))

# c) What time of day should you fly if you want to avoid delays as much as
# possible?
flights |>
  filter(!is.na(dep_delay)) |>
  group_by(sched_dep_time) |>
  mutate(n_delay = sum(dep_delay > 0),  #n_delay; number of delays
         m_delay = sum(dep_delay),      #m_delay; mean delay
         p_delay = mean(dep_delay > 0), #p_delay: proportion of delays
         t_delay = sum(dep_delay)       #t_delay: total minutes delayed
  ) |>
  ggplot(aes(x = sched_dep_time, y = p_delay)) +
  geom_bin2d(bins=50)
  
# d) What does flights |> group_by(dest) |> filter(row_number() < 4) do? What
# does flights |> group_by(dest) |> filter(row_number(dep_delay) < 4) do?
?row_number

flights |>
  #group by "dest"
  group_by(dest) |>
  #filter for the first 3 elements per group
  filter(row_number() < 4) |>
  select(dest, everything()) |> arrange(dest)

flights |>
  #group by "dest"
  group_by(dest) |>
  #filter for the 3 largest departure delays per group
  filter(row_number(dep_delay) < 4) |>
  select(dest, everything()) |> arrange(dest)
 
# e) For each destination, compute the total minutes of delay. For each flight,
# compute the proportion of the total delay for its destination.
flights |>
  #group by "dest"
  group_by(dest) |>
  #sum up delay time per group
  summarize(t_delay = sum(dep_delay, na.rm = TRUE))

flights |>
  #filter out early departures
  filter(dep_delay > 0) |>
  #group by "flight"
  group_by(dest, origin, carrier, flight) |>
  #sum up delay time per group
  summarize(t_delay = sum(dep_delay),.groups="drop") |>
  #group by "dest"
  group_by(dest) |>
  mutate(
    #calculate proportion of delay per group
    p_delay = round(t_delay/sum(t_delay),3)
  ) |>
  arrange(dest, desc(p_delay)) |>
  select(carrier, flight, origin, dest, p_delay)

# f) Delays are typically temporally correlated: even once the problem that
# caused the initial delay has been resolved, later flights are delayed to allow
# earlier flights to leave. Using lag(), explore how the average flight delay
# for an hour is related to the average delay for the previous hour.
#   flights |> 
#     mutate(hour = dep_time %/% 100) |> 
#     group_by(year, month, day, hour) |> 
#     summarize(
#       dep_delay = mean(dep_delay, na.rm = TRUE),
#       n = n(),
#       .groups = "drop"
#     ) |> 
#     filter(n > 5)
flights_dep_lag <- flights |>
  #extract hour of departure
  mutate(hour = dep_time %/% 100) |>
  #group by "year", "month", "day" and "hour"
  group_by(year, month, day, hour) |>
  summarize(
    #calculate average delay per group
    dep_delay = mean(dep_delay, na.rm = TRUE),
    #calculate number of observations per group
    n = n(),
    .groups = "drop"
  ) |>
  #filter for groups with more than 5 observations
  filter(n > 5) |>
  #make sure data is order by time
  arrange(year, month, day, hour) |>
  #create variable of previous average delay
  mutate(dep_delay_lag = lag(dep_delay, n = 1)) |>
  #filter out no delays and no previous time point
  filter(dep_delay > 0, !is.na(dep_delay_lag))

flights_dep_lag |>
  summarize(cor(dep_delay,dep_delay_lag, use = "complete.obs"))

flights_dep_lag |>
  ggplot(aes(x = dep_delay_lag, y = dep_delay)) +
  geom_bin2d(bins = 50) +
  stat_smooth(method = "lm", formula = "y~x", se = FALSE)
    
rm(flights_dep_lag)

# g) Look at each destination. Can you find flights that are suspiciously fast 
# (i.e. flights that represent a potential data entry error)? Compute the air 
# time of a flight relative to the shortest flight to that destination. Which
# flights were most delayed in the air?
flights |>
  ggplot(aes(x = air_time, y = dest)) +
  geom_boxplot() +
  facet_wrap(~origin, ncol=3) +
  theme(axis.text = element_text(size=6))

flights |>
  #group by "origin" and "dest"
  group_by(origin, dest) |>
  mutate(
    #calculate Q25(air_time) per group
    at_Q25 = quantile(air_time, prob = 0.25, na.rm = TRUE),
    #calculate Q50(air_time) per group
    at_Q50 = quantile(air_time, prob = 0.50, na.rm = TRUE),
    #calculate Q75(air_time) per group
    at_Q75 = quantile(air_time, prob = 0.75, na.rm = TRUE),
    #calculate suspiciousness
    at_flg = air_time < at_Q25 - 2 * (at_Q75 - at_Q25)
  ) |>
  filter(at_flg) |>
  select(origin, dest, tailnum, air_time, at_Q25, at_Q50, at_Q75) |>
  arrange(desc(air_time))
    
flights |>
  #group by "origin" and "dest"
  group_by(origin, dest) |>
  mutate(
    #calculate minimum air time per group
    at_min = min(air_time, na.rm = TRUE),
    #calculate air_time relative to minimum per group
    at_rel = air_time/at_min,
    #calculate the top most delayed flight per group
    at_flg = min_rank(desc(at_rel)) == 1
  ) |>
  filter(at_flg) |>
  select(origin, dest, tailnum, air_time, at_min, at_rel) |>
  arrange(desc(at_rel))
  
# h) Find all destinations that are flown by at least two carriers. Use those
# destinations to come up with a relative ranking of the carriers based on their
# performance for the same destination.
flights |>
  #filter out NA values 
  filter(!is.na(air_time)) |>
  #group by "dest"
  group_by(dest) |>
  #calculate number of observations per group
  mutate(n_carriers = n_distinct(carrier)) |>
  #filter for groups at least 2 observations
  filter(n_carriers > 1) |>
  #group by "carrier"
  group_by(dest, origin, carrier) |>
  #calculate performance
  summarize(at_mean = mean(air_time)) |>
  group_by(dest, origin) |>
  #calculate rank of performance per group
  mutate(at_rnk = min_rank(at_mean)) |>
  select(dest, origin, carrier, at_mean, at_rnk) |>
  arrange(dest, origin, at_rnk)
  
## 1.4 SUMMARY STATISTICS

# a) Brainstorm at least 5 different summary statistics (center, location,
# spread) to describe the typical delay of flights to each destination.
# When is mean() useful? When is median() useful? When might you want to use
# something else? Should you use arrival delay or departure delay?
flights |>
  mutate(
#   v = dep_delay                      #select dep_delay as variable in use
    v = arr_delay                      #select arr_delay as variable in use
  ) |>
  filter(v > 0) |>                     #filter in delayed flights
  group_by(dest, origin) |>            #group by "dest" and "origin"
  summarize(
    n = n(),                           #calculate number of obs. per group
    dd_ave = mean(v),                  #calculate mean per group (center)
    dd_Q50 = median(v),                #calculate median per group (center)
    dd_min = min(v),                   #calculate minimum per group (location)
    dd_max = max(v),                   #calculate maximum per group (location)
    dd_Q05 = quantile(v, prob = 0.05), #calculate Q05 per group (location)
    dd_Q25 = quantile(v, prob = 0.25), #calculate Q25 per group (location)
    dd_Q75 = quantile(v, prob = 0.75), #calculate Q25 per group (location)
    dd_Q95 = quantile(v, prob = 0.95), #calculate Q05 per group (location)
    dd_IQR = dd_Q75 - dd_Q25,          #calculate IQR per group (spread)
    dd_rng = dd_max - dd_min,          #calculate range per group (spread)
    dd_non = sum(v == 0)               #calculate number of zeros
  ) |>
  filter(n > 5) |>                     #filter for groups with observations > 5
  arrange(dest)                        #order by "dest"
  
# b) Which destinations show the greatest variation in air speed?
flights |>
  #group by "dest"
  group_by(dest) |>
  summarize(
    #calculate variation of air time per group
    at_var = var(air_time, na.rm = TRUE),
    #calculate number of non-NA observations per group
    n = sum(!is.na(air_time))
  ) |> 
  arrange(desc(at_var))

# c) Create a plot to further explore the adventures of EGE. Can you find any
# evidence that the airport moved locations? Can you find another variable that
# might explain the difference?
flights |>
  filter(dest == "EGE") |>
  ggplot(aes(x = month, y = distance, color = origin)) +
  geom_point(size = 2) +
  geom_jitter(width = 0.2, height = 0.2, shape = 1, size = 1) + 
  scale_x_continuous(breaks = 1:12)

}
# 2. FACTORES
{
library("tidyverse")

# 2.1. BASICS
#[no exercises]

# 2.2. DATASET gss_cat

# a) Explore the distribution of rincome (reported income). What makes the
# default bar chart hard to understand? How could you improve the plot?
gss_cat |>
  ggplot(aes(x = rincome)) +
  geom_bar()

gss_cat |>
  ggplot(aes(y = fct_relevel(rincome, "Not applicable"))) +
  geom_bar()
  
# b) What is the most common relig in this survey? What’s the most common
# partyid?
gss_cat |> count(relig, sort = TRUE)

gss_cat |> count(partyid, sort = TRUE)

# c) Which relig does denom (denomination) apply to? How can you find out with a
# table? How can you find out with a visualization?
gss_cat |>
  group_by(denom) |>
  count() |>
  print(n = Inf)

gss_cat |>
  filter(!denom %in% c("No answer", "Other", "Don't know", "Not applicable",
    "No denomination")) |>
  count(relig)

gss_cat |>
  ggplot(aes(y=relig,fill=denom)) +
  geom_bar() +
  theme(legend.position="bottom", legend.box="vertical")
  
gss_cat |>
  ggplot(aes(x=relig,y=denom)) +
  geom_count() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(axis.text.y = element_text(size = 7))
  
# 2.3. MODIFYING FACTOR ORDER

# a) For each factor in gss_cat identify whether the order of the levels is
# arbitrary or principled.
gss_cat |> count(marital) #somewhat principled
gss_cat |> count(race)    #principled by count of observations
gss_cat |> count(rincome) #principled
gss_cat |> count(partyid) #principled
gss_cat |> count(relig)   #arbitrary
gss_cat |> count(denom)   #arbitrary
  
# b) Why did moving “Not applicable” to the front of the levels move it to the
# bottom of the plot?
gss_cat |>
  #move closer to origin of axis
  ggplot(aes(y = fct_relevel(rincome, "Not applicable"))) +
  geom_bar()

# 2.4. MODIFYING FACTOR LEVEL

# a) How have the proportions of people identifying as Democrat, Republican, and
# Independent changed over time?
gss_cat_mod <- gss_cat |>
  mutate(
    partyid = fct_collapse(partyid,
      "other" = c("No answer", "Don't know", "Other party"),
      "rep" = c("Strong republican", "Not str republican"),
      "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
      "dem" = c("Not str democrat", "Strong democrat")
    )
  )

gss_cat_mod |>
  ggplot(aes(x = year, fill = partyid)) +
  geom_bar(position = "fill")

gss_cat_mod |>
  count(year, partyid) |>
  group_by(year) |>
  mutate(p = n/sum(n)) |>
  ggplot(aes(x = year, y = p, colour = partyid)) +
  geom_point() +
  geom_line()

rm(gss_cat_mod)
    
# b) How could you collapse rincome into a small set of categories?
gss_cat |> count(rincome)
gss_cat |>
  mutate(
    rincome = fct_collapse(rincome,
      "Not applicable" = "Not applicable",
      "N/A"            = c("No answer",
                           "Don't know",
                           "Refused"),
      "$25000 or more" = "$25000 or more",
      "$10000 - 24999" = c("$20000 - 24999",
                           "$15000 - 19999",
                           "$10000 - 14999"),
      "$5000 - 9999"   = c("$8000 to 9999",
                           "$7000 to 7999",
                           "$6000 to 6999",
                           "$5000 to 5999"),
      "Lt $5000"       = c("$4000 to 4999",
                           "$3000 to 3999",
                           "$1000 to 2999",
                           "Lt $1000")
    )
  ) |>
  count(rincome)
  
# c) Notice there are 9 groups (excluding other) in the fct_lump example above.
# Why not 10? (Hint: type ?fct_lump, and find the default for the argument
# other_level is “Other”.)
gss_cat |>
  mutate(relig = fct_lump_n(relig, n = 10)) |>
  count(relig) |>
  print(n = Inf)

gss_cat |>
  mutate(relig = fct_lump_n(relig, n = 10, other_level="Other answer")) |>
  count(relig) |>
  print(n = Inf)

}
# 3. LOGICAL VECTORS
{
library("nycflights13")
library("tidyverse")

# 3.1. Comparison

# a) How does dplyr::near() work? Type near to see the source code. 
# Is sqrt(2)^2 near 2?
?near
near
near(sqrt(2)^2, 2)
near(sqrt(2)^2, 2, tol = 1e-16)

# b) Use mutate(), is.na(), and count() together to describe how the missing
# values in dep_time, sched_dep_time and dep_delay are connected.
flights |>
  mutate(
    #calculate number of missing values for dep_time
    dt_na = is.na(dep_time),
    #calculate number of missing values for sched_dep_time
    sdt_na = is.na(sched_dep_time),
    #calculate number of missing values for dep_delay
    dd_na = is.na(dep_delay),
    .keep="used"
  ) |>
  #group by "dt_na", "sdt_na" and "dd_na"
  group_by(dt_na, sdt_na, dd_na) |>
  count()
  
# 3.2. Boolean algebra

# a) Find all flights where arr_delay is missing but dep_delay is not. Find all
# flights where neither arr_time nor sched_arr_time are missing, but arr_delay
# is.
flights |>
  filter(is.na(arr_delay) & !is.na(dep_delay))

flights |>
  filter(!is.na(arr_time) & !is.na(sched_arr_time) & is.na(arr_delay))
  
# b) How many flights have a missing dep_time? What other variables are missing 
#in these rows? What might these rows represent?
flights |> filter(is.na(dep_time)) |> count()

flights |>
  #filter in cancelled flights
  filter(is.na(dep_time)) |>
  #select any columns with missing values
  select(where(~ any(is.na(.)))) |>
  #calculate number of missing values
  summarise(across(everything(),~ sum(is.na(.))))

# c) Assuming that a missing dep_time implies that a flight is cancelled, look
# at the number of cancelled flights per day. Is there a pattern? Is there a 
# connection between the proportion of cancelled flights and the average delay
# of non-cancelled flights?
flights |>
  #group by "year", "month", "day"
  group_by(year, month, day) |>
  summarise(
    #number of missing in "dep_time" per groups
    n_cancel = sum(is.na(dep_time)),
    .groups = "drop"
  ) |> 
  mutate(
    #calculate day of the year
    year_day = row_number()
  ) |>
  ggplot(aes(x = year_day, y = n_cancel)) +
  geom_line() +
  geom_point()

flights |>
  #group by "year", "month", "day"
  group_by(year, month, day) |>
  summarise(
    #proportion of missing in "dep_time" per groups
    p_cancel = mean(is.na(dep_time)),
    #average departure delay per groups
    dd_ave   = mean(dep_delay, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  ggplot(aes(x = dd_ave, y = p_cancel)) +
  geom_point()

# 3.3. Summaries

# a) What will sum(is.na(x)) tell you? How about mean(is.na(x))?
set.seed(123)
v_length <- 9
v <- 1:v_length
NA_index <- sample(1:v_length, size = (1/3)*v_length, replace = FALSE)
v[NA_index] <- NA
#v <- (1, 2, NA, 4, 5, NA, 7, 8, NA)
sum(is.na(v))
mean(is.na(v))

# b) What does prod() return when applied to a logical vector? What logical
# summary function is it equivalent to? What does min() return when applied to a
# logical vector? What logical summary function is it equivalent to? Read the
# documentation and perform a few experiments.
?prod
?min

v1 <- c(TRUE,  TRUE,  TRUE)
v2 <- c(FALSE, FALSE, FALSE)
v3 <- c(TRUE,  FALSE, FALSE)
prod(v1) #all(v1)
prod(v2) #all(v2)
prod(v3) #all(v3)
min(v1)  #any(v1)
min(v2)  #any(v2)
min(v3)  #any(v3)

# 3.4. Conditional transformations

# a) A number is even if it’s divisible by two, which in R you can find out with
# x %% 2 == 0. Use this fact and if_else() to determine whether each number
# between 0 and 20 is even or odd.
tibble(numbers = 1:20) |>
  mutate(eve_odd = if_else(1:20 %% 2 == 0,"even","odd"))

# b) Given a vector of days like x <- c("Monday", "Saturday", "Wednesday"), use
# an ifelse() statement to label them as weekends or weekdays.
set.seed(123)
wdays <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday",
  "Sunday")
tibble(wday = sample(wdays,15,rep=TRUE)) |>
  mutate(wlab = if_else(wday %in% c("Saturday","Sunday"),"weekends","weekdays"))

# c) Use ifelse() to compute the absolute value of a numeric vector called x.
v <- -6:6
ifelse(v < 0, -v, v)

# d) Write a case_when() statement that uses the month and day columns from
# flights to label a selection of important US holidays (e.g., New Years Day,
# 4th of July, Thanksgiving, and Christmas). First create a logical column that
# is either TRUE or FALSE, and then create a character column that either gives
# the name of the holiday or is NA.
flights |>
  mutate(
    is_important =
      month ==  1 & day ==  1 |        #New Years Day
      month ==  4 & day ==  7 |        #4th of July
      month == 11 & day == 28 |        #Thanksgiving 2013
      month == 12 & day == 25,         #Christmas
    important_day = case_when(
      month ==  1 & day ==  1 ~ "New Years Day",
      month ==  4 & day ==  7 ~ "4th of July",
      month == 11 & day == 28 ~ "Thanksgiving",
      month == 12 & day == 25 ~ "Christmas"
    ),
    .keep = "used"
  ) |>
  group_by(is_important,important_day) |>
  count()

}
