## ------------------------------------------------------------------------
library(tidyverse)


## ------------------------------------------------------------------------
# load readr
library(readr)

# read a csv
recent_grads <- read_csv(file = "https://raw.githubusercontent.com/fivethirtyeight/data/master/college-majors/recent-grads.csv")
head(recent_grads)

## Your turn ---------------------------------------------------------------

# 1. Read the NFL salaries dataset from https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv into `R`. 
# 2. What is the highest NFL salary in this dataset? Who is the highest paid player?
# 3. Make a histogram and describe the distribution of NFL salaries.

nfl <- read_tsv("https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv")
max_sal <- max(nfl$Salary)

nfl[nfl$Salary == max_sal,]
nfl[which(nfl$Salary == max_sal),]

ggplot(data = nfl) +
  geom_histogram(aes(Salary))

## add custom scaling
ggplot(data = nfl) +
  geom_histogram(aes(Salary)) +
  scale_x_continuous(labels = scales::dollar_format(prefix="$")) # relies on scales package

## ---- eval=FALSE---------------------------------------------------------
## a %>% b()


## ---- eval=FALSE---------------------------------------------------------
## b(a)


## ------------------------------------------------------------------------
recent_grads %>% 
  filter(Major == "STATISTICS AND DECISION SCIENCE")


## ------------------------------------------------------------------------
recent_grads %>% 
  filter(Major_category == "Computers & Mathematics")


## ------------------------------------------------------------------------
math_grads <- recent_grads %>% filter(Major_category == "Computers & Mathematics")

recent_grads %>%
  filter(Major_category == "Computers & Mathematics") -> math_grads


## ------------------------------------------------------------------------
math_grads %>% arrange(ShareWomen)


## ------------------------------------------------------------------------
math_grads %>% arrange(desc(ShareWomen))


## ------------------------------------------------------------------------
math_grads %>% select(Major, ShareWomen, Total, Full_time, P75th)


## ------------------------------------------------------------------------
math_grads %>% select(Major, College_jobs:Low_wage_jobs)

math_grads[, -3]
math_grads[, names(math_grads) != "Major"]
# math_grads[, -"Major"]

math_grads %>% select(-Major)


## ------------------------------------------------------------------------
math_grads %>% rename(Code_major = Major_code)


## ------------------------------------------------------------------------
math_grads %>% mutate(Full_time_rate = Full_time_year_round/Total)

# we can't see everything
math_grads %>% 
  mutate(Full_time_rate = Full_time_year_round/Total,
         College_jobs_rate = College_jobs/Total) %>% 
  select(Major, ShareWomen, College_jobs_rate, Full_time_rate)


## ------------------------------------------------------------------------
math_grads %>% summarise(mean_major_size = mean(Total))

mean(math_grads$Total)


## ------------------------------------------------------------------------
math_grads %>% summarise(mean_major_size = mean(Total), num_majors = n())


## ------------------------------------------------------------------------
recent_grads %>%
  group_by(Major_category) %>%
  summarise(mean_major_size = mean(Total, na.rm = TRUE)) %>%
  arrange(desc(mean_major_size))

recent_grads %>%
  group_by(Major_category) %>%
  mutate(max_major_total = max(Total)) %>%
  select(Major_category, Major, Total, max_major_total) %>%
  filter(Total == max_major_total)

## Your turn --------------------------------------------------------------
# Using the NFL salaries from https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv that you loaded into `R` in the previous your turn, perform the following.
# 1. What is the team with the highest paid roster?
# 2. What are the top 5 paid players?
# 3. What is the highest paid position on average? the lowest? the most variable?

nfl %>%
  group_by(Team) %>%
  summarise(Salary = sum(Salary)) %>%
  arrange(desc(Salary)) %>%
  head()

nfl %>%
  arrange(desc(Salary)) %>%
  head(5)

nfl %>%
  group_by(Position) %>%
  summarise(mean_salary = mean(Salary)) %>%
  arrange(desc(mean_salary)) %>%
  head()

nfl %>%
  group_by(Position) %>%
  summarise(mean_salary = mean(Salary)) %>%
  arrange(mean_salary) %>%
  head()

nfl %>%
  group_by(Position) %>%
  summarise(sd_salary = sd(Salary)) %>%
  arrange(desc(sd_salary)) %>%
  head()

## ------------------------------------------------------------------------
table1

table2

table3

# spread across two data frames
table4a

table4b


## ------------------------------------------------------------------------
# Compute rate per 10,000
table1 %>% 
  mutate(rate = cases / population * 10000)

# Visualize cases over time
library(ggplot2)
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country)) + 
  geom_point(aes(colour = country))

table1 %>% 
  mutate(rate = cases / population * 10000) %>%
  ggplot(aes(year, rate)) + 
  geom_line(aes(group = country)) + 
  geom_point(aes(colour = country))


## ------------------------------------------------------------------------
table4a


## ---- eval=FALSE---------------------------------------------------------
## table4a %>%
##   pivot_longer(-country, names_to = "year", values_to = "cases")


## ---- eval=FALSE---------------------------------------------------------
## table4a %>%
##   pivot_longer(-country, names_to = "year", values_to = "cases") %>%
##   left_join(table4b %>% pivot_longer(-country, names_to = "year", values_to = "population"))


## ---- eval=FALSE---------------------------------------------------------
## table2
## 
## table2 %>%
##   pivot_wider(names_from = type, values_from = count)


## ------------------------------------------------------------------------
table4a


## ------------------------------------------------------------------------
table4a %>%
  gather(-country, key = "year", value = "cases")


## ------------------------------------------------------------------------
table4a %>%
  gather(-country, key = "year", value = "cases") %>%
  left_join(table4b %>% gather(-country, key = "year", value = "population"))


## ------------------------------------------------------------------------
table2

table2 %>%
  spread(key = type, value = count)


## ------------------------------------------------------------------------
table3


## ------------------------------------------------------------------------
table3 %>%
  separate(rate, into = c("cases", "population"), sep = "/") %>%
  mutate(cases = as.numeric(cases),
         population = as.numeric(population))

## Your turn --------------------------------------------------------------
# 1. Is the NFL salaries from https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv that you loaded into `R` in a previous your turn tidy? Why or why not?
# 2. There is a data set in `tidyr` called `world_bank_pop` that contains information about population from the World Bank (https://data.worldbank.org/). Why is this data not tidy? You may want to read more about the data to answer (`?world_bank_pop`).
# 3. Use functions in `tidyr` to turn this into a tidy form.


