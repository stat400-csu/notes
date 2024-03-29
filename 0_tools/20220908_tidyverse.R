## ------------------------------------------------------------------------
library(tidyverse)
library(dplyr)

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
## this won't work
nfl <- read_csv("https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv")
nfl <- read_tsv("https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv")

max(nfl$Salary)
nfl[nfl$Salary == max(nfl$Salary), "PlayerName"]

ggplot(nfl) +
  geom_histogram(aes(Salary), fill = "red")

## right skewed, unimodal, mode close to zero
median(nfl$Salary)

## ---- eval=FALSE---------------------------------------------------------
## a |> b()


## ---- eval=FALSE---------------------------------------------------------
## b(a)


## ------------------------------------------------------------------------
recent_grads |> 
  filter(Major == "STATISTICS AND DECISION SCIENCE")


## ------------------------------------------------------------------------
recent_grads |> filter(Major_category == "Computers & Mathematics")


## ------------------------------------------------------------------------
math_grads <- recent_grads |> filter(Major_category == "Computers & Mathematics")

recent_grads |>
  filter(Major_category == "Computers & Mathematics") -> math_grads

## ------------------------------------------------------------------------
math_grads |> arrange(ShareWomen)


## ------------------------------------------------------------------------
math_grads |> arrange(desc(ShareWomen))


## ------------------------------------------------------------------------
math_grads |> 
  select(Major, ShareWomen, Total, Full_time, P75th) |>
  arrange(desc(P75th))


## ------------------------------------------------------------------------
math_grads |> 
  select(Major, College_jobs:Low_wage_jobs) |>
  data.frame()


## ------------------------------------------------------------------------
math_grads |> rename(Code_major = Majorx_code)


## ------------------------------------------------------------------------
math_grads |> mutate(Full_time_rate = Full_time_year_round/Total)

# we can't see everything
math_grads |> 
  mutate(Full_time_rate = Full_time_year_round/Total) |> 
  select(Major, ShareWomen, Full_time_rate)


## ------------------------------------------------------------------------
math_grads |> 
  summarise(mean_major_size = mean(Total),
            max_major_size = max(Total),
            min_major_size = min(Total),
            num_majors = n())


## ------------------------------------------------------------------------
math_grads |> summarise(mean_major_size = mean(Total), num_majors = n())


## ------------------------------------------------------------------------
recent_grads |>
  group_by(Major_category) |>
  summarise(mean_major_size = mean(Total, na.rm = TRUE)) |>
  arrange(desc(mean_major_size))

## Your turn --------------------------------------------------------------
# Using the NFL salaries from https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv that you loaded into `R` in the previous your turn, perform the following.
# 1. What is the team with the highest paid roster?
# 2. What are the top 5 paid players?
# 3. What is the highest paid position on average? the lowest? the most variable?

nfl |>
  group_by(Team) |>
  summarise(salary = sum(Salary)) |>
  arrange(desc(salary)) |>
  #head(1)
  top_n(1)

nfl |>
  arrange(desc(Salary)) |>
#  head(5)
  top_n(5)

nfl |>
  group_by(Position) |>
  summarise(mean_salary = mean(Salary),
            sd_salary = sd(Salary)) -> nfl_position_stats

## highest average position
nfl_position_stats |>
  arrange(desc(mean_salary)) |>
  top_n(1) ## look into this function more

## lowest average position
nfl_position_stats |>
  filter(!is.na(Position)) |>
  arrange(mean_salary) |>
  head(1)

nfl_position_stats |>
  arrange(sd_salary) |>
  tail(1)


## ------------------------------------------------------------------------
table1

table2

table3

# spread across two data frames
table4a

table4b


## ------------------------------------------------------------------------
# Compute rate per 10,000
table1 |> 
  mutate(rate = cases / population * 10000)

# Visualize cases over time
library(ggplot2)
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country)) + 
  geom_point(aes(colour = country))


## ------------------------------------------------------------------------
table4a


## ---- eval=FALSE---------------------------------------------------------
table4a |>
  pivot_longer(-country, 
               names_to = "year", 
               values_to = "cases")


## ---- eval=FALSE---------------------------------------------------------
table4a |>
  pivot_longer(-country, names_to = "year", values_to = "cases") -> tidy_a

table4b |> 
  pivot_longer(-country, names_to = "year", values_to = "population") -> tidy_b

tidy_a |>
  left_join(tidy_b, by = c("year", "country"))
  
tidy_a |>
  left_join(tidy_b)


## ---- eval=FALSE---------------------------------------------------------
table2

table2 |>
  pivot_wider(names_from = type, 
              values_from = count)


## ------------------------------------------------------------------------
table3 |>
  separate(rate, 
           into = c("cases", "population"), 
           sep = "/") |>
  mutate(cases = as.numeric(cases),
         population = as.numeric(population))

## Your turn --------------------------------------------------------------
# 1. Is the NFL salaries from https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv that you loaded into `R` in a previous your turn tidy? Why or why not?
# 2. There is a data set in `tidyr` called `world_bank_pop` that contains information about population from the World Bank (https://data.worldbank.org/). Why is this data not tidy? You may want to read more about the data to answer (`?world_bank_pop`).
# 3. Use functions in `tidyr` to turn this into a tidy form.


