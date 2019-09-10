


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
  separate(rate, into = c("cases", "population"), sep = "/")

## Your Turn
## 1. Is the NFL salaries from https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv that you loaded into `R` in a previous your turn tidy? Why or why not?
## 2. There is a data set in `tidyr` called `world_bank_pop` that contains information about population from the World Bank (https://data.worldbank.org/). Why is this data not tidy? You may want to read more about the data to answer (`?world_bank_pop`).
## 3. Use functions in `tidyr` to turn this into a tidy form.