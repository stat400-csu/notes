library(tidyverse)

## Your Turn
## 1. Is the NFL salaries from https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv that you loaded into `R` in a previous your turn tidy? Why or why not?

salaries <- read_tsv("https://raw.githubusercontent.com/ada-lovecraft/ProcessingSketches/master/Bits%20and%20Pieces/Football_Stuff/data/nfl-salaries.tsv")
head(salaries)

# Yes, this looks tide because each variable has its own column and each observation has its own row.

## 2. There is a data set in `tidyr` called `world_bank_pop` that contains information about population from the World Bank (https://data.worldbank.org/). Why is this data not tidy? You may want to read more about the data to answer (`?world_bank_pop`).

# sorry, this is in the dev version.
load("0_tools/world_bank_pop.Rdata")
head(world_bank_pop)

# This is not tidy because it has a new column for each year (observation) 
# and uses the year information as a column name
# Also it has total and growth in the same column, which means not each variable is in a column

## 3. Use functions in `tidyr` to turn this into a tidy form.
world_bank_pop %>%
  gather(year, value, `2000`:`2017`) %>%
  spread(indicator, value)