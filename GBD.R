# Get the data and stuff

pacman::p_load(data.table, readxl, tidyverse, janitor)
setwd("/Users/mikey/Downloads/GBD")
data = fread("GBD Data.csv")
country_incomes = data.table(read_excel("Country Incomes.xlsx"))

data <- data %>% 
  janitor::clean_names()

# Change a stupid variable name

setnames(country_incomes, "Economy", "Country")

# Assign countries to locations

# Assign low-, middle-, high-income country to each row
# 
# data = merge(
#   data, 
#   country_incomes[, .(Country, `Income group`)], 
#   by.x="Location", 
#   by.y="Country", 
#   all.x=TRUE
#   )
# table(data$`Income group`)

# Create a new variable that combines the starting age and the ending age of the sample

# data[, age_bucket := paste0(`Age Start`,"-",`Age End`)]
# data[, old := fifelse(`Age End`<65,0,fifelse(`Age Start`>=65,1,NA_real_))]

# Break studies down into one row per age year
data %>% 
  select(age_end) %>% 
  unique() %>% 
  arrange(desc(age_end))

# arrange(unique(select(data, age_end)), desc(age_end))

data_small <- data %>% 
  select(location, age_start, age_end, sample_size)

data_small <- data_small %>% 
  mutate(age = 0)

for (i in 1:99) {
  data_small <- data_small %>% 
    bind_rows(
      mutate(data, age = i)
    )
  print(i)
}

data_small <- data_small %>% 
  mutate(
    in_range = age >= age_start & age <= age_end,
    young = age < 65
  )

# Calculate proportion of population by age by country

population = read_excel("population.xlsx", skip = 16)

population <- population %>%
  filter(`1` != "...") %>% 
  mutate(across(`0`:`100+`, parse_number)) %>%
  mutate(`99` = `99` + `100+`) %>% 
  pivot_longer(`0`:`99`, names_to = "age", values_to = "pop") %>% 
  mutate(pop = pop * 1000) %>% 
  janitor::clean_names() %>% 
  select(country = region_subregion_country_or_area, iso3 = iso3_alpha_code, age, pop)


# Scale sample sizes down by percent of country population that is that age

# Sum up population by buckets (income group X age range)

# Sum up sample size by same buckets

# Divide
