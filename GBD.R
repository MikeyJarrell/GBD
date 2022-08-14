# Get the data and stuff

pacman::p_load(data.table, readxl, tidyverse, janitor, fuzzyjoin)
setwd("/Users/mikey/Downloads/GBD")

age_old <- 64

data = fread("GBD Data.csv")
country_incomes = data.table(read_excel("Country Incomes.xlsx")) %>% 
  janitor::clean_names()

data <- data %>% 
  janitor::clean_names()

# Change a stupid variable name
setnames(country_incomes, "economy", "country")

# Assign countries to locations
location_countries <- read_csv("location_country_matches.csv") %>% 
  janitor::clean_names()

data <- data %>% 
  left_join(location_countries, by=c("citation", "location")) %>% 
  rename(iso3 = iso3_alpha_code)

# Assign low-, middle-, high-income country to each row
data <- data %>% 
  left_join(country_incomes, by=c("iso3"="code")) 


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
# data %>% 
#   select(age_end) %>% 
#   unique() %>% 
#   arrange(desc(age_end))

# arrange(unique(select(data, age_end)), desc(age_end))

data <- data %>% 
  mutate(age = 0)

for (i in 1:99) {
  data <- data %>% 
    bind_rows(
      mutate(data, age = i)
    )
  print(i)
}

data <- data %>% 
  mutate(
    in_range = age >= age_start & age <= age_end,
    old = age > age_old
  )

# Calculate proportion of population by age by country

population = read_excel("population.xlsx", skip = 16, guess_max = 2000)

population <- population %>%
  filter(`1` != "...") %>% 
  mutate(across(`0`:`100+`, parse_number)) %>%
  mutate(`99` = `99` + `100+`) %>% 
  pivot_longer(`0`:`99`, names_to = "age", values_to = "pop") %>% 
  mutate(pop = pop * 1000) %>% 
  janitor::clean_names() %>% 
  select(country = region_subregion_country_or_area, iso3 = iso3_alpha_code, age, pop)

# Scale sample sizes down by percent of country population that is that age
data <- data %>% 
  left_join(population, by=c("iso3","age")) %>% 
  filter(in_range) %>% 
  group_by(iso3) %>% 
  mutate(pop_in_study_range = sum(pop)) %>% 
  ungroup() %>% 
  mutate(age_sample = sample_size * pop / pop_in_study_range)

# Sum up population by buckets (income group X age range)
population %>% 
  left_join(country_incomes, by=c("iso3"="code")) %>% 
  mutate(old = age > age_old) %>% 
  group_by(income_group, old) %>% 
  summarise(pop = sum(pop))

# Sum up sample size by same buckets
data %>% 
  group_by(income_group, old) %>% 
  summarise(pop = sum(pop))

# Divide
