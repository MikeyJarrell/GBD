library(tidyverse)
library(readxl)

income_df <- read_excel("Country Incomes.xlsx")

income_df %>%
    filter(startsWith(Economy, "Som"))

unicef_df <- read_csv("UNICEF child pop.csv")

unicef_df <- unicef_df %>%
    janitor::clean_names() %>%
    filter(str_starts(indicator_indicator, "DM_POP_U18")) %>%
    filter(str_starts(time_period_time_period, "202"))

unicef_df %>%
    write_csv("UNICEF child pop.csv")

df <- unicef_df %>% 
    filter(str_starts(indicator_indicator, "DM_POP_U18")) %>%
    filter(sex_sex == "_T: Total") %>%
    select(-c(indicator_indicator, sex_sex, dataflow)) %>%
    select(-c(obs_status_observation_status:custodian_custodian)) %>%
    select(-c(time_period_method_time_period_activity_related_to_when_the_data_are_collected:age_current_age)) %>%
    mutate(
        country = str_sub(ref_area_geographic_area, 1, 3)
    ) %>%
    select(-c(unit_measure_unit_of_measure)) %>%
    filter(!str_starts(ref_area_geographic_area, "UNICEF")) %>%
    select(-ref_area_geographic_area) %>%
    select(-unit_multiplier_unit_multiplier) %>%
    mutate(child_pop = as.numeric(obs_value_observation_value) * 1000) %>%
    filter(str_starts(time_period_time_period, "202")) %>%
    group_by(country) %>%
    arrange(desc(time_period_time_period), .by_group = TRUE) %>%
    slice_head(n = 1) %>%
    left_join(income_df, by = c("country" = "Code")) %>%
    select(year = time_period_time_period, country, child_pop, country_name = Economy, income = `Income group`)


df %>%
    filter(income %in% c("Upper middle income", "Lower middle income", "Low income")) %>%
    ungroup() %>%
    summarise(childpop = sum(child_pop))
