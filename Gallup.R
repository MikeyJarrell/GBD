setwd("/Users/mikey/Downloads/")
pacman::p_load(data.table, dplyr, naniar, ggplot2, viridis, cowplot)
data = data.table(read.csv("Gallup.csv"))

data$depressed = (1-(data$MH7A-1)) # data[, depressed := fifelse(MH7A == 1, 0, fifelse(MH7A == 2, 1, NA))]
summary = data %>%
  filter(depressed != -97) %>%
  group_by(COUNTRYNEW)  %>% 
  summarise(weighted_depression = weighted.mean(depressed, WGT, na.rm=TRUE))  
print(summary, n=113)

summary_old = data %>%
  filter(depressed != -97) %>%
  filter(Age >= 55) %>%
  group_by(COUNTRYNEW)  %>% 
  summarise(weighted_depression = weighted.mean(depressed, WGT, na.rm=TRUE))
summary_old2 =  data[depressed != -97 & Age >= 55, weighted.mean(depressed, WGT, na.rm = TRUE) , COUNTRYNEW]
print(summary_old, n=113)

global_old = data %>%
  filter(depressed != -97) %>%
  filter(Age >= 55) %>%
  summarise(weighted_depression = weighted.mean(depressed, PROJWT, na.rm=TRUE))  
global_old

global = data %>%
  filter(depressed != -97) %>%
  summarise(weighted_depression = weighted.mean(depressed, PROJWT, na.rm=FALSE))  
global



country_incomes <- data.table(read_excel("Country Incomes.xlsx")) %>%
  janitor::clean_names()


# Change a stupid variable name
setnames(country_incomes, "economy", "country")
country_incomes[code == "VEN", income_group := "Lower middle income"]
country_incomes[, income_group_simple := fifelse(income_group == "High income", "HIC", "LMIC")]

country_incomes[
  code == "BIH", country := "Bosnia Herzegovina"
  ][
  code == "COG", country := "Congo Brazzaville"
  ][
  code == "EGY", country := "Egypt"
  ][
  code == "HKG", country := "Hong Kong"
  ][
  code == "IRN", country := "Iran"
  ][
  code == "CIV", country := "Ivory Coast"
  ][
  code == "KGZ", country := "Kyrgyzstan"
  ][
  code == "LAO", country := "Laos"
  ][
  code == "RUS", country := "Russia"
  ][
  code == "SVK", country := "Slovakia"
  ][
  code == "KOR", country := "South Korea"
  ][
  code == "TWN", country := "Taiwan"
  ][
  code == "TUR", country := "Turkey"
  ][
  code == "VEN", country := "Venezuela"
  ]

# Assign low-, middle-, high-income country to each row
data <- data %>%
  left_join(country_incomes, by = c("COUNTRYNEW" = "country"))

age_bin_width = 10

setDT(data)

data[
  ,
  age_bin := Age %/% age_bin_width
][
  ,
  age_bin_name := paste0(age_bin * age_bin_width, "-", (age_bin + 1) * age_bin_width - 1)
]

data[Age < 20, age_bin_name := "<20"]
data[Age >= 80, age_bin_name := "80+"]

setkey(data, age_bin)

# suspicious of 99s and 100s
to_plot = data[Age < 99 & depressed != -97, .(depressed = weighted.mean(depressed, PROJWT, na.rm = TRUE), age_bin = first(age_bin)), .(income_group_simple, age_bin_name)]
to_plot[, age_bin_factor := factor(age_bin_name, ordered = TRUE)]

ggplot(to_plot) + 
  geom_line(aes(x = age_bin_factor, y = depressed, color = income_group_simple, group = income_group_simple), size = 1) +
  scale_colour_viridis(discrete = TRUE, direction = -1, end = 0.7, option = "inferno") +
  theme_cowplot() +
  xlab("Age") +
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  expand_limits(y = 0) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)), breaks = seq(0, 0.25, 0.05)) +
  ggtitle("Share Depressed")


ggsave("DepressionByIncome.pdf")