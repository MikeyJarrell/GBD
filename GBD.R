pacman::p_load(data.table, readxl)
setwd("/Users/mikey/Downloads/GBD")
data = fread("GBD Data.csv")
country_incomes = data.table(read_excel("Country Incomes.xlsx"))
table(data$Location)

