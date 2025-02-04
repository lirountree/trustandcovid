# Confounder analysis

library(tidyverse) ; set.seed(2024) ; library(countrycode)

dat <- read.csv("~/Desktop/GSRA/Organized (clean) dataframes/Regression dataframe.csv", check.names=FALSE)
countries <- countrycode(dat$Country, "country.name", "iso3c")

school <- read.csv("~/Desktop/GSRA/Confounders Data/average-years-of-schooling/average-years-of-schooling.csv") %>% 
  filter(Year == 2019, Code %in% countries) %>% select(2,4) # Just Taiwan is missing
school <- rbind(school, c("TWN", 12.3))
# Taiwan: 12.3 years https://stats.moe.gov.tw/files/Statistical%20Indicators/index_eng.pdf
# From our world in data

urban <- read.csv("~/Desktop/GSRA/Confounders Data/share-of-population-urban/share-of-population-urban.csv") %>% 
  filter(Year == 2019, Code %in% countries) %>% select(2,4) # Taiwan missing :(
urban <- rbind(urban, c("TWN", 83.2))
# Taiwan: 83.2% urban https://www.worldometers.info/demographics/taiwan-demographics/

govt <- read.csv("~/Desktop/GSRA/Confounders Data/freedom.csv", skip=1) %>%  
  filter(Edition == 2019) %>% select(1,44)
govt$Country.Territory <- countrycode(unique(govt$Country.Territory), "country.name", "iso3c")
govt <- govt %>% filter(Country.Territory %in% countries) %>% distinct(Country.Territory, .keep_all = TRUE)
colnames(govt) <- c("Code", "Freedom_Score")

confound <- list(school, urban, govt) %>% reduce(left_join, by='Code')
colnames(confound) <- c("Country", "Education", "Urbanicity", "Freedom_Score")
confound$Country <- countrycode(confound$Country, "iso3c", "country.name")

new_dat <- left_join(dat, confound, by="Country")

questions <- colnames(new_dat)[2:34]

new_dat <- new_dat[,c("Country", questions, "GDP", "Life_expectancy", "Education", 
           "Urbanicity", "Freedom_Score", "Deaths_million", "Vaccination_rate",
           "Excess_per_million")]

write.csv(new_dat, "~/Desktop/GSRA/Organized (clean) dataframes/Regression dataframe.csv", row.names=FALSE)
