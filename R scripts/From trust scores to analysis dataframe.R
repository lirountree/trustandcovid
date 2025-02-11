library(tidyverse)
library(countrycode)
library(car)

# Our X

wvs <- read.csv("Wave 7 Q57-89 Median Imputation.csv", check.names=FALSE)
wvs <- wvs[!wvs$Country=='EGY',]
countrynames <- wvs$Country

# Our Y

deaths <- read.csv("COVID Data/JHU Total Deaths 2020-2022.csv", check.names=FALSE) %>% 
  select(Country, Deaths_million)

vax <- read.csv("COVID Data/vaccination-data.csv") %>% select(ISO3, PERSONS_VACCINATED_1PLUS_DOSE_PER100)
colnames(vax) <- c("Country", "Vaccination_rate")
vax[126,]$Vaccination_rate <- 94

excess <- read.csv("COVID Data/WHO Excess deaths 2020-2021.csv", check.names=FALSE)
excess$Country <- countrycode(excess$Country, 'country.name', 'iso3c')

# Our Z

gdp1 <- read.csv("Confounders Data/GDP per capita/GDP.csv", header=FALSE)[-c(1,2),] 
gdp <- gdp1 %>% select(V2, V64) %>% filter(V2 %in% countrynames) 
colnames(gdp) <- c("Country", "GDP")
gdp <- rbind(gdp, c("TWN", 34430))
gdp[gdp$Country=="VEN","GDP"] <- 3.69*1000

le1 <- read.csv("Confounders Data/WB Life expectancy/Life expectancy.csv", header=FALSE)[-c(1,2),]
le <- le1 %>% select(V2, V64) %>% filter(V2 %in% countrynames)
colnames(le) <- c("Country", "Life_expectancy")
le <- rbind(le, c("TWN", 81.6))
le[le$Country=="AND", "Life_expectancy"] <- 83.6

# Let's merge all of this data:

df_list <- list(wvs, gdp, le, deaths, vax, excess)
dat <- df_list %>% reduce(left_join, by='Country') %>% na.omit
dat$Country <- countrycode(dat$Country, "iso3c", "country.name")
dat$GDP <- as.numeric(dat$GDP)/1000

write.csv(dat, "Regression dataframe.csv", row.names=FALSE)
