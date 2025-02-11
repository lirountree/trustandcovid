library(countrycode)
library(tidyverse)

# Confirmed COVID-19 deaths per million:

df <- read.csv("COVID Data/time_series_covid19_deaths_global.csv") %>% select(Country.Region, Province.State, X1.1.23)
df[df$Country.Region=='Taiwan*',]$Country.Region <- 'Taiwan'
deaths <- aggregate(df$X1.1.23, list(df$Country.Region), sum)
colnames(deaths) <- c("Country", "Total_deaths")

deaths$Country <- countrycode(deaths$Country, 'country.name', "iso3c")

pop1 <- read.csv("COVID Data/API_SP.POP.TOTL_DS2_en_csv_v2_79.csv", header=FALSE)[-c(1,2),] %>%
  select(V2, V67) 
pop <- pop1[c(-1),]
colnames(pop) <- c("Country", "Population") # 2022 population
taiwan <- c("TWN", 23349666)
pop <- rbind(pop, taiwan)

df2 <- merge(deaths, pop, by.x='Country')

df2$Deaths_million <- (df2$Total_deaths / as.numeric(df2$Population))*1000000

write.csv(df2, "COVID Data/JHU Total Deaths 2020-2022.csv", row.names=FALSE)

# Excess deaths per million:

df <- read.csv("COVID Data/excess_summarized.csv", check.names=FALSE) %>% 
  select(Country, est, WHO_region)

pop <- read.csv("COVID Data/JHU Total Deaths 2020-2022.csv", check.names=FALSE) %>%
  select(Country, Population)
#pop$Country <- countrycode(pop$Country, 'iso3c', 'country.name')

excess <- c()

for (country in unique(df$Country)){
  excess[country] <- sum(df[df$Country==country,]$est)
}

df2 <- na.omit(data.frame(Country=countrycode(names(excess), 'country.name', 'iso3c'), Excess=unname(excess)))[!duplicated(df2$Country),]

data <- merge(df2, pops, by="Country")

taiwan <- read.csv("COVID Data/taiwan_excess_deaths.csv", check.names=FALSE)

data <- merge(df2, pop, by="Country")

data$Country <- countrycode(data$Country, 'iso3c', 'country.name')

data$Excess_per_million <- (data$Excess / data$Population)*1000000

get_metric <- function(df){
  states <- unique(df$country)
  excess_deaths_per_million <- c()
  
  for (state in states){
    count <- df %>%
      filter(country==state, end_date <="2022-01-01")
    
    dat <- (sum(count$excess_deaths)/unique(count$population))*1000000
    excess_deaths_per_million[state] <- dat
  }
  
  excess_deaths_per_million
}

get_metric(taiwan)

taiwan2 <- c("Taiwan", 1, 1, unname(get_metric(taiwan)))
data <- rbind(data, taiwan2)

write.csv(data[,c("Country", "Excess_per_million")], "COVID Data/WHO Excess deaths 2020-2021.csv", row.names=FALSE)
