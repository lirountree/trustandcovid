dat <- read.csv("~/Desktop/GSRA/Trust paper/Organized (clean) dataframes/Regression dataframe.csv", check.names=FALSE)

questions <- paste0("Q", 57:89)

int_qs <- paste0("Q", c(81, 82, 83, 87)) 
dom_qs <- paste0("Q", c(70:74, 76)) 
who_qs <- "Q88"
uni_qs <- "Q75"
med_qs <- paste0("Q", 66:67)
com_qs <- paste0("Q", 58:60)
str_qs <- paste0("Q", 61:63)

subdomains <- list(international=int_qs, domestic=dom_qs, 
                   who=who_qs, university=uni_qs, 
                   media=med_qs, community=com_qs, strangers=str_qs)

get_z_star <- function(qs){
  if (length(qs) == 1) {
    z <- dat[,qs]
  }
  else {
    z <- apply(dat[,qs], 1, sum) 
  }
  z_star <- 100*(z-length(qs))/(4*(length(qs)) - length(qs))
  return(z_star)
}

results <- sapply(subdomains, get_z_star)

df <- data.frame(country=dat$Country, results, gdp=dat$GDP, life_expectancy=dat$Life_expectancy,
                 education = dat$Education, urban = dat$Urbanicity, freedom_score = dat$Freedom_Score,
                 deaths_million=dat$Deaths_million,
                 vax_rate=dat$Vaccination_rate, excess_million=dat$Excess_per_million)

write.csv(df, "~/Desktop/GSRA/Trust paper/Organized (clean) dataframes/Subdomain dataframe.csv", row.names=FALSE)

# Analysis then follows steps outlined as outlined for single items in other files.
