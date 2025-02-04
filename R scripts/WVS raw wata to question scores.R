# Extracting all questions

library(tidyverse)
library(countrycode)

dat <- read.csv("~/Desktop/GSRA/WVS Data/WVS_Cross-National_Wave_7_csv_v6_0.csv")

# Want questions 58:89 (share ordinal structure)

questions <- paste0("Q", 58:89)

data <- select("A_WAVE", "A_YEAR", "B_COUNTRY_ALPHA", all_of(questions),
               .dat=dat)

# Turning this into a list of country-specific data:

countries <- unique(data$B_COUNTRY_ALPHA)
countrynames <- replace_na(countrycode(countries, "iso3c", "country.name.en"), "Northern Ireland")

countrydats <- lapply(countries, function(country) filter(data, B_COUNTRY_ALPHA == country))

# Building the functions to extract the data in the format I want. 

get_counts <- function(q, countrydat){
  nonresponses <- sum(countrydat[,q] == -1) + sum(countrydat[,q] == -2) 
  missing <- sum(countrydat[,q] == -4) + sum(countrydat[,q] == -5) 
  question <- c(sum(countrydat[,q] == 1), sum(countrydat[,q] == 2), nonresponses,
                sum(countrydat[,q] == 3), sum(countrydat[,q] == 4), missing, 
                length(countrydat[,q]))
  question
} # Counts individual responses for each question

get_count_dfs <- function(country){
  step <- data.frame(sapply(questions, get_counts, country))
  
  Country <- rep(unique(country[["B_COUNTRY_ALPHA"]]), dim(step)[1])
  
  Information <- c(1, 2, 0, 3, 4, NA, "Total") # Where 0 is our non-response
  
  counts_df <- cbind(Information, Country, step)
  counts_df
} # Applies across countries

dataframes <- lapply(countrydats, get_count_dfs)

# For one question:

question <- "Q58"

get_score <- function(nation, question){
  info <- select(nation, question)[,1]
  n_adj <- info[7] - (info[6]+info[3])
  
  p1 <- info[1]/n_adj
  p2 <- info[2]/n_adj
  p3 <- info[4]/n_adj
  p4 <- info[5]/n_adj
  
  c(unique(nation$Country), (p1*4+p2*3+p3*2+p4*1), n_adj, ((info[3])/info[7])*100)
}

country_results <- lapply(dataframes, get_score, question)
results_df <- t(data.frame(country_results))
rownames(results_df) <- 1:length(country_results)
colnames(results_df) <- c("Country", "Score", "n Adjusted", "% Did Not Answer")

# Dataframe of country responses for one question, including information on missingness.

naming <- function(name){
  named <- countrycode(name, "iso3c", "country.name.en")
  if (is.na(named)==TRUE){
    named <- name}
  named
}

input <- paste0("~/Desktop/GSRA/Wave 7 Questions/Wave 7 ", question, ".csv")

write.csv(results_df, input) # Testing whether it works for one.

# Now to  make this whole crazy thing a function:

question_dfs <- function(question){
  country_results <- lapply(dataframes, get_score, question)
  
  results_df <- t(data.frame(country_results))
  rownames(results_df) <- 1:length(country_results)
  colnames(results_df) <- c("Country", question, "n Adjusted", "% Missingness")
  
  input <- paste0("~/Desktop/GSRA/Wave 7 csvs/Wave 7 ", question, ".csv")
  
  write.csv(results_df, input, row.names=FALSE)
}

lapply(questions, question_dfs)

# Check:
read.csv("~/Desktop/GSRA/Wave 7 csvs/Wave 7 Q88.csv", check.names=FALSE)

# Huzzah!

# For Question 57:

question <- "Q57"

data <- select("A_WAVE", "A_YEAR", "B_COUNTRY_ALPHA", question,
               .dat=dat)

# Turning this into a list of country-specific data:

countries <- unique(data$B_COUNTRY_ALPHA)
countrynames <- replace_na(countrycode(countries, "iso3c", "country.name.en"), "Northern Ireland")

countrydats <- lapply(countries, function(country) filter(data, B_COUNTRY_ALPHA == country))

# Building the functions to extract the data in the format I want. 

get_counts <- function(q, countrydat){
  nonresponses <- sum(countrydat[,q] == -1) + sum(countrydat[,q] == -2) 
  missing <- sum(countrydat[,q] == -4) + sum(countrydat[,q] == -5) 
  
  question <- c(sum(countrydat[,q] == 1), sum(countrydat[,q] == 2), 
                nonresponses,
                missing, 
                length(countrydat[,q]))
  question
}

get_count_dfs <- function(country){
  step <- data.frame(sapply(question, get_counts, country))
  
  Country <- rep(unique(country[["B_COUNTRY_ALPHA"]]), dim(step)[1])
  
  Information <- c(1, 2, 0, NA, "Total")
  
  counts_df <- cbind(Information, Country, step)
  counts_df
}

dataframes <- lapply(countrydats, get_count_dfs)

get_score <- function(nation, question){
  info <- select(nation, question)[,1]
  n_adj <- info[5] - (info[4]+info[3])
  
  p1 <- info[1]/n_adj
  p2 <- info[2]/n_adj
  
  c(unique(nation$Country), (p1*2+p2*1), n_adj, ((info[3])/info[5])*100)
}

country_results <- lapply(dataframes, get_score, question)

results_df <- t(data.frame(country_results))
rownames(results_df) <- 1:length(country_results)
colnames(results_df) <- c("Country", question, "n Adjusted", "% Missingness")

naming <- function(name){
  named <- countrycode(name, "iso3c", "country.name.en")
  if (is.na(named)==TRUE){
    named <- name}
  named
}

rownames(results_df) <- NULL
df2 <- data.frame(results_df)

colnames(df2) <- c("Country", question, "n Adjusted", "% Missingness")

input <- paste0("~/Desktop/GSRA/Wave 7 csvs/Wave 7 ", question, ".csv")

write.csv(df2, input, row.names=FALSE)

df <- read.csv("~/Desktop/GSRA/Wave 7 csvs/Wave 7 Q57.csv", check.names=FALSE)

# Now to get complete dataset with median imputation:

questions <- paste0("Q", 57:89)

inputs <- c()
for (question in questions){
  inputs[question] <- paste0("~/Desktop/GSRA/Wave 7 csvs/Wave 7 ", question, ".csv")
}

impute_median <- function(i){
  df <- read.csv(inputs[i], check.names=FALSE)
  df[,questions[i]][df$`% Missingness`>40 | is.na(df[,questions[i]]) == TRUE] <- median(df[df$`% Missingness`<40,][,questions[[i]]], na.rm=TRUE)
  df[,questions[i]]
}

countries <- read.csv(inputs[1])$Country

dat <- data.frame(countries, sapply(1:length(inputs), impute_median))
colnames(dat) <- c("Country", questions)

write.csv(dat, "~/Desktop/GSRA/Organized (clean) dataframes/Wave 7 Q57-89 Median Imputation.csv", row.names=FALSE)
