library(tidyverse) ; library(car)

set.seed(2024)

dat <- read.csv("Regression dataframe.csv", check.names=FALSE)
covariates <- colnames(dat[,2:34])
outcomes <- c('Deaths_million', 'Vaccination_rate', 'Excess_per_million')
confounders <- c("GDP", "Life_expectancy", "Education", "Urbanicity", "Freedom_Score")
qdat <- dat

# Single item regression

irrel <- c("Q64", "Q65", "Q68", "Q69", "Q77", "Q78", "Q80", "Q84", "Q85","Q89")
questions <- covariates[! covariates %in% irrel]

item_regs <- list()

for (outcome in outcomes){
  holds <- list()
  for (question in questions){
    model <- lm(reformulate(c(question, confounders), outcome), data=qdat)
    holds[[question]] <- c(question, round(summary(model)$coefficients[2,1], 2),
                           paste0("(", paste( as.character(round(confint(model)[2,], 2)), collapse=", "), ")"),
                           formatC(summary(model)$coefficients[2, 4], format = "g", digits = 2))
  }
  holds2 <- data.frame(do.call(rbind, holds), row.names = NULL)
  colnames(holds2 ) <- c("Question", "Estimate", "95% Confidence Interval", "p-value")
  
  item_regs[[outcome]] <- holds2
}

gt::gt(item_regs[[3]]) %>%
  gt::tab_header(
    title = gt::md("**Single item regression for excess rate**")
  ) %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels()
  )# %>%
gt::gtsave(filename = "~/Desktop/GSRA/Figures_New analysis/Question regression excess.docx")


deaths <- item_regs[[1]]
vax <- item_regs[[2]]
excess <- item_regs[[3]]