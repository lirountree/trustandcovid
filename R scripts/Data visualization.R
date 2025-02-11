library(tidyverse) ; library(ggpubr) ; library(reshape2) ; library(gtsummary)
library(countrycode) ; library(Hmisc)

dat <- read.csv("Regression dataframe.csv", check.names=FALSE)
dat$GDP <- dat$GDP*1000

covariates <- colnames(dat[,2:34])
outcomes <- c("COVID-19 deaths per million", "Vaccination rate", "Excess deaths per million")
confounders <- c("GDP", "Life expectancy", "Education", "Urbanicity", "Freedom Score")

colnames(dat)[35:39] <- confounders
colnames(dat)[40:42] <- outcomes

irrel <- c("Q64", "Q65", "Q68", "Q69", "Q77", "Q78", "Q80", "Q84", "Q85","Q89")
questions <- covariates[! covariates %in% irrel]

### Violin plots: Distributions of trust scores

ggplot(melt(dat[,questions]), aes(x=factor(variable), y=value, fill=variable)) + 
  geom_violin() +
  geom_boxplot(width=0.1, fill= 'white') +
  labs(title = "Distribution of trust items across countries", 
       x = "Trust item", y="Trust score") +
  guides(fill=guide_legend(title="Trust subdomain")) + theme(legend.position='none')


## Correlation matrices

heatmap <- melt(rcorr(as.matrix(dat[,c(questions, confounders, outcomes)]), type='pearson')$r)

pvals <- melt(rcorr(as.matrix(dat[,c(questions, confounders, outcomes)]), type='pearson')$P)
pvals$value[is.na(pvals$value)] <- 1

holds <- c()

for (i in 1:length(pvals$value)){
  if (pvals$value[i] < 0.05) {holds[i] <- "*"}
  else {holds[i] <- ""}}

ggplot(heatmap, aes(x = Var1, y = Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(high = "lightpink2", low="lightblue2", mid='white') +
  guides(fill = guide_colourbar(title = "Correlation")) +
  labs(x = "", y = "", 
       title = "Heatmap of Correlations: Covariates, Confounders, Outcomes") +
  scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  geom_text(label = paste0(round(heatmap$value,2), holds), size=3)

### Summary table

covar <- tbl_summary(dat[,questions], statistic = list(
    all_continuous() ~ "{mean} ({sd})"),
    digits = all_continuous()~2) %>%
  modify_header(label = "**Summary of data (N=61)**", stat_0="**Mean (SD)**")

confound <- tbl_summary(dat[,confounders], statistic = list(
  all_continuous() ~ "{mean} ({sd})"),
  digits = all_continuous()~2) %>%
  modify_header(label = "**Confounder**", stat_0="**Average**")

outcomes <- tbl_summary(dat[,outcomes], statistic = list(
  all_continuous() ~ "{mean} ({sd})"),
  digits = all_continuous()~2) %>%
  modify_header(label = "**Confounder**", stat_0="**Average**")

tbl_stack(list(covar, confound, outcomes),
          group_header=c('Covariates', 'Confounders', 'Outcomes')) %>%
  modify_footnote(c(all_stat_cols(), stat_0) ~ NA) %>%
  as_gt() %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups(groups = everything())
  ) %>%
    gt::gtsave(filename = "Summary table.docx")
  
# Distribution of question trust scores

df <- read.csv("Wave 7 Q57-89 Median Imputation.csv",
               check.names=FALSE)
df$Country <- countrycode(df$Country, origin='iso3c', 'country.name')
qdat <- df[df$Country %in% dat$Country,]

ggplot(melt(qdat), aes(x=variable, y=value, fill=variable)) + geom_boxplot() +
  geom_boxplot() + theme(legend.position="none") +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "white") +
  xlab("Question") + ylab("Trust score") +
  labs(title="Distribution of country-level question trust scores")

ggplot(melt(cor(qdat[,-1])), aes(x = Var1, y = Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(high = "lightpink2", low="lightblue2", mid='white') +
  guides(fill = guide_colourbar(title = "Correlation")) +
  labs(x = "", y = "", 
       title = "Heatmap of correlations between WVS questions") +
  scale_x_discrete(guide = guide_axis(n.dodge=3))

# Matrix of missingness

questions <- paste0("Q", 57:89)

inputs <- c()
for (question in questions){inputs[question] <- paste0("Wave 7 csvs/Wave 7 ", question, ".csv")}
countries <- data.frame(read.csv(inputs[1], check.names=FALSE)$Country)

get_vecs_missing <- function(input){vec <- data.frame(read.csv(input)) ; vec}
vectors_m <- lapply(inputs, get_vecs_missing)

get_missing_matrix <- function(vec){
  vec <- data.frame(vec)
  vec$X..Missingness[is.na(vec[,2]) == TRUE] <- 100
  miss <- data.frame(vec[,4])
  colnames(miss) <- colnames(vec)[2]
  miss
}

clean_dat <- lapply(vectors_m, get_missing_matrix)
clean <- bind_cols(countries, clean_dat)
colnames(clean) <- c("Country", questions) 
clean$Country <- countrycode(clean$Country, 'iso3c', 'country.name')
clean <- clean[clean$Country %in% dat$country,]

ggplot(melt(clean), aes(x=variable, y=Country, fill=value)) + 
  geom_tile() +
  guides(fill = guide_colourbar(title = "% Missing")) +
  labs(title = "Individual item non-response rate across countries", x = "Question")

# Question correlation matrix

heatmap2 <- melt(rcorr(as.matrix(qdat[,-1]), type='pearson')$r)

pvals2 <- melt(rcorr(as.matrix(qdat[,-1]), type='pearson')$P)
pvals2$value[is.na(pvals2$value)] <- 1

holds2 <- c()

for (i in 1:length(pvals2$value)){
  if (pvals2$value[i] < 0.05) {holds2[i] <- "*"}
  else {holds2[i] <- ""}}

ggplot(heatmap2, aes(x = Var1, y = Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(high = "lightpink2", low="lightblue2", mid='white') +
  guides(fill = guide_colourbar(title = "Correlation")) +
  labs(x = "", y = "", 
       title = "Heatmap of correlations between WVS questions") +
  scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  geom_text(label = paste0(round(heatmap2$value,2), holds2), size=2.5)
