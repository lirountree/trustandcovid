library(PReMiuM) ; library(jsonlite) ; library(tidyverse) ; library(coda)

# Obtaining data for analysis:

dat <- read.csv("Regression dataframe.csv", check.names=FALSE)
covariates <- colnames(dat[,2:34])
outcomes <- c('Deaths_million', 'Vaccination_rate', 'Excess_per_million')
confounders <- c("GDP", "Life_expectancy", "Education", "Urbanicity", "Freedom_Score")

# Selecting top 10/15 questions:

irrel <- c("Q64", "Q65", "Q68", "Q69", "Q77", "Q78", "Q80", "Q84", "Q85","Q89")
questions <- covariates[! covariates %in% irrel]

item_regs <- list()

for (outcome in outcomes){
  holds <- list()
  for (question in questions){
    model <- lm(reformulate(c(question, confounders), outcome), data=dat)
    holds[[question]] <- c(question, round(summary(model)$coefficients[2,1], 2),
                           paste0("(", paste( as.character(round(confint(model)[2,], 2)), collapse=", "), ")"),
                           round(summary(model)$coefficients[2, 4], 4))
  }
  holds2 <- data.frame(do.call(rbind, holds), row.names = NULL)
  colnames(holds2 ) <- c("Question", "Estimate", "95% Confidence Interval", "p-value")
  
  item_regs[[outcome]] <- holds2
}

deaths <- item_regs[[1]] %>% arrange(`p-value`, Estimate)
vax <- item_regs[[2]]  %>% arrange(`p-value`, Estimate)
excess <- item_regs[[3]]  %>% arrange(`p-value`, Estimate)

sig_ds <- deaths %>%
  arrange(`p-value`, Estimate) %>% head(10) %>% select(Question) %>% pull

sig_vs <- vax %>%
  arrange(`p-value`, Estimate) %>% head(5) %>% select(Question) %>% pull

sig_es <- excess %>%
  arrange(`p-value`, Estimate) %>% head(10) %>% select(Question) %>% pull

### Using these top questions...

centers <- attr(scale(dat[,-1]), 'scaled:center')
scales <- attr(scale(dat[,-1]), 'scaled:scale')

dat[,-1] <- scale(dat[,-1])

dat_list <- list()

dat_list$data <- dat
dat_list$covNames <- sig_ds
dat_list$xModel <- "Normal"
dat_list$yModel <- "Normal"
dat_list$nCovariates <- length(sig_ds)
dat_list$fixedEffectNames <- confounders

# Below is done for each of the three outcomes

setwd('premium/death') 
outputs <- profRegr(yModel=dat_list$yModel,
                    xModel=dat_list$xModel,
                    outcome="Deaths_million",
                    nSweeps=10000, nClusInit=2,
                    nBurn=5000, data=dat_list$data, output="output",
                    covNames = dat_list$covNames,
                    fixedEffectsNames = dat_list$fixedEffectNames, seed = '2024'
                    )
# Getting output

dissimObj <- calcDissimilarityMatrix(outputs)
clusObj <- calcOptimalClustering(dissimObj) ; clusObj$clusterSizes

riskProfileObj <- calcAvgRiskAndProfile(clusObj)
#clusterOrderObj <- plotRiskProfile(riskProfileObj, "summary-sim.png") 

# Output analysis

covariates <- sig_vs

risks <- riskProfileObj$risk

profiles <- riskProfileObj$profile

cluster1 <- list()

covs1 <- list()

for (i in 1:length(covariates)){
  covariate <- as.mcmc(profiles[,1,i]*scales[i] + centers[i], start=1, end=1000)
  cluster1[[i]] <- c(Mean=mean(covariate), Lower=HPDinterval(covariate, prob=0.95)[1], 
                     Upper=HPDinterval(covariate, prob=0.95)[2])
  covs1[[i]] <- covariate
}

cluster1df <- t(data.frame(cluster1))
rownames(cluster1df) <- covariates

covs1df <- data.frame(covs1)
colnames(covs1df) <- covariates

cluster2 <- list()
covs2 <- list()
for (i in 1:length(covariates)){
  covariate <- as.mcmc(profiles[,2,i]*scales[i] + centers[i], start=1, end=1000)
  cluster2[[i]] <- c(Mean=mean(covariate), Lower=HPDinterval(covariate, prob=0.95)[1], 
                     Upper=HPDinterval(covariate, prob=0.95)[2])
  covs2[[i]] <- covariate
}

cluster2df <- t(data.frame(cluster2))
rownames(cluster2df) <- covariates

covs2df <- data.frame(covs2)
colnames(covs2df) <- covariates

cluster3 <- list()
covs3 <- list()
for (i in 1:length(covariates)){
  covariate <- as.mcmc(profiles[,3,i]*scales[i] + centers[i], start=1, end=1000)
  cluster3[[i]] <- c(Mean=mean(covariate), Lower=HPDinterval(covariate, prob=0.95)[1], 
                     Upper=HPDinterval(covariate, prob=0.95)[2])
  covs3[[i]] <- covariate
}

cluster3df <- t(data.frame(cluster3))
rownames(cluster3df) <- covariates

covs3df <- data.frame(covs3)
colnames(covs3df) <- covariates

end_result <- list(cluster1 = cluster1df, cluster2 = cluster2df, cluster3=cluster3df,
     covs1 = covs1df, covs2=covs2df, covs3=covs3df,
     empiricals=riskProfileObj$empiricals*scales["Vaccination_rate"] + centers["Vaccination_rate"],
     clustering = clusObj$clustering,
     silhouette = clusObj$avgSilhouetteWidth)

write(jsonlite::toJSON(end_result, pretty = TRUE), 'vax.json')
