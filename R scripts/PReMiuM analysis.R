library(jsonlite) ; library(tidyverse) ; library(gt) ; library(coda)

# Actual data analysis:

dat <- read.csv("~/Desktop/GSRA/Organized (clean) dataframes/Regression dataframe.csv", check.names=FALSE)
irrel <- c("Q64", "Q65", "Q68", "Q69", "Q77", "Q78", "Q80", "Q84", "Q85","Q89")
confounders <- c("GDP", "Life_expectancy", "Education", "Urbanicity", "Freedom_Score")
covariates <- paste0("Q", 57:89)
questions <- covariates[! covariates %in% irrel]

sig_ds <- c("Q71", "Q66", "Q70", "Q67", "Q73", "Q72", "Q74", "Q87", "Q88", "Q79")
sig_vs <- c("Q73", "Q76", "Q72", "Q83", "Q88")
sig_es <- c( "Q70", "Q88", "Q83", "Q59", "Q74", "Q67", "Q86", "Q82", "Q66", "Q87")

excess <- fromJSON('~/Desktop/premium/excess/excess.json')
vax <- fromJSON('~/Desktop/premium/vax/vax.json')
death <- fromJSON('~/Desktop/premium/death/death.json')

dat$excess.clustering <- excess$clustering
dat$vax.clustering <- vax$clustering
dat$death.clustering <- death$clustering

paste0(dat[dat$excess.clustering=="2", "Country"], collapse=", ") ; length(dat[dat$excess.clustering=='2', "Country"])

columns <- c("Mean", "Lower", "Upper")
rows <- sig_es[order(sig_es)]

tapply(dat$Vaccination_rate, dat$vax.clustering, mean)

clust1 <- round(data.frame(death.clustering$cluster1, row.names = rows),2) ; colnames(clust1) <- columns
clust2 <- round(data.frame(death$cluster2, row.names = rows),2) ; colnames(clust2) <- columns

clust1

holds1 <- c()
for (i in 1:length(rows)){
  holds1[i] <- paste0(clust1[i,][[1]]," ", "(", clust1[i,][[2]], ", ", clust1[i,][[3]],")")
}

holds2 <- c()
for (i in 1:length(rows)){
  holds2[i] <- paste0(clust2[i,][[1]]," ", "(", clust2[i,][[2]], ", ", clust2[i,][[3]],")")
}

results <- data.frame(cbind(holds1, holds2), row.names = rows) ; colnames(results) <- c("High trust", "Low trust")
results <- results[rows,]
avgs <- data.frame(t(round(excess$empiricals, 2)))
for (confounder in confounders) {avgs <- rbind(avgs, round(tapply(dat[,confounder], dat$excess.clustering, mean), 2))}
rownames(avgs) <- c("Average outcome", confounders)
colnames(avgs) <- c("High trust", "Low trust")
ns <- data.frame(t(summary(factor(excess$clustering))))
rownames(ns) <- "Number of countries" ; colnames(ns) <- c("High trust", "Low trust")

gt(rbind(results, avgs, ns), rownames_to_stub=TRUE) %>%
  tab_header(title = "Excess deaths per million (using top 10 questions)") %>% 
  tab_stubhead(label = "Cluster") %>% 
  tab_row_group(
    label = md("*Cluster information*"),
    rows = 11:17
  ) %>%
  tab_row_group(
    label = md("*Trust item scores*"),
    rows = 1:10
  ) %>%
  tab_footnote(footnote=md("*Results are reported with the posterior mean and 95% credible interval.*"),
                   location = cells_row_groups(1)) #%>%
  gtsave(filename = "~/Desktop/GSRA/Figures_New analysis/premium excess.docx")

## Vax

columns <- c("Mean", "Lower", "Upper")
rows <- sig_vs[order(sig_vs)]

clust1 <- round(data.frame(vax$cluster1, row.names = rows),2) ; colnames(clust1) <- columns
clust2 <- round(data.frame(vax$cluster2, row.names = rows),2) ; colnames(clust2) <- columns
clust3 <- round(data.frame(vax$cluster3, row.names = rows),2) ; colnames(clust3) <- columns

clust1

holds1 <- c()
for (i in 1:length(rows)){
  holds1[i] <- paste0(clust1[i,][[1]]," ", "(", clust1[i,][[2]], ", ", clust1[i,][[3]],")")
}

holds2 <- c()
for (i in 1:length(rows)){
  holds2[i] <- paste0(clust2[i,][[1]]," ", "(", clust2[i,][[2]], ", ", clust2[i,][[3]],")")
}

holds3 <- c()
for (i in 1:length(rows)){
  holds3[i] <- paste0(clust3[i,][[1]]," ", "(", clust3[i,][[2]], ", ", clust3[i,][[3]],")")
}

results <- data.frame(cbind(holds1, holds2, holds3), row.names = rows) ; colnames(results) <- c("High trust", "Low trust", "Medium trust")
results <- results[rows,]
avgs <- data.frame(t(round(vax$empiricals, 2)))
for (confounder in confounders) {avgs <- rbind(avgs, round(tapply(dat[,confounder], dat$vax.clustering, mean), 2))}
rownames(avgs) <- c("Average outcome", confounders)
colnames(avgs) <- c("High trust", "Low trust", "Medium trust")
ns <- data.frame(t(summary(factor(vax$clustering))))
rownames(ns) <- "Number of countries" ; colnames(ns) <- c("High trust", "Low trust", "Medium trust")

gt(rbind(results, avgs, ns), rownames_to_stub=TRUE) %>%
  tab_header(title = "Excess deaths per million (using top 10 questions)") %>% 
  tab_stubhead(label = "Cluster") %>% 
  tab_row_group(
    label = md("*Cluster information*"),
    rows = 6:12
  ) %>%
  tab_row_group(
    label = md("*Trust item scores*"),
    rows = 1:5
  ) %>%
  tab_footnote(footnote=md("*Results are reported with the posterior mean and 95% credible interval.*"),
               location = cells_row_groups(1)) %>%
  gtsave(filename = "~/Desktop/GSRA/Figures_New analysis/premium vax.docx")

##

zs <- lapply(strsplit(readLines('~/Desktop/premium/excess/output_z.txt'), " "), as.numeric) # set WD
clust_df <- c()

for (i in 1:length(zs)){
  df <- data.frame(cbind(dat[,c('Country', confounders, 'Excess_per_million')], z=(zs[[i]])))
  clust_df[i] <- mean(df[df$z==0,"Excess_per_million"]) - mean(df[df$z==1,"Excess_per_million"])
}

mean(abs(clust_df), na.rm=TRUE) ; HPDinterval(as.mcmc(abs(clust_df)), prob=0.95)

# This, alongside silhouette width, will be manually added into the final table
