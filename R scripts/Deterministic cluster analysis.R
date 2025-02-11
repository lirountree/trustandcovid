library(ggpubr) ; library(tidyverse) ; library(factoextra) ; library(mclust)

set.seed(2024)

dat <- read.csv("Regression dataframe.csv", check.names=FALSE)
covariates <- colnames(dat[,2:34])
outcomes <- c('Deaths_million', 'Vaccination_rate', 'Excess_per_million')
confounders <- c("GDP", "Life_expectancy", "Education", "Urbanicity", "Freedom_Score")

irrel <- c("Q64", "Q65", "Q68", "Q69", "Q77", "Q78", "Q80", "Q84", "Q85","Q89")
questions <- covariates[! covariates %in% irrel]

plot_df <- dat[,questions]
rownames(plot_df) <- dat$Country

# k-means clustering

ggarrange(fviz_nbclust(dat[,2:8], kmeans, method = "wss"),
         fviz_nbclust(dat[,2:8], kmeans, method = "silhouette"),
        fviz_nbclust(dat[,2:8], kmeans, method = "gap_stat")) 

# Optimal number of 3.

km.res <- kmeans(plot_df, 3, nstart = 25) 
kmeans_plot <- fviz_cluster(object=km.res, data=plot_df, 
                            main="k-means cluster plot") +
  scale_colour_manual(values = c("darkgoldenrod3", "dodgerblue2", "seagreen4")) + 
  scale_fill_manual(values = c("darkgoldenrod2", "dodgerblue", "seagreen3")) ; kmeans_plot

dat$kmeans <- km.res$cluster

# Getting numerical summaries of these clusters.

c3 <- dat[dat$kmeans==3, c(questions, confounders)]
c2 <- dat[dat$kmeans==2, c(questions, confounders)]
c1 <- dat[dat$kmeans==1, c(questions, confounders)]

trust <- data.frame(cbind(colMeans(c1), colMeans(c2), colMeans(c3)))
colnames(trust) <- c("Medium ", "High ", "Low ")
trust <- trust[, c("Low ", "Medium ", "High ")]
colMeans(trust[questions,])

# These roughly correspond to Low, Medium, and High

dat[dat$kmeans==1,]$kmeans <- "Medium"
dat[dat$kmeans==2,]$kmeans <- "High"
dat[dat$kmeans==3,]$kmeans <- "Low"

kmeans_plot <- fviz_cluster(object=list(data=plot_df, cluster=dat$kmeans),
                            main="k-means cluster plot", show.legend=FALSE,
                            check_overlap=TRUE) +
  scale_colour_manual(values = c("darkgoldenrod3", "dodgerblue2", "seagreen4"), 'Clusters') + 
  scale_fill_manual(values = c("darkgoldenrod2", "dodgerblue", "seagreen3"), 'Clusters') +
  ylab("Principal component 1") +
  xlab("Principal component 2") + theme_bw() ; kmeans_plot

# Visualizing cluster differences for outcomes.
kplottitle <- "Cluster"

kplot1 <- ggplot(dat, aes(x=factor(kmeans), y=Deaths_million, fill=factor(kmeans))) + theme_light() + 
  geom_violin() + geom_boxplot(width=0.1, fill='white') + xlab("Confirmed COVID deaths per million") +
  ylab(" ") + ggtitle("k-means clusters") +
  scale_colour_manual(values = c("darkgoldenrod3", "dodgerblue2", "seagreen4")) + 
  scale_fill_manual(kplottitle, values = c("darkgoldenrod2", "dodgerblue", "seagreen3")) 

kplot2 <- ggplot(dat, aes(x=factor(kmeans), y=Vaccination_rate, fill=factor(kmeans))) + theme_light() + 
  geom_violin() + geom_boxplot(width=0.1, fill='white') + xlab("Vaccination rate per 100 people") +
  ylab(" ") + ggtitle("k-means clusters") +
  scale_colour_manual(values = c("darkgoldenrod3", "dodgerblue2", "seagreen4")) + 
  scale_fill_manual(kplottitle, values = c("darkgoldenrod2", "dodgerblue", "seagreen3")) 

kplot3 <- ggplot(dat, aes(x=factor(kmeans), y=Excess_per_million, fill=factor(kmeans))) + theme_light() + 
  geom_violin() + geom_boxplot(width=0.1, fill='white') + xlab("Excess deaths per million") +
  ylab(" ") + ggtitle("k-means clusters") +
  scale_colour_manual(values = c("darkgoldenrod3", "dodgerblue2", "seagreen4")) + 
  scale_fill_manual(kplottitle, values = c("darkgoldenrod2", "dodgerblue", "seagreen3")) 

ggarrange(kplot1, kplot2, kplot3)

# Gaussian mixture model clustering

model <- Mclust(plot_df, G=3) # from assessing log-likelihood, BIC, ICl for G=2,3,4,5, 3 is the best.

# Assessing assignment accuracy

sum(apply(model$z, 1, max) > 0.95) / dim(dat)[1]

# Assessing cluster differences (maximum posterior probability)

dat$classes <- model$classification

gc3 <- dat[dat$classes==3, c(questions, confounders)]
gc2 <- dat[dat$classes==2, c(questions, confounders)]
gc1 <- dat[dat$classes==1, c(questions, confounders)]

g_trust <- data.frame(cbind(colMeans(gc1), colMeans(gc2), colMeans(gc3)))
colnames(g_trust) <- c("High", "Low", "Medium")
g_trust <- g_trust[, c("Low", "Medium", "High")]
colMeans(g_trust[questions,])

dat[dat$classes==1,]$classes <- "High"
dat[dat$classes==2,]$classes <- "Low"
dat[dat$classes==3,]$classes <- "Medium"

gauss_plot <- fviz_cluster(object=list(data=plot_df, cluster=dat$classes),
             main = "GMM cluster plot", show.legend=FALSE) +
  scale_colour_manual(values = c("darkgoldenrod3", "dodgerblue2", "seagreen4"), 'Classes') + 
  scale_fill_manual(values = c("darkgoldenrod2", "dodgerblue", "seagreen3"), 'Classes') +
  ylab("Principal component 1") + xlab("Principal component 2") ; gauss_plot

kplottitle <- "Class"

gplot1 <- ggplot(dat, aes(x=factor(classes), y=Deaths_million, fill=factor(classes))) + theme_light() + 
  geom_violin() + geom_boxplot(width=0.1, fill='white') + xlab("COVID-19 deaths per million") +
  ylab(" ") + ggtitle("GMM classes") +
  scale_colour_manual(values = c("darkgoldenrod3", "dodgerblue2", "seagreen4")) + 
  scale_fill_manual(kplottitle, values = c("darkgoldenrod2", "dodgerblue", "seagreen3")) 

gplot2 <- ggplot(dat, aes(x=factor(classes), y=Vaccination_rate, fill=factor(classes))) + theme_light() + 
  geom_violin() + geom_boxplot(width=0.1, fill='white') + xlab("Vaccination rate per 100 people") +
  ylab(" ") + ggtitle("GMM classes") +
  scale_colour_manual(values = c("darkgoldenrod3", "dodgerblue2", "seagreen4")) + 
  scale_fill_manual(kplottitle, values = c("darkgoldenrod2", "dodgerblue", "seagreen3")) 

gplot3 <- ggplot(dat, aes(x=factor(classes), y=Excess_per_million, fill=factor(classes))) + theme_light() + 
  geom_violin() + geom_boxplot(width=0.1, fill='white') + xlab("Excess deaths per million") +
  ylab(" ") + ggtitle("GMM classes") +
  scale_colour_manual(values = c("darkgoldenrod3", "dodgerblue2", "seagreen4")) + 
  scale_fill_manual(kplottitle, values = c("darkgoldenrod2", "dodgerblue", "seagreen3")) 

ggarrange(gplot1, gplot2, gplot3)

## Output table (deterministic)

df <- cbind(round(trust, 2), round(g_trust, 2))

gt::gt(df, rownames_to_stub = TRUE) %>%
  gt::tab_header(title = "Trust item means") %>%
  gt::tab_stubhead(label = "Question") |>
  gt::tab_spanner(
    label = "GMM classes",
    columns = c(Low, Medium, High)
  ) %>%
  gt::tab_spanner(
    label = "k-means clusters",
    columns = c(`Low `, `Medium `, `High `)
  ) %>%
  gt::tab_style(
    style = list(gt::cell_text(weight = "bold")), 
    location=list(gt::cells_stubhead(), gt::cells_title(groups='title'),
                  gt::cells_column_spanners())) %>%
  gt::tab_style(style=list(gt::cell_text(style='italic')),
                location=list(gt::cells_column_labels())) %>%
  gt::tab_footnote(footnote = "Uses maximum posterior probability for class assignment.",  
                   locations = gt::cells_column_spanners(spanners = 'GMM classes')) %>%
  gt::tab_style(style=(gt::cell_text(style='italic', size="small")),
                location=list(gt::cells_footnotes())) %>%
  gt::gtsave(filename = "Cluster subdomain table.docx")

# Regression results for GMM

cluster_vax <- lm(data=dat, Vaccination_rate ~ factor(classes) +
                    GDP + Life_expectancy + Urbanicity + Education + Freedom_Score)
cluster_death <- lm(data=dat, Deaths_million ~ factor(classes) + 
                      GDP + Life_expectancy + Urbanicity + Education + Freedom_Score)
cluster_excess <- lm(data=dat, Excess_per_million ~ factor(classes) + 
                       GDP + Life_expectancy + Urbanicity + Education + Freedom_Score)
  
vaxregk <- data.frame(rbind(c("Low trust", round(summary(cluster_vax)$coefficients[2,1], 2),
                              paste0("(", paste( as.character(round(confint(cluster_vax)[2,], 2)), collapse=", "), ")"),
                              round(summary(cluster_vax)$coefficients[2,4],3)),
                            c("Medium trust", round(summary(cluster_vax)$coefficients[3,1], 2),
                              paste0("(", paste( as.character(round(confint(cluster_vax)[3,], 2)), collapse=", "), ")"),
                              round(summary(cluster_vax)$coefficients[3,4],3))))
colnames(vaxregk) <- c("Cluster", "Estimate", "95% Confidence Interval", "p-value")

deathregk <- data.frame(rbind(c("Low trust", round(summary(cluster_death)$coefficients[2,1], 2),
                               paste0("(", paste( as.character(round(confint(cluster_death)[2,], 2)), collapse=", "), ")"),
                               round(summary(cluster_death)$coefficients[2,4],3)),
                             c("Medium trust", round(summary(cluster_death)$coefficients[3,1], 2),
                               paste0("(", paste( as.character(round(confint(cluster_death)[3,], 2)), collapse=", "), ")"),
                               round(summary(cluster_death)$coefficients[3,4],3))))

colnames(deathregk) <- c("Cluster", "Estimate", "95% Confidence Interval", "p-value")

excessregk <- data.frame(rbind(c("Low trust", round(summary(cluster_excess)$coefficients[2,1], 2),
                                 paste0("(", paste( as.character(round(confint(cluster_excess)[2,], 2)), collapse=", "), ")"),
                                 round(summary(cluster_excess)$coefficients[2,4],3)),
                               c("Medium trust", round(summary(cluster_excess)$coefficients[3,1], 2),
                                 paste0("(", paste( as.character(round(confint(cluster_excess)[3,], 2)), collapse=", "), ")"),
                                 round(summary(cluster_excess)$coefficients[3,4],3))))

colnames(excessregk) <- c("Cluster", "Estimate", "95% Confidence Interval", "p-value")

gt::gt(rbind(deathregk, vaxregk, excessregk)) %>%
  gt::tab_row_group(
    group = gt::md("*Excess deaths per million*"),
    rows = 5:6
  ) %>%   
  gt::tab_row_group(
    group = gt::md("*Vaccination rate per 100 people*"),
    rows = 3:4
  ) %>%
  gt::tab_row_group(
    group = gt::md("*COVID-19 deaths per million*"),
    rows = 1:2
  ) %>% 
  gt::tab_header(
    title =gt::md('**Regression of outcomes on MPP GMM clusters**')) %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels()
  ) %>%
  gt::tab_footnote(footnote=gt::md("*High trust is the reference group used in all regression models.*"),
                 location = gt::cells_column_labels(1)) %>%
  gt::gtsave(filename = "k-means outcome table.docx")

## kmeans regression  
  
cluster_vax <- lm(data=dat, Vaccination_rate ~ factor(kmeans) + 
                    GDP + Life_expectancy + Urbanicity + Education + Freedom_Score)
cluster_death <- lm(data=dat, Deaths_million ~ factor(kmeans) + 
                      GDP + Life_expectancy + Urbanicity + Education + Freedom_Score)
cluster_excess <- lm(data=dat, Excess_per_million ~ factor(kmeans) + 
                       GDP + Life_expectancy + Urbanicity + Education + Freedom_Score)

vaxregk <- data.frame(rbind(c("Low trust", round(summary(cluster_vax)$coefficients[2,1], 2),
                              paste0("(", paste( as.character(round(confint(cluster_vax)[2,], 2)), collapse=", "), ")"),
                              round(summary(cluster_vax)$coefficients[2,4],3)),
                            c("Medium trust", round(summary(cluster_vax)$coefficients[3,1], 2),
                              paste0("(", paste( as.character(round(confint(cluster_vax)[3,], 2)), collapse=", "), ")"),
                              round(summary(cluster_vax)$coefficients[3,4],3))))
colnames(vaxregk) <- c("Cluster", "Estimate", "95% Confidence Interval", "p-value")

deathregk <- data.frame(rbind(c("Low trust", round(summary(cluster_death)$coefficients[2,1], 2),
                                paste0("(", paste( as.character(round(confint(cluster_death)[2,], 2)), collapse=", "), ")"),
                                round(summary(cluster_death)$coefficients[2,4],3)),
                              c("Medium trust", round(summary(cluster_death)$coefficients[3,1], 2),
                                paste0("(", paste( as.character(round(confint(cluster_death)[3,], 2)), collapse=", "), ")"),
                                round(summary(cluster_death)$coefficients[3,4],3))))

colnames(deathregk) <- c("Cluster", "Estimate", "95% Confidence Interval", "p-value")

excessregk <- data.frame(rbind(c("Low trust", round(summary(cluster_excess)$coefficients[2,1], 2),
                                 paste0("(", paste( as.character(round(confint(cluster_excess)[2,], 2)), collapse=", "), ")"),
                                 round(summary(cluster_excess)$coefficients[2,4],3)),
                               c("Medium trust", round(summary(cluster_excess)$coefficients[3,1], 2),
                                 paste0("(", paste( as.character(round(confint(cluster_excess)[3,], 2)), collapse=", "), ")"),
                                 round(summary(cluster_excess)$coefficients[3,4],3))))

colnames(excessregk) <- c("Cluster", "Estimate", "95% Confidence Interval", "p-value")

gt::gt(rbind(deathregk, vaxregk, excessregk)) %>%
  gt::tab_row_group(
    group = gt::md("*Excess deaths per million*"),
    rows = 5:6
  ) %>%   
  gt::tab_row_group(
    group = gt::md("*Vaccination rate per 100 people*"),
    rows = 3:4
  ) %>%
  gt::tab_row_group(
    group = gt::md("*COVID-19 deaths per million*"),
    rows = 1:2
  ) %>% 
  gt::tab_header(
    title =gt::md('**Regression of outcomes on k-means clusters**')) %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels()
  ) %>%
  gt::tab_footnote(footnote=gt::md("*High trust is the reference group used in all regression models.*"),
                   location = gt::cells_column_labels(1)) %>%
  gt::gtsave(filename = "kmeans outcome table.docx")

##

## Exploring country membership of each cluster.

dat[dat$classes=='High',]$Country

dat[dat$kmeans=='Low',]$Country

dat[dat$kmeans=='Medium',]$Country

