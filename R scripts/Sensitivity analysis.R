### Sensitivity analysis ##
set.seed(2024)

library(car); library(tidyverse) ; library(factoextra)
library(countrycode) ; library(EValue) ; library(effsize)

dat <- read.csv("~/Desktop/GSRA/Trust paper/Organized (clean) dataframes/Regression dataframe.csv", check.names=FALSE)

countries <- countrycode(dat$Country, 'country.name', 'iso3c')

# Adding in all confounders.

age <- read.csv("~/Desktop/GSRA/Trust paper/Confounders Data/Population over 65/Population over 65.csv", skip = 3) %>%
  select(Country.Code, X2019) %>% filter(Country.Code %in% countries)
# Taiwan: Pulled from https://www.ceicdata.com/en/taiwan/social-demography-non-oecd-member-annual/population-ages-65-and-above--of-total-population
age <- rbind(age, c("TWN", "15.080"))
colnames(age) <- c("Country", "over_65")
age$over_65 <- as.numeric(age$over_65)
age$Country <- countrycode(age$Country, 'iso3c', 'country.name')

hospital <- read.csv("~/Desktop/GSRA/Trust paper/Confounders Data/Hospital beds/Hospital beds.csv", skip = 3)%>%
  filter(Country.Code %in% countries) %>% select(Country.Code, X2019)
# Missing data for: Andorra, Australia, Ethiopia, Nigeria, Vietnam, Zimbabwe, Taiwan. Carrying last obs forward for all but Taiwan (not in data)
hospital[hospital$Country.Code == "AND",]$X2019 <- 2.5 # 2009
hospital[hospital$Country.Code == "AUS",]$X2019 <- 3.8 # 2016
hospital[hospital$Country.Code == "ETH",]$X2019 <- 0.3 # 2016
hospital[hospital$Country.Code == "VNM",]$X2019 <- 2.5 # 2017
hospital[hospital$Country.Code == "ZWE",]$X2019 <- 2.0 # 2014
hospital[hospital$Country.Code == "NGA",]$X2019 <- 0.5 # 2004
hospital <- rbind(hospital, c("TWN", 4.172))
colnames(hospital) <- c("Country", "hospital_beds")
hospital$Country <- countrycode(hospital$Country, 'iso3c', 'country.name')
hospital$hospital_beds <- as.numeric(hospital$hospital_beds)
# Sourced from https://www.mohw.gov.tw/cp-7155-79484-2.html 

# Gov't effectiveness
wgi <- read.csv("~/Desktop/GSRA/Trust paper/Confounders Data/wgidataset_excel/wgi.csv") %>% select(countryname, year, indicator, estimate) %>%
  filter(indicator == 'ge', year == 2019) %>% select(countryname, estimate)
wgi$countryname <- countrycode(wgi$countryname, "country.name", "iso3c")
wgi$estimate <- as.numeric(wgi$estimate) + 2.5 ## So that all values are positive
wgi <- wgi %>% filter(countryname %in% countries)
colnames(wgi) <- c("Country", "govt_effectiveness")
wgi$Country <- countrycode(wgi$Country, 'iso3c', 'country.name')

dat <- cbind(dat, Over_65=age$over_65, Hospital_beds=hospital$hospital_beds, Govt_effectiveness=wgi$govt_effectiveness)

###

# Obtaining k-means clusters.

covariates <- colnames(dat[,2:34])
outcomes <- c('Deaths_million', 'Vaccination_rate', 'Excess_per_million')
confounders <- c("GDP", "Life_expectancy", "Education", "Urbanicity", "Freedom_Score",
                 "Over_65", "Hospital_beds", "Govt_effectiveness")
irrel <- c("Q64", "Q65", "Q68", "Q69", "Q77", "Q78", "Q80", "Q84", "Q85","Q89")
questions <- covariates[! covariates %in% irrel]

plot_df <- dat[,questions]
rownames(plot_df) <- dat$Country

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
colnames(trust) <- c("High", "Medium", "Low")
colMeans(trust[questions,])

# These roughly correspond to Low, Medium, and High 

dat[dat$kmeans==1,]$kmeans <- "Medium"
dat[dat$kmeans==2,]$kmeans <- "High"
dat[dat$kmeans==3,]$kmeans <- "Low"

cluster_death <-  lm(data=dat, Deaths_million ~ factor(kmeans) + GDP + Life_expectancy + Urbanicity +
                       Education + Freedom_Score + Hospital_beds + Over_65 + Govt_effectiveness)
cluster_excess <-  lm(data=dat, Excess_per_million ~ factor(kmeans) + GDP + Life_expectancy + Urbanicity +
                        Education + Freedom_Score + Hospital_beds + Over_65 + Govt_effectiveness)
cluster_vax <- lm(data=dat, Vaccination_rate ~ factor(kmeans) + GDP + Life_expectancy + Urbanicity +
                    Education + Freedom_Score + Hospital_beds + Over_65 + Govt_effectiveness)

######## Vax rate ########

formulas <- list()
formulas[[1]] <- as.formula("Vaccination_rate ~ factor(kmeans)")
for (i in seq_along(confounders)) {
  tmp <- combn(confounders, i)
  tmp <- apply(tmp, 2, paste, collapse=" + ")
  tmp <- paste0("Vaccination_rate ~", " factor(kmeans) + ", tmp)
  formulas[[i+1]] <- tmp
}
formulas <- unlist(formulas)
formulas <- sapply(formulas, as.formula)

models <- lapply(formulas, lm, data=dat)

get_cis <- function(model){return(c(model$coefficients[2], confint(model)[2,]))}

holds <- lapply(models, get_cis)

results <- t(data.frame(holds))
colnames(results) <- c("est", "lower", "upper")
rownames(results) <- 1:length(holds)

rects <- data.frame(xstart = c(0,1,9,38,94,163,120,220, 256), xend = c(1,9,38,94,163,120,220,256, 257), col = letters[1:9])

mean_vax <- cohen.d(dat[dat$kmeans=="High", 'Vaccination_rate'], dat[dat$kmeans=="Low", 'Vaccination_rate'])
vax_evalue <- evalues.MD(est = mean_vax$estimate, se = mean_vax$var)

ggplot() +
  geom_hline(yintercept = 0, linetype=2)+
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, 
                              ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3) +
  geom_linerange(data=results, aes(x = 1:256,y = est, ymin = lower, ymax = upper), 
                 cex=0.75, color='gray60')+ 
  geom_point(data=results, aes(x = 1:256,y = est), 
             color='red', size=1.25) +
  ylab(paste("Average difference in vaccination rate (from a high to low trust country)"))+
  scale_x_continuous()+
  scale_fill_discrete(labels=c('0', '1', '2', '3' ,'4', '5', '6', '7','8'))+
 # ggtitle("Sensitivity analysis for average vaccination rate in a low trust vs. high trust country")+
  guides(fill=guide_legend(title="Number of adjusted covariates"),
  )+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        text=element_text(size=12),
        legend.position="bottom") +
  annotate("label", x = 225, y = 0.25, label = paste("E-value = ",round(vax_evalue[2,1], 2),"\n","Lower bound = ",round(vax_evalue[2,2], 2),sep = ""), size=5)


######## Excess deaths ########

formulas <- list()
formulas[[1]] <- as.formula("Excess_per_million ~ factor(kmeans)")
for (i in seq_along(confounders)) {
  tmp <- combn(confounders, i)
  tmp <- apply(tmp, 2, paste, collapse=" + ")
  tmp <- paste0("Excess_per_million ~", " factor(kmeans) + ", tmp)
  formulas[[i+1]] <- tmp
}
formulas <- unlist(formulas)
formulas <- sapply(formulas, as.formula)

models <- lapply(formulas, lm, data=dat)

get_cis <- function(model){return(c(model$coefficients[2], confint(model)[2,]))}

holds <- lapply(models, get_cis)

results <- t(data.frame(holds))
colnames(results) <- c("est", "lower", "upper")
rownames(results) <- 1:length(holds)

rects <- data.frame(xstart = c(0,1,9,38,94,163,120,220, 256), xend = c(1,9,38,94,163,120,220,256, 257), col = letters[1:9])
#pcol = ifelse(results2[,3]<0.05, 'red','dark grey')

mean_excess <- cohen.d(dat[dat$kmeans=="High", 'Excess_per_million'], dat[dat$kmeans=="Low", 'Excess_per_million'])
excess_evalue <- evalues.MD(est = mean_excess$estimate, se = mean_excess$var)

ggplot() +
  geom_hline(yintercept = 0, linetype=2)+
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, 
                              ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3) +
  geom_linerange(data=results, aes(x = 1:256,y = est, ymin = lower, ymax = upper), 
                 cex=0.75, color='gray60')+ 
  geom_point(data=results, aes(x = 1:256,y = est), 
             color='red', size=1.25) +
  ylab(paste("Average difference in excess deaths per million (from a high to low trust country)"))+
  scale_x_continuous()+
  scale_fill_discrete(labels=c('0', '1', '2', '3' ,'4', '5', '6', '7','8'))+
  # ggtitle("Sensitivity analysis for excess deaths per million in a low trust vs. high trust country")+
  guides(fill=guide_legend(title="Number of adjusted covariates"),
  )+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        text=element_text(size=12),
        legend.position="bottom") +
  annotate("label", x = 225, y = 4500, label = paste("E-value = ",round(excess_evalue[2,1], 2),"\n","Lower bound = ",round(excess_evalue[2,3], 2),sep = ""), size=5)

######## deaths per million ########

formulas <- list()
formulas[[1]] <- as.formula("Deaths_million ~ factor(kmeans)")
for (i in seq_along(confounders)) {
  tmp <- combn(confounders, i)
  tmp <- apply(tmp, 2, paste, collapse=" + ")
  tmp <- paste0("Deaths_million ~", " factor(kmeans) + ", tmp)
  formulas[[i+1]] <- tmp
}
formulas <- unlist(formulas)
formulas <- sapply(formulas, as.formula)

models <- lapply(formulas, lm, data=dat)

get_cis <- function(model){return(c(model$coefficients[2], confint(model)[2,]))}

holds <- lapply(models, get_cis)

results <- t(data.frame(holds))
colnames(results) <- c("est", "lower", "upper")
rownames(results) <- 1:length(holds)

rects <- data.frame(xstart = c(0,1,9,38,94,163,120,220, 256), xend = c(1,9,38,94,163,120,220,256, 257), col = letters[1:9])
#pcol = ifelse(results2[,3]<0.05, 'red','dark grey')

mean_death <- cohen.d(dat[dat$kmeans=="High", 'Deaths_million'], dat[dat$kmeans=="Low", 'Deaths_million'])
death_evalue <- evalues.MD(est = mean_death$estimate, se = mean_death$var)

ggplot() +
  geom_hline(yintercept = 0, linetype=2)+
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, 
                              ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3) +
  geom_linerange(data=results, aes(x = 1:256,y = est, ymin = lower, ymax = upper), 
                 cex=0.75, color='gray60')+ 
  geom_point(data=results, aes(x = 1:256,y = est), 
             color='red', size=1.25) +
  ylab(paste("Average difference in COVID-19 deaths per million (from a high to low trust country)"))+
  scale_x_continuous()+
  scale_fill_discrete(labels=c('0', '1', '2', '3' ,'4', '5', '6', '7','8'))+
  # ggtitle("Sensitivity analysis for average COVID-19 deaths per million in a low trust vs. high trust country")+
  guides(fill=guide_legend(title="Number of adjusted covariates"),
  )+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position="bottom") +
  annotate("label", x = 225, y = 2800, label = paste("E-value = ",round(death_evalue[2,1], 2),"\n","Lower bound = ",round(death_evalue[2,3], 2),sep = ""), size=5)

