## ----setup, include=FALSE, eval=TRUE-------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(magrittr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(knitr)
options(digits = 3)

## ----eval=FALSE----------------------------------------------------------
#  install.packages("ahpsurvey")

## ------------------------------------------------------------------------
library(ahpsurvey)

## ----echo= FALSE---------------------------------------------------------
Rating <- as.character(1:9)
Definition <- c("Two characteristics are equally important",
                "Between 1 and 3",
                "The preferred characteristics are slightly more important",
                "Between 3 and 5",
                "The preferred characteristics are moderately more important",
                "Between 5 and 7",
                "The preferred characteristics are strongly more important",
                "Between 7 and 9",
                "The preferred characteristics are absolutely more important")
data.frame(Rating, Definition) %>% kable()

## ------------------------------------------------------------------------
atts <- c("cult", "fam", "house", "jobs", "trans")
data(city200)
head(city200)

## ------------------------------------------------------------------------
city200 %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  head(3)

## ----fig.cap="\\label{fig:figs}Maximum difference of between eigenvalue and mean aggregation", fig.height=4, fig.width=7----
cityahp <- city200 %>% 
  ahp.mat(atts, negconvert = T)
eigentrue <- ahp.indpref(cityahp, atts, method = "eigen")
geom <- ahp.indpref(cityahp, atts, method = "arithmetic")
error <- data.frame(id = 1:length(cityahp), maxdiff = apply(abs(eigentrue - geom), 1, max))
error %>%
  ggplot(aes(x = id, y = maxdiff)) +
  geom_point() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_x_continuous("Respondent ID") +
  scale_y_continuous("Maximum difference") +
  theme_minimal()

## ------------------------------------------------------------------------
amean <- ahp.aggpref(cityahp, atts, method = "arithmetic")
amean

## ----fig.cap="\\label{fig:figs}Changes of aggregated weights based on quantile of data trimmed", fig.height=4, fig.width=7----
qtresults <- matrix(nrow = 50, ncol = 5, data = NA)
for (q in 1:50){
  qtresults[q,] <- ahp.aggpref(cityahp, atts, method = "arithmetic", 
                               aggmethod = "tmean", qt = (q-1)/100)
}
colnames(qtresults) <- atts
qtresults %>%
  as.data.frame() %>%
  mutate(trimperc = 1:nrow(qtresults)-1) %>%
  mutate(cult = cult - amean[1],
         fam = fam - amean[2],
         house = house - amean[3],
         jobs = jobs - amean[4],
         trans = trans - amean[5]) %>%
  gather(cult, fam, house, jobs, trans, key = "att", value = "weight") %>%
  ggplot(aes(x = trimperc, y = weight, group = att, shape = att, color = att, fill = att)) +
  geom_line() +
  geom_point() +
  scale_x_continuous("Quantile (from top and bottom) trimmed") +
  scale_y_continuous("Change from untrimmed mean") +
  geom_hline(yintercept = 0, color = "gray") +
  theme_minimal()

## ------------------------------------------------------------------------
mean <- city200 %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.aggpref(atts, method = "arithmetic")

sd <- city200 %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.aggpref(atts, method = "arithmetic", aggmethod = "sd")

t(data.frame(mean, sd))%>% kable()

## ------------------------------------------------------------------------
city200 %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.aggjudge(atts, aggmethod = "geometric")

## ----echo=FALSE----------------------------------------------------------
rownum <- seq(1:15)
RI <- t(data.frame(c(0.0000000, 0.0000000, 0.5251686, 0.8836651, 1.1081014, 1.2492774, 1.3415514, 1.4048466, 1.4507197, 1.4857266, 1.5141022,1.5356638, 1.5545925, 1.5703498, 1.5839958)))
rownames(RI) <- "RI"
colnames(RI) <- 1:15
RI%>% kable()

## ------------------------------------------------------------------------
weight <- c(5,-3,2,-5,
            -7,-1,-7,
            4,-3,
            -7)
sample_mat <- ahp.mat(t(weight), atts, negconvert = TRUE)

(cr_std <- ahp.cr(sample_mat, atts))

## ------------------------------------------------------------------------
cr <- city200 %>%
  ahp.mat(atts, negconvert = T) %>% 
  ahp.cr(atts)
table(cr <= 0.1)

## ------------------------------------------------------------------------
## Generate a random index with 1000 simulations, 5 dimensions and seed 30000 for reproducibility (seed = 42 by default).
(RI <- ahp.ri(nsims = 1000, dim = 5, seed = 30000))

## Use this RI to calculate the consistency ratio instead of the default one.
ahp.cr(sample_mat, atts, RI)

## ----fig.cap="\\label{fig:figs}Individual priorities with respect to goal", fig.height=4, fig.width=7----
thres <- 0.1
dict <- c("cult" = "Culture", 
          "fam" = "Family", 
          "house" = "Housing", 
          "jobs" = "Jobs", 
          "trans" = "Transportation")

cr.df <- city200 %>%
  ahp.mat(atts, negconvert = TRUE) %>% 
  ahp.cr(atts) %>% 
  data.frame() %>%
  mutate(rowid = 1:length(cr), cr.dum = as.factor(ifelse(cr <= thres, 1, 0))) %>%
  select(cr.dum, rowid)

city200 %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.indpref(atts, method = "eigen") %>% 
  mutate(rowid = 1:nrow(eigentrue)) %>%
  left_join(cr.df, by = 'rowid') %>%
  gather(cult, fam, house, jobs, trans, key = "var", value = "pref") %>%
  ggplot(aes(x = var, y = pref)) + 
  geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "gray") +
  geom_jitter(alpha = 0.6, height = 0, width = 0.1, aes(color = cr.dum)) +
  geom_boxplot(alpha = 0, width = 0.3, color = "#808080") +
  scale_x_discrete("Attribute", label = dict) +
  scale_y_continuous("Weight (dominant eigenvalue)", 
                     labels = scales::percent, 
                     breaks = c(seq(0,0.7,0.1))) +
  guides(color=guide_legend(title=NULL))+
  scale_color_discrete(breaks = c(0,1), 
                       labels = c(paste("CR >", thres), 
                                  paste("CR <", thres))) +
  labs(NULL, caption = paste("n =", nrow(city200), ",", "Mean CR =",
                           round(mean(cr),3)))+
  theme_minimal()

## ----echo = FALSE--------------------------------------------------------
sample_mat

## ------------------------------------------------------------------------

preference <- t(ahp.indpref(sample_mat, atts, method = "eigen"))
preference

## ------------------------------------------------------------------------
S <- preference %*% t((preference)^-1)
S

## ------------------------------------------------------------------------
sample_mat[[1]] * t(S)


## ------------------------------------------------------------------------
error <- ahp.error(sample_mat, atts, reciprocal = TRUE)
error

## ------------------------------------------------------------------------
gm_mean <- function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

mat <- cityahp %>%
  ahp.error(atts, reciprocal = TRUE) %>%
  unlist() %>%
  as.numeric() %>%
  array(dim=c(length(atts), length(atts), length(cityahp))) %>%
  apply(c(1,2), gm_mean)

colnames(mat) <- rownames(mat) <- atts

mat

## ------------------------------------------------------------------------
city200 %>%
  ahp.mat(atts) %>%
  ahp.pwerror(atts) %>%
  head()

## ----fig.cap="\\label{fig:figs}Pairwise comparison and its frequency\n as the most, second-most, and third most inconsistent pairwise comparsion", fig.height=4, fig.width=7----
cityahp %>%
  ahp.pwerror(atts) %>% 
  gather(top1, top2, top3, key = "max", value = "pair") %>%
  table() %>%
  as.data.frame() %>%
  ggplot(aes(x = pair, y = Freq, fill = max)) + 
  geom_bar(stat = 'identity') +
  scale_y_continuous("Frequency", breaks = c(seq(0,180,20))) +
  scale_fill_discrete(breaks = c("top1", "top2", "top3"), labels = c("1", "2", "3")) +
  scale_x_discrete("Pair") +
  guides(fill = guide_legend(title="Rank")) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        panel.background = element_rect(fill = NA),
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.major.x = element_blank(),
        panel.ontop = FALSE)

## ------------------------------------------------------------------------
family <- c(1,1/5,1/3,1/7,1/6,1/6,3,4,
            5,1,3,1/5,1/3,1/3,5,7,
            3,1/3,1,1/6,1/3,1/4,1/6,5,
            7,5,6,1,3,4,7,8,
            6,3,3,1/3,1,2,5,6,
            6,3,4,1/4,1/2,1,5,6,
            1/3,1/5,6,1/7,1/5,1/5,1,2,
            1/4,1/7,1/5,1/8,1/6,1/6,1/2,1)

fam.mat <- list(matrix(family, nrow = 8 , ncol = 8))

atts <- c("size", "trans", "nbrhd", "age", "yard", "modern", "cond", "finance")

rownames(fam.mat[[1]]) <- colnames(fam.mat[[1]]) <- atts

fam.mat[[1]] %>% kable()

## ------------------------------------------------------------------------
ahp.cr(fam.mat, atts)

## ------------------------------------------------------------------------
edited <- ahp.harker(fam.mat, atts, iterations = 10, stopcr = 0.1)
edited[[1]]%>% kable() 
ahp.cr(edited, atts)

## ------------------------------------------------------------------------
crmat <- matrix(NA, nrow = 200, ncol = 11)
colnames(crmat) <- 0:10

atts <- c("cult", "fam", "house", "jobs", "trans")

crmat[,1] <- city200 %>%
    ahp.mat(atts, negconvert = TRUE) %>%
    ahp.cr(atts)

for (it in 1:10){
  crmat[,it+1] <- city200 %>%
    ahp.mat(atts, negconvert = TRUE) %>%
    ahp.harker(atts, iterations = it, stopcr = 0.1, 
               limit = T, round = T, printiter = F) %>%
    ahp.cr(atts)
}

data.frame(table(crmat[,1] <= 0.1), 
           table(crmat[,3] <= 0.1),
           table(crmat[,5] <= 0.1)) %>% 
  select(Var1, Freq, Freq.1, Freq.2) %>%
  rename("Consistent?" = "Var1", "No Iteration" = "Freq",
         "2 Iterations" = "Freq.1", "4 Iterations" = "Freq.2")


## ---- fig.height=4, fig.width=7, fig.cap="\\label{fig:figs}Consistency Ratios under different number of iterations with Harker's method"----
crmat %>% 
  as.data.frame() %>%
  gather(key = "iter", value = "cr", `0`, 1,2,3,4,5,6,7,8,9,10,11) %>%
  mutate(iter = as.integer(iter)) %>%
  ggplot(aes(x = iter, y = cr, group = iter)) +
  geom_hline(yintercept = 0.1, color = "red", linetype = "dashed")+
  geom_jitter(alpha = 0.2, width = 0.3, height = 0, color = "turquoise4") +
  geom_boxplot(fill = "transparent", color = "#808080", outlier.shape = NA) + 
  scale_x_continuous("Iterations", breaks = 0:10) +
  scale_y_continuous("Consistency Ratio") +
  theme_minimal()

## ---- fig.height=4, fig.width=7, fig.cap="\\label{fig:figs}Individual preference weights with respect to goal (1 iteration)"----
it <- 1
thres <- 0.1
cr.df1 <- data.frame(cr = city200 %>%
  ahp.mat(atts, negconvert = TRUE) %>%
  ahp.harker(atts, iterations = it, stopcr = 0.1, limit = T, round = T, printiter = F) %>%
  ahp.cr(atts))

cr.df2 <- cr.df1 %>%
  mutate(rowid = 1:nrow(city200), cr.dum = as.factor(ifelse(. <= thres, 1, 0))) %>%
  select(cr.dum, rowid)

city200 %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.harker(atts, iterations = it, stopcr = 0.1, limit = T, round = T, printiter = F) %>%
  ahp.indpref(atts, method = "eigen") %>% 
  mutate(rowid = 1:nrow(city200)) %>%
  left_join(cr.df2, by = 'rowid') %>%
  gather(cult, fam, house, jobs, trans, key = "var", value = "pref") %>%
  ggplot(aes(x = var, y = pref)) + 
  geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "gray") +
  geom_jitter(alpha = 0.3, height = 0, width = 0.1, aes(color = cr.dum)) +
  geom_boxplot(alpha = 0, width = 0.3, color = "#808080") +
  scale_x_discrete("Attribute", label = dict) +
  scale_y_continuous("Weight (dominant eigenvalue)", 
                     labels = scales::percent, breaks = c(seq(0,0.7,0.1))) +
  guides(color=guide_legend(title=NULL))+
  scale_color_discrete(breaks = c(0,1), 
                       labels = c(paste("CR >", thres), 
                                  paste("CR <", thres))) +
  labs(NULL, caption =paste("n =",nrow(city200), ",", "Mean CR =",round(mean(cr),3)))+
  theme_minimal()

## ------------------------------------------------------------------------
options(scipen = 99)
inconsistent <- city200 %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.aggpref(atts, method = "eigen")

consistent <- city200 %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.harker(atts, iterations = 5, stopcr = 0.1, limit = T, round = T, printiter = F) %>%
  ahp.aggpref(atts, method = "eigen")

true <- t(ahp.indpref(sample_mat, atts, method = "eigen"))

aggpref.df <- data.frame(Attribute = atts, true,inconsistent,consistent) %>%
  mutate(error.incon = abs(true - inconsistent),
         error.con = abs(true - consistent))

aggpref.df

## ------------------------------------------------------------------------
missing.df <- city200[1:10,]
for (i in 1:10){
  missing.df[i, round(runif(1,1,10))] <- NA
  if (i > 7){
    missing.df[i, round(runif(1,2,10))] <- NA
  }
}
missing.df[,1:7]

## ------------------------------------------------------------------------
atts <- c("cult", "fam", "house", "jobs", "trans")
imputed <- missing.df %>% 
  ahp.mat(atts, negconvert = TRUE) %>%
  ahp.missing(atts, round = T, limit = T)

actual <- city200 %>% 
  ahp.mat(atts, negconvert = TRUE)

list(actual[[5]],imputed[[5]])


## ------------------------------------------------------------------------
list(ahp.cr(actual, atts)[[5]],ahp.cr(imputed, atts)[[5]])

## ------------------------------------------------------------------------
list(actual[[8]],imputed[[8]])

## ------------------------------------------------------------------------
list(ahp.cr(actual, atts)[[8]],ahp.cr(imputed, atts)[[8]])

## ------------------------------------------------------------------------
canned <- ahp(df = city200, 
              atts = c('cult', 'fam', 'house', 'jobs', 'trans'), 
              negconvert = TRUE, 
              reciprocal = TRUE,
              method = 'arithmetic', 
              aggmethod = "arithmetic", 
              qt = 0.2,
              censorcr = 0.1,
              agg = TRUE)
head(canned$indpref)


## ------------------------------------------------------------------------
canned$aggpref

## ------------------------------------------------------------------------
library(randomNames)

edl <- c("No High School", "High School", "Undergraduate", "Postgraduate")

edunames <- tibble(edu = factor(rep(edl,50)),
                  names = randomNames(200, which.names = "first"),
                  catowner = c(rep(TRUE,100), rep(FALSE,100)))

citynames <- cbind(edunames, city200)
head(citynames)

## ---- error = TRUE-------------------------------------------------------
named <- ahp(df = citynames, 
              atts = c('cult', 'fam', 'house', 'jobs', 'trans'), 
              negconvert = TRUE, 
              reciprocal = TRUE,
              method = 'arithmetic', 
              aggmethod = "arithmetic", 
              qt = 0.2,
             censorcr = 0.1,
             agg = FALSE, 
             ID = c("edu", "names")
             )

head(named)

## ------------------------------------------------------------------------
columns <- c("cult_fam", "cult_house", "cult_jobs", "cult_trans",
           "fam_house", "fam_jobs", "fam_trans",
           "house_jobs", "house_trans",
           "jobs_trans")

named <- ahp(df = citynames, 
             atts = c('cult', 'fam', 'house', 'jobs', 'trans'), 
             negconvert = TRUE, 
             reciprocal = TRUE,
             method = 'arithmetic', 
             aggmethod = "arithmetic",
             qt = 0.2,
             censorcr = 0.1,
             agg = FALSE, 
             ID = c("edu", "names"),
             col = columns
             )

head(named)

## ------------------------------------------------------------------------
named %>%
  group_by(edu) %>%
  dplyr::summarize(Mean = mean(cult, na.rm=TRUE))

## ------------------------------------------------------------------------
## Defining attributes
set.seed(42)
atts <- c("cult", "fam", "house", "jobs", "trans")

colnames <- c("cult_fam", "cult_house", "cult_jobs", "cult_trans",
              "fam_house", "fam_jobs", "fam_trans",
              "house_jobs", "house_trans",
              "jobs_trans")

## True weights derived from Saaty's example
weight <- c(5,-3,2,-5,
            -7,-1,-7,
            4,-3,
            -7)

## Defining the saaty scale
saatyscale <- c(-9:-2, 1:9)
nobs <- 200

## saatyprob creates a list of probabilities in the saaty scale for being sampled given
## the position of the weight in the weight list (x) and standard deviation (sd)

saatyprob <- function(x, sd) dnorm(saatyscale, mean = weight[x], sd = sd) 

## Standard deviation set on saatyprob(x, *sd*)
cult_fam <- sample(saatyscale, nobs, prob = saatyprob(1, 2), replace = TRUE)
cult_house <- sample(saatyscale, nobs, prob = saatyprob(2, 1), replace = TRUE)
cult_jobs <- sample(saatyscale, nobs, prob = saatyprob(3, 2), replace = TRUE)
cult_trans <- sample(saatyscale, nobs, prob = saatyprob(4, 1.5), replace = TRUE)
fam_house <- sample(saatyscale, nobs, prob = saatyprob(5, 2), replace = TRUE)
fam_jobs <- sample(saatyscale, nobs, prob = saatyprob(6, 1.5), replace = TRUE)
fam_trans <- sample(saatyscale, nobs, prob = saatyprob(7, 2.5), replace = TRUE)
house_jobs <- sample(saatyscale, nobs, prob = saatyprob(8, 0.5), replace = TRUE)
house_trans <- sample(saatyscale, nobs, prob = saatyprob(9, 0.5), replace = TRUE)
jobs_trans <- sample(saatyscale, nobs, prob = saatyprob(10, 1), replace = TRUE)

city200 <- data.frame(cult_fam, cult_house, cult_jobs, cult_trans,
                      fam_house, fam_jobs, fam_trans,
                      house_jobs, house_trans,
                      jobs_trans)
head(city200[,1:7])

