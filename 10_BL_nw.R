
load("data/data_class.RData")
colnames(d)

library(tidyverse)
d <- d %>% select(2:4,6:9,66) %>%
  mutate(sex = as.numeric(sex),
         age_cat = as.numeric(age_cat),
         education_cat = as.numeric(education_cat),
         choice_of_party = as.numeric(choice_of_party),
         marstat = as.numeric(marstat),
         household = as.numeric(household),
         class = as.numeric(class))
d <- data.frame(d)

blacklist <- readxl::read_excel("data/from_to_blacklist_covid_DE_cl.xlsx")
bl_mat <- as.matrix(blacklist)

library(parallel)
cl <- makeCluster(7)

library(bnlearn)
set.seed(666)
bn_op <- tabu(d, blacklist = bl_mat)

library(qgraph)
qgraph(bn_op, vsize = 15, label.cex = 3)
set.seed(666)
boot_res <- boot.strength(data = d,
                          R = 10000,
                          algorithm = "tabu",
                          algorithm.args = list(blacklist = bl_mat),
                          cluster = cl)

avgnet_threshold <- averaged.network(boot_res, threshold = .975)

Labels <- c("Gender", "Age Group", "Education Level",
            "Party Choice", "Political Orientation",
            "Marital Status", "# of People in Household",
            "Latent Class")

qgraph(avgnet_threshold, vsize = 15, label.cex = 3, labels = Labels,
       title = "Bayesian Belief Network Explaining the Mechanism for Voting Intention with Latent Class")

gdata::keep(cl, sure = T)

load("data/data_profile.RData")
colnames(d)

d <- d %>% select(2:4,6:9,42) %>%
  mutate(sex = as.numeric(sex),
         age_cat = as.numeric(age_cat),
         education_cat = as.numeric(education_cat),
         choice_of_party = as.numeric(choice_of_party),
         marstat = as.numeric(marstat),
         household = as.numeric(household),
         profile = as.numeric(profile))
d <- data.frame(d)

blacklist <- readxl::read_excel("data/from_to_blacklist_covid_DE_pr.xlsx")
bl_mat <- as.matrix(blacklist)

set.seed(666)
bn_op <- tabu(d, blacklist = bl_mat)
qgraph(bn_op, vsize = 15, label.cex = 3)
set.seed(666)
boot_res <- boot.strength(data = d,
                          R = 10000,
                          algorithm = "tabu",
                          algorithm.args = list(blacklist = bl_mat),
                          cluster = cl)
avgnet_threshold <- averaged.network(boot_res, threshold = .975)

Labels <- c("Gender", "Age Group", "Education Level",
            "Party Choice", "Political Orientation",
            "Marital Status", "# of People in Household",
            "Latent Profile")

qgraph(avgnet_threshold, vsize = 15, label.cex = 3, labels = Labels,
       title = "Bayesian Belief Network Explaining the Mechanism for Voting Intention with Latent Profile")
