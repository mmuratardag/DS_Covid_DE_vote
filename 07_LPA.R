
cores <- parallel::detectCores(logical = F)
doParallel::registerDoParallel(cores = cores-1)

load("data/data.RData")
colnames(d)
library(tidyverse)
library(EGAnet)

# probability of cov infection
cov_inf <- d %>% select(hzcy001a:hzcy005a)
EGA(cov_inf, algorithm = "louvain", plot.EGA = T)

get_factor_scores <- function(df) {
  irt_op <- mirt::mirt(df,
                       model = 1,
                       itemtype = "graded")
  irt_op_fs <- mirt::fscores(irt_op, method = "MAP")
  irt_op_fs <- as.data.frame(irt_op_fs)
  irt_op_fs <- scales::rescale(irt_op_fs$F1,
                               to = c(0, 1),
                               from = range(irt_op_fs$F1, na.rm = F, finite = T))
  return(irt_op_fs)
}

d$cov_inf_prob <- get_factor_scores(cov_inf)
rm(cov_inf)

# effectiveness of measures
ef_me <- d %>% select(hzcy019a:hzcy025a)
EGA(ef_me, algorithm = "louvain", plot.EGA = T)
d$ef_me <- get_factor_scores(ef_me)
rm(ef_me)

# compliance with curfew
co_cu <- d %>% select(hzcy027a:hzcy032a)
EGA(co_cu, algorithm = "louvain", plot.EGA = T)
co_cu <- co_cu %>% select(-hzcy028a)
d$co_cu <- get_factor_scores(co_cu)
rm(co_cu)

# effectiveness Government Measures
ef_gov_me <- d %>% select(hzcy040a:hzcy043a)
EGA(ef_gov_me, algorithm = "louvain", plot.EGA = T)
d$ef_gov_me <- get_factor_scores(ef_gov_me)
rm(ef_gov_me)

# trust
trs <- d %>% select(hzcy044a:hzcy052a)
EGA(trs, algorithm = "louvain", plot.EGA = T)
# hzcy044a hzcy045a hzcy046a
# family doctor Local Health Authority Municipal Administration
# hzcy048a hzcy049a hzcy050a
# Federal Government Chancellor Ministry of Health
# hzcy047a hzcy051a hzcy052a
# Robert Koch Institute World Health Organization Scientist
trs_pol_ins_df <- d %>% select(hzcy048a, hzcy049a, hzcy050a)
d$trs_pol_ins <- get_factor_scores(trs_pol_ins_df)
trs_sci_df <- d %>% select(hzcy047a, hzcy051a, hzcy052a)
d$trs_sci <- get_factor_scores(trs_sci_df)
trs_hau_df <- d %>% select(hzcy044a, hzcy045a, hzcy046a)
d$trs_hau <- get_factor_scores(trs_hau_df)

gdata::keep(d, cores, sure = T)

library(mclust)
set.seed(666)
mc_res <- Mclust(d[,66:72])
summary(mc_res)
library(factoextra)
fviz_mclust(mc_res, "BIC", palette = "jco")
# mc_res_LRT <- mclustBootstrapLRT(d[,66:72], modelName = "VEV", maxG = 10)
# mc_res_LRT
# 8 vs 9    385.3742             0.001
# 9 vs 10   -46.5509             0.706
# 10 vs 11  191.9868             0.001
# mclustModelNames("VEV")

d$profile <- mc_res$classification
d <- d %>% mutate(profile = recode_factor(profile,
                                          `1` = "Profile1",
                                          `2` = "Profile2",
                                          `3` = "Profile3",
                                          `4` = "Profile4",
                                          `5` = "Profile5",
                                          `6` = "Profile6",
                                          `7` = "Profile7",
                                          `8` = "Profile7",
                                          `9` = "Profile9"))

# d %>% select(cov_inf_prob:profile) %>%
#   mutate_each_(funs(scale),vars=c(names(d[,66:72]))) %>%
#   group_by(profile) %>% summarize_all(mean) %>%
#   pivot_longer(!profile, names_to = "indicator", values_to = "mean") %>%
#   ggplot(aes(x = indicator, y = mean,
#                     group = profile, color = profile)) +
#   geom_point(size = 2) +
#   geom_line(size = 1) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "top")

df_temp <- d %>% select(cov_inf_prob:profile) %>%
  mutate_each_(funs(scale),vars=c(names(d[,66:72]))) %>%
  group_by(profile) %>% summarize_all(mean) %>%
  gather(key, val, -profile) %>%
  rename(variable = key) %>% 
  mutate(variable = recode(variable,
                           cov_inf_prob = "Covid Infection Probability",
                           ef_me = "Belief in the Effectiveness of Covid Measures",
                           co_cu = "Compliance with a Posible Curfew",
                           ef_gov_me = "Support of Government's Covid Measures",
                           trs_pol_ins = "Trust in Political Actors",
                           trs_sci = "Trust in Science Institutions",
                           trs_hau = "Trust in Health Authorities"))

p1 <- df_temp %>% ggplot(aes(x = profile, 
                      y = val, fill = variable)) +
  geom_col(position = "dodge") + 
  scale_x_discrete() +
  ggsci::scale_fill_jco() +
  xlab("") +
  ylab("Item Means") + 
  theme_bw() +
  scale_colour_hue() +
  labs(title = "Estimated Profiles")

library(janitor)
ct <- d %>% tabyl(choice_of_party, profile)
p2 <- ct %>% pivot_longer(!choice_of_party, names_to = "profile", values_to = "count") %>%
  ggplot(aes(fill = profile, y = count, x = choice_of_party)) + 
  geom_bar(position = "fill", stat = "identity") +
  ggsci::scale_fill_lancet() +
  xlab("Political Parties") +
  ylab("Profile %") + theme_bw() +
  labs(title = "% of Profiles by Voting Intention")

gridExtra::grid.arrange(p1, p2)

d <- d %>% select(-c(hzcy001a:hzcy005a, # prob cov inf
                    hzcy019a:hzcy025a, # eff of mea
                    hzcy027a:hzcy032a, # comp w curf
                    hzcy040a:hzcy043a, # ef gov mea
                    hzcy044a:hzcy052a)) # trs

save(d, file = "data/data_with_profile.RData")
