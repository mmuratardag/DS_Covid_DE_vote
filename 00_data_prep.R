
cores <- parallel::detectCores(logical = F)
doParallel::registerDoParallel(cores = cores-1)

library(haven)
library(tidyverse)
d <- haven::read_dta("data/ZA5667_v1-1-0_Stata14.dta") %>% 
  zap_formats() %>% zap_labels() %>% zap_label()
# publicly available
# @ https://doi.org/10.4232/1.13520
# alternatively 
# gesisdata::gesis_download(file_id = "ZA5667")
# see https://fsolt.org/gesisdata/articles/gesisdata-vignette.html
# sjPlot::view_df(d)
d <- d %>% select(id, sex:hzcy099a) %>% 
  na_if(-111) %>%
  na_if(-99) %>% 
  na_if(-88) %>% 
  na_if(-77) %>% 
  na_if(-77) %>%
  na_if(-33) %>%
  na_if(-22) %>%
  na_if(98)

library(naniar)
d <- d %>% replace_with_na_at(.vars = c("intention_to_vote","choice_of_party"),
                     condition = ~.x == 97)

d %>% select(hzcy001a:hzcy018a) %>% vis_miss() # measures & actions taken
d %>% select(hzcy019a:hzcy025a) %>% vis_miss() # effectiveness of measures
d %>% select(hzcy026a) %>% vis_miss() # curfew compliance vs refusal
d %>% select(hzcy027a:hzcy039a) %>% vis_miss() # compliance with curfew
d <- d %>% select(-c(hzcy033a:hzcy039a)) # remove denial of curfew
d %>% select(hzcy027a:hzcy032a) %>% vis_miss() # only compliance with curfew
d %>% select(hzcy040a:hzcy043a) %>% vis_miss() # effectiveness of government measures
d %>% select(hzcy044a:hzcy052a) %>% vis_miss() # trust in institutions
d %>% select(hzcy053a) %>% vis_miss() # employment status in early March
d %>% select(hzcy054a:hzcy060a) %>% vis_miss() # employees
d <- d %>% select(-c(hzcy054a:hzcy060a)) # remove employees items
d %>% select(hzcy061a:hzcy070a) %>% vis_miss() # self-employed
d <- d %>% select(-c(hzcy061a:hzcy070a)) # remove self-employed items
d %>% select(hzcy071a:hzcy083a) %>% vis_miss() # children/child-care
d <- d %>% select(-c(hzcy071a:hzcy083a)) # remove children/child-care
d %>% select(hzcy084a:hzcy095a) %>% vis_miss() # media consumption
d %>% select(hzcy096a) %>% vis_miss() # facebook
d <- d %>% select(-hzcy096a) # remove facebook
d %>% select(hzcy097a:hzcy099a) %>% vis_miss() # corona info
d <- d %>% select(-c(hzcy097a:hzcy099a)) # remove corona info

colnames(d)
psych::describe(d[,c(10:65)])

d <- d %>% mutate(hzcy001a = recode(hzcy001a,
                                    `97` = 8),
                  hzcy002a = recode(hzcy002a,
                                    `97` = 8),
                  hzcy004a = recode(hzcy004a,
                                    `97` = 8),
                  hzcy005a = recode(hzcy005a,
                                    `97` = 8))

psych::describe(d[,c(10:65)])

d <- d %>% drop_na(intention_to_vote)
d <- d %>% mutate(choice_of_party = replace_na(choice_of_party, 8))

skimr::skim(d[,2:65])

d_temp <- d %>% select(2:65)

library(missRanger)
non_miss <- rowSums(!is.na(d))
d_temp <- d_temp %>%
  missRanger(verbose = 1, formula = . ~ ., num.trees = 100,
             maxiter = 100, pmm.k = 10,
             seed = 666, case.weights = non_miss)
d[,2:65] <- d_temp
rm(d_temp, non_miss)

skimr::skim(d[,2:65])

no_var <- d %>% select(hzcy009a:hzcy011a, hzcy015a:hzcy018a, hzcy026a, hzcy084a,
                       hzcy093a:hzcy095a)
ggplot(reshape2::melt(no_var), aes(x=value)) + geom_histogram() +
  facet_wrap(~variable) + theme_bw()

gtsummary::tbl_summary(no_var)

rm(no_var)

colnames(d)

d <- d %>% mutate(sex = recode_factor(sex, `1` = "male", `2` = "female"),
                  age_cat = recode_factor(age_cat,
                                          `1` = "25_eq_less",
                                          `2` = "26_30",
                                          `3` = "31_35",
                                          `4` = "36_40",
                                          `5` = "41_45",
                                          `6` = "46_50",
                                          `7` = "51_60",
                                          `8` = "61_65",
                                          `9` = "66_70",
                                          `10` = "71_eq_more"),
                  education_cat = recode_factor(education_cat,
                                                `1` = "low",
                                                `2` = "medium",
                                                `3` = "high"),
                  intention_to_vote = recode_factor(intention_to_vote,
                                                    `1` = "No",
                                                    `2` = "Yes"),
                  choice_of_party = recode_factor(choice_of_party,
                                                  `1` = "CDU / CSU",
                                                  `2` = "SPD",
                                                  `3` = "FDP",
                                                  `4` = "LEFT",
                                                  `5` = "GREENS",
                                                  `6` = "AfD",
                                                  `7` = "OTHER",
                                                  `8` = "UNDECIDED"),
                  marstat = recode_factor(marstat,
                                          `1` = "married",
                                          `2` = "single",
                                          `3` = "divorced",
                                          `4` = "widowed"),
                  household = recode_factor(household,
                                            `1` = "one",
                                            `2` = "two",
                                            `3` = "three_and_more"))


save(d, file = "data/data.RData")
