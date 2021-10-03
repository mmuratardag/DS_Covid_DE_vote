
load("data/data_profile.RData")

library(tidyverse)

df_temp <- d %>% select(cov_inf_prob:profile) %>%
  mutate_each_(funs(scale),vars=c(names(d[,35:41]))) %>%
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
ct %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

p2 <- ct %>% pivot_longer(!choice_of_party, names_to = "profile", values_to = "count") %>%
  ggplot(aes(fill = profile, y = count, x = choice_of_party)) + 
  geom_bar(position = "fill", stat = "identity") +
  ggsci::scale_fill_lancet() +
  xlab("Political Parties") +
  ylab("Profile %") + theme_bw() +
  labs(title = "% of Profiles by Voting Intention")

gridExtra::grid.arrange(p1, p2,
                        top = "Note that this is a nationally representative sample of voters from April 2020",
                        bottom = "Profile 1 seems to be the most noncompliant: 29 % of this profile intended to vote for OTHER parties that are not listed here,
                        \nfollowed by 23 % intended to vote for the AfD, 21 % for FDP, 19 % reported to be UNDECIDED, 18 % intended to vote for the LINKE.")
