
load("data/data.RData")
colnames(d)
library(tidyverse)
library(EGAnet)

all_items_nw <- d %>% select(hzcy001a:hzcy095a)
all_items_nw_ega <- EGA(all_items_nw, algorithm = "louvain", plot.EGA = F)
plot(all_items_nw_ega, theme = "colorblind", title = "All", legend = F)

rm(all_items_nw, all_items_nw_ega)

get_ega_op <- function(da_fr) {
  ega_output <- EGAnet::EGA(da_fr, algorithm = "louvain", plot.EGA = F)
  return(ega_output)
}

# probability of cov infection
cov_inf <- d %>% select(hzcy001a:hzcy005a)
plot(get_ega_op(cov_inf)) 
cov_inf_scores <- net.scores(cov_inf, A = get_ega_op(cov_inf))
d$cov_inf_score <- cov_inf_scores$std.scores$`1`

rm(cov_inf, cov_inf_scores)

d <- d %>% select(-c(hzcy001a:hzcy005a))

colnames(d)

# measures & actions taken
ma_tak <- d %>% select(hzcy006a:hzcy018a)
plot(get_ega_op(ma_tak))

ma_tak_la <- UVA(data = ma_tak, model = "glasso", method = "wTO",
                 type = "adapt",
                 reduce = T, reduce.method = "latent",
                 adhoc = T)
ma_tak_la_red <- data.frame(ma_tak_la[["reduced"]][["data"]])

rm(ma_tak, ma_tak_la)

d <- d %>% select(-c(hzcy006a:hzcy018a)) %>% bind_cols(ma_tak_la_red)
rm(ma_tak_la_red)

colnames(d)

# effectiveness of measures
ef_me <- d %>% select(hzcy019a:hzcy025a)
plot(get_ega_op(ef_me))
ef_me_scores <- net.scores(ef_me, A = get_ega_op(ef_me))
d$ef_me_score <- ef_me_scores$std.scores$`1`

rm(ef_me, ef_me_scores)

d <- d %>% select(-c(hzcy019a:hzcy025a))

colnames(d)

# compliance with curfew
co_cu <- d %>% select(hzcy027a:hzcy032a)
plot(get_ega_op(co_cu))

co_cu_la <- UVA(data = co_cu, model = "glasso", method = "wTO",
                type = "adapt",
                reduce = T, reduce.method = "latent",
                adhoc = T)
co_cu_la_red <- data.frame(co_cu_la[["reduced"]][["data"]])

rm(co_cu, co_cu_la)

d <- d %>% select(-c(hzcy026a:hzcy032a)) %>% bind_cols(co_cu_la_red)

rm(co_cu_la_red)

colnames(d)

# effectiveness Government Measures
ef_gov_me <- d %>% select(hzcy040a:hzcy043a)
plot(get_ega_op(ef_gov_me))
ef_gov_me_scores <- net.scores(ef_gov_me, A = get_ega_op(ef_gov_me))
d$ef_gov_me_score <- ef_gov_me_scores$std.scores$`1`

rm(ef_gov_me, ef_gov_me_scores)

d <- d %>% select(-c(hzcy040a:hzcy043a))
colnames(d)

# trust
trs <- d %>% select(hzcy044a:hzcy052a)
plot(get_ega_op(trs))

# trs_la <- UVA(data = trs, model = "glasso", method = "wTO",
#               type = "adapt",
#               reduce = T, reduce.method = "latent",
#               adhoc = T)

trs_nw_sco <- net.scores(data = trs, A = get_ega_op(trs))
d$trs_nw_sco1 <- trs_nw_sco$std.scores$`1`
d$trs_nw_sco2 <- trs_nw_sco$std.scores$`2`
d$trs_nw_sco3 <- trs_nw_sco$std.scores$`3`

rm(trs, trs_nw_sco)

d <- d %>% select(-c(hzcy044a:hzcy052a))

colnames(d)

# media consumption on Corona
med_con <- d %>% select(hzcy084a:hzcy095a)
plot(get_ega_op(med_con))

med_con_la <- UVA(data = med_con, model = "glasso", method = "wTO",
                  type = "adapt",
                  reduce = T, reduce.method = "latent",
                  adhoc = T)
med_con_la_red <- data.frame(med_con_la[["reduced"]][["data"]])

rm(med_con, med_con_la)

d <- d %>% select(-c(hzcy084a:hzcy095a)) %>% bind_cols(med_con_la_red)

colnames(d)

rm(med_con_la_red)

skimr::skim(d)

save(d, file = "data/data_fe.RData")
