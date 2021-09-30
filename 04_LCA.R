
cores <- parallel::detectCores(logical = F)
doParallel::registerDoParallel(cores = cores-1)

load("data/data.RData")
colnames(d)

library(tidyverse)
d <- d %>% mutate(intention_to_vote = as.numeric(intention_to_vote))

library(mclust)
set.seed(666)
mc_res <- Mclust(d[,c(5,7,10:65)])
summary(mc_res)
library(factoextra)
fviz_mclust(mc_res, "BIC", palette = "jco")
#mc_res_LRT <- mclustBootstrapLRT(d[,c(5,7,10:65)], modelName = "EEV", maxG = 4)

class_indicators <- d[,c(5,7,10:65)] + 1

library(poLCA)
attach(d)
f <- cbind(intention_to_vote, political_orientation,
           hzcy001a, hzcy002a, hzcy003a, hzcy004a, hzcy005a,
           hzcy006a, hzcy007a, hzcy008a, hzcy009a, hzcy010a,
           hzcy011a, hzcy012a, hzcy013a, hzcy014a, hzcy015a,
           hzcy016a, hzcy018a, hzcy019a, hzcy020a, hzcy021a,
           hzcy022a, hzcy023a, hzcy024a, hzcy025a, hzcy026a,
           hzcy027a, hzcy028a, hzcy029a, hzcy030a, hzcy031a,
           hzcy032a, hzcy040a, hzcy041a, hzcy042a, hzcy043a,
           hzcy044a, hzcy045a, hzcy046a, hzcy047a, hzcy048a,
           hzcy049a, hzcy050a, hzcy051a, hzcy052a, hzcy053a,
           hzcy084a, hzcy085a, hzcy086a, hzcy087a, hzcy088a,
           hzcy089a, hzcy090a, hzcy091a, hzcy092a, hzcy093a,
           hzcy095a)~1

set.seed(666)
lca_res <- poLCA(f,
                 nclass = 2,
                 data = class_indicators,
                 nrep = 100,
                 na.rm = F,
                 graphs = F,
                 maxiter = 1000)

d$class <- lca_res$predclass
colnames(d)

wide <- d %>% dplyr::select(intention_to_vote, political_orientation,
                     hzcy001a:hzcy095a, class) %>%
  mutate_each_(funs(scale),vars=c(names(d[,c(5,7,10:65)]))) %>%
  group_by(class) %>% summarize_all(mean)
long <- wide %>%
  pivot_longer(!class, names_to = "indicator", values_to = "mean") %>%
  mutate(class = recode_factor(class, `1` = "Class 1", `2` = "Class 2"))

long %>% ggplot(aes(x = indicator, y = mean,
                    group = class, color = class)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

d <- d %>% mutate(class = recode_factor(class,
                                        `1` = "Class1",
                                        `2` = "Class2"),
                  intention_to_vote = recode_factor(intention_to_vote,
                                      `1` = "No",
                                      `2` = "Yes"))

skimr::skim(d)

save(d, file = "data/data_class.RData")
