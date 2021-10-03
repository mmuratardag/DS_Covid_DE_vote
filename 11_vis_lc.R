
load("data/data_class.RData")
colnames(d)

library(tidyverse)
library(janitor)
ct <- d %>% tabyl(choice_of_party, class)
p1 <- ct %>% pivot_longer(!choice_of_party, names_to = "class", values_to = "count") %>%
  ggplot(aes(fill = class, y = count, x = choice_of_party)) + 
  geom_bar(position = "fill", stat = "identity") +
  xlab("Political Parties") +
  ylab("Class %") + 
  labs(title="% of COVID-19 Skeptics by Voting Intention",
       subtitle = "Class 1: the Skeptic Group; Class 2: The Compliant Group",
       caption = "Class 1 is low 
       \n(1) in their belief, support for the effectiveness measures taken by the government,
       \n(2) in their trust fof the institutions like the Federal Government, Robert Koch Institute, WHO, Scientist") +
  theme_bw()

p2 <- d %>% select(intention_to_vote, political_orientation,
             hzcy001a:hzcy095a, class) %>%
  mutate(intention_to_vote = as.numeric(intention_to_vote)) %>% 
  mutate_each_(funs(scale),vars=c(names(d[,c(5,7,10:65)]))) %>%
  group_by(class) %>% summarize_all(mean) %>%
  pivot_longer(!class, names_to = "indicator", values_to = "mean") %>%
  mutate(class = recode_factor(class, `1` = "Class 1", `2` = "Class 2")) %>%
  ggplot(aes(x = indicator, y = mean,
                    group = class, color = class)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

gridExtra::grid.arrange(p2, p1,
                        top = "Model Evaluation Metrics")

ct %>% pivot_longer(!choice_of_party, names_to = "class", values_to = "count") %>%
  ggplot(aes(fill = class, y = count, x = choice_of_party)) + 
  geom_bar(position = "fill", stat = "identity") +
  xlab("Political Parties") +
  ylab("Class %") + 
  labs(title="% of COVID-19 Skeptics by Voting Intention",
       subtitle = "Class 1: the Skeptic Group; Class 2: The Compliant Group",
       caption = "Class 1 is low 
       \n(1) in their belief, support for the effectiveness measures taken by the government,
       \n(2) in their trust fof the institutions like the Federal Government, Robert Koch Institute, WHO, Scientist") +
  theme_bw()
