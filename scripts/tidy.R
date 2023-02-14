library(here)
library(brms)
library(tidyverse)

exp_one = read.csv(here("docs", "exp_one.csv")) %>% 
  filter(!is.na(version)) %>% 
  mutate(context = case_when(
    context == "NEUTRAL" ~ "NEU",
    context == "NEU" ~ "NEU",
    context == "REL" ~ "REL"
  ))

exp_two = read.csv(here("docs", "exp_two.csv"))

exp_one %>%
  group_by(conjunct, monotonicity, context) %>% 
  summarise(mean_r = mean(Rating), sd_r = sd(Rating)) %>% 
  ggplot(aes(x=conjunct, y= mean_r, fill=context)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) + facet_grid(~monotonicity) +
  geom_errorbar(aes(ymin= mean_r - sd_r, ymax= mean_r + sd_r), width=.2,
                position=position_dodge(.9)) +
  theme_minimal() +
  xlab("Conjunction") + ylab("Mean Rating") +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(title = "Mean Ratings by Conjunction, montonicity and context",
         subtitle = "Experiment 1",
         caption = "
       The bars preresent the mean rating of AND and BUT in sentences 
       with both OPPOSITE and SAME montonicity and in NEUTRAL and 
       SUPPORTING contexts. The error bars were calculated using +/- 1 
       standard deviation from the mean")


exp_two %>%
  group_by(conjunct, order, only.present.) %>% 
  summarise(mean_r = mean(Rating), sd_r = sd(Rating)) %>% 
  ggplot(aes(x=conjunct, y= mean_r, fill=order)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) + facet_grid(~only.present.) +
  geom_errorbar(aes(ymin= mean_r - sd_r, ymax= mean_r + sd_r), width=.2,
                position=position_dodge(.9)) +
  theme_minimal() +
  xlab("Conjunction") + ylab("Mean Rating") +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(title = "Mean Ratings by Conjunction, order and the presence of only",
       subtitle = "Experiment 2",
       caption = "
       The bars preresent the mean rating of AND and BUT in sentences 
       with both WEAK-STRONG and STRONG-WEAK scalar order and when  
       ONLY was present or not present. The error bars were calculated using +/- 1 
       standard deviation from the mean")


mod_one = brm(as.integer(Rating) ~ conjunct*monotonicity*context + (1 | item.number) + 
      (1 | version), data = exp_one,
      family = cumulative())

conditional_effects(mod_one)