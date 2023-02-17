library(here)
library(brms)
library(tidyverse)
library(janitor)
library(bayestestR)
library(bayesplot)
library(tidybayes)
library(modelr)

# Load and tidy data 
exp_one = read.csv(here("docs", "exp_one.csv")) %>% 
  filter(!is.na(version)) %>% 
  mutate(context = case_when(
    context == "NEUTRAL" ~ "NEU",
    context == "NEU" ~ "NEU",
    context == "REL" ~ "REL"
  ))

exp_two = read.csv(here("docs", "exp_two.csv"))

# Create plots of descriptive data 
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
       standard deviation from the mean") +
  ggsave(here("plots", "exp_one.png"))


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
       standard deviation from the mean") +
  ggsave(here("plots", "exp_two.png"))



# Fit models 
mod_one <- brm(
  formula = Rating ~ conjunct*monotonicity*context +
  (1 | item.number) + (1 | participants), 
  data = exp_one, 
  family = cumulative("probit"), 
  file = here("models", "mod_one.RDS"))


data_grid_one_neu = data_grid_one %>% 
  rename("neu_estimate" = ".value") %>% 
  filter(context == "NEU") %>% 
  select(conjunct, monotonicity, context, Rating, neu_estimate, .draw)

data_grid_one_rel = data_grid_one %>% 
  rename("rel_estimate" = ".value") %>% 
  filter(context == "REL") %>% 
  select(conjunct, monotonicity, context, Rating, rel_estimate, .draw)  


data_grid_two_only = data_grid_two %>% 
  rename("estimate_only" = ".value") %>% 
  filter(only.present. == "ONLY") 

data_grid_two_no = data_grid_two %>% 
  rename("estimate_no" = ".value") %>% 
  filter(only.present. == "NO") 

data_grid_es_only = left_join(data_grid_two_only, data_grid_two_no,
                         by = c(".draw")) %>% 
  mutate(es = estimate_only - estimate_no)

data_grid_es = left_join(data_grid_one_neu, data_grid_one_rel,
                         by = c(".draw")) %>% 
  mutate(es = neu_estimate - rel_estimate)



mod_two <- brm(
  formula = Rating ~ conjunct*order*only.present. +
    (1 | item.number) + (1 | participants), 
  data = exp_two, 
  family = cumulative("probit"), 
  file = here("models", "mod_two.RDS"))

# Tidy model output for more plots 

data_grid_one = exp_one %>%
  data_grid(conjunct, monotonicity, context) %>%
  add_fitted_draws(mod_one, dpar = TRUE, category = "Rating",
                   re_formula = NA) 

summary(mod_one)



data_grid_one %>% 
  rename("estimate" = ".value") %>% 
  ggplot(aes(y = context, x = estimate, fill = monotonicity)) +
  stat_halfeye(alpha = .5) +
  theme_minimal() + 
  geom_vline(xintercept = 0, linetype = "dashed", alpha = .4) +
  facet_grid(Rating~conjunct) +
  ggsave(here("plots", "forest_plot_one.png"))





data_grid_two = exp_two %>%
  data_grid(conjunct, order, only.present.) %>%
  add_fitted_draws(mod_two, dpar = TRUE, category = "Rating",
                   re_formula = NA) 

data_grid_two %>% 
  rename("estimate" = ".value") %>% 
  ggplot(aes(y = only.present., x = estimate, fill = order)) +
  stat_halfeye(alpha = .5) +
  theme_minimal() + 
  geom_vline(xintercept = 0, linetype = "dashed", alpha = .4) +
  facet_grid(Rating~conjunct) +
  ggsave(here("plots", "forest_plot_two.png"))

desc_df_two = data_grid_two %>% 
  rename("estimate" = ".value") %>% 
  group_by(conjunct, only.present., order, Rating) %>% 
  summarize(mean_hdi = round(mean(estimate), digits = 3),
            hdi_low = round(hdi(estimate)[,1], digits = 3),
            hdi_hi = round(hdi(estimate)[,2], digits = 3)) 


desc_df_two %>% 
  ggplot(aes(x=conjunct, y= mean_hdi, fill=only.present.)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) + facet_grid(order~Rating) +
  geom_errorbar(aes(ymin= hdi_low, ymax= hdi_hi), width=.2,
                position=position_dodge(.9)) +
  theme_minimal()

data_grid_es_only %>% 
  ggplot(aes(y = conjunct.x, x = es, fill = conjunct.x)) +
  stat_halfeye(alpha = .5) +
  theme_minimal() + 
  geom_vline(xintercept = 0, linetype = "dashed", alpha = .4) +
  facet_grid(Rating.x ~ order.y) + 
  ylab("Conjunction") + xlab("Difference in probability") +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(title = "The difference between ONLY and NO in each condition",
       subtitle = "Experiment 2",
       caption = "
       The plot represents the difference in probability as a function of 
       the presence of only, order and conjunction") +
  theme(legend.position = "none") +
  ggsave(here("plots", "es_exp_two.png"))





data_grid_es %>% 
  rename("estimate" = "es") %>% 
  ggplot(aes(y = conjunct.x, x = estimate, fill = conjunct.x)) +
  stat_halfeye(alpha = .5) +
  theme_minimal() + 
  geom_vline(xintercept = 0, linetype = "dashed", alpha = .4) +
  facet_grid(Rating.x ~ monotonicity.x) + 
  ylab("Conjunction") + xlab("Difference in probability") +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(title = "The difference between NEU and REL in each condition",
       subtitle = "Experiment 1",
       caption = "
       The plot represents the difference in probability as a function of 
       context, conjunction and monotinicity.") +
  scale_fill_discrete(name = "New Legend Title") +
  theme(legend.position = "none") +
  ggsave(here("plots", "es_exp_one.png"))
  
