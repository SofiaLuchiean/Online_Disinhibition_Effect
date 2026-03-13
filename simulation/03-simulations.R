#1. simulating data
source("simulation/01-functions.R")
library(ggplot2)
set.seed(41)


n = 30
df <- expand.grid(
  id = 1:n,
  anonymity = c(0.2, 0.8),
  cues = c(0.2, 0.8)
)

df$MOD <- round((rbeta(nrow(df), 1.6, 1.7)*4 + 1), 2) #simulated MOD values

df$base_resp <- round(rbeta(nrow(df), 6.6, 1.38), 2) #simulated base responsibility values

df$bad_sentence_percentage <- round(curse_function(df$anonymity, df$cues, df$MOD, df$base_resp),2) #simulated disinhibited behavior


# 2. plots with simulated data
p1 = ggplot(df, aes(x = anonymity, y = bad_sentence_percentage, color = factor(cues))) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange",
               position = position_dodge(width = 0.1)) +
  stat_summary(fun = mean, geom = "line",
               position = position_dodge(width = 0.1)) +
  theme_minimal()
print(p1) # original plot, with only anonymity and cues as IV showing 
          #the mean differences between groups

p2 = ggplot(df, aes(x = factor(anonymity), y = bad_sentence_percentage, color = factor(cues)))+
  geom_boxplot() + 
  labs(
    x="Anonymity",
    y="Bad sentence percentage",
    color="Interpersonal cues"
  ) +
  theme_bw()
print(p2) # box plot for visualizing distribution of the outcome 
          # variable between groups

p3 = ggplot(df, aes(x= MOD, y = bad_sentence_percentage, color = factor(cues))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ anonymity) + 
  ylim(0,1) +
  xlim(1,5) +
  labs(
    x="MOD",
    y="Bad sentence percentage",
    color="Interpersonal cues"
  ) +
  theme_bw()
print(p3) # scatterplot with visualized MOD as predictor 
