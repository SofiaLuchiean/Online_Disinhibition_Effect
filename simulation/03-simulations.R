# simulating data
source("simulation/01-functions.R")
library(ggplot2)
set.seed(4)


n = 35 # n per condition
df <- expand.grid(
  id = 1:n,
  anonymity = c(0.2, 0.8),
  cues = c(0.2, 0.8)
)

df$MOD <- round((rbeta(nrow(df), 1.6, 1.7)*4 + 1), 2) #simulated MOD values

df$base_resp <- round(rbeta(nrow(df), 6.6, 1.38), 2) #simulated base responsibility values

df$bad_sentence_percentage <- round(curse_function(df$anonymity, df$cues, df$MOD, df$base_resp),2) #simulated disinhibited behavior


# descriptive statistics
## AN 0.2, IC 0.2
mean(df[df$anonymity == 0.2 & df$cues == 0.2, ]$bad_sentence_percentage)
sd(df[df$anonymity == 0.2 & df$cues == 0.2, ]$bad_sentence_percentage)

## AN 0.8, IC 0.2
mean(df[df$anonymity == 0.8 & df$cues == 0.2, ]$bad_sentence_percentage)
sd(df[df$anonymity == 0.8 & df$cues == 0.2, ]$bad_sentence_percentage)

## AN 0.2, IC 0.8
mean(df[df$anonymity == 0.2 & df$cues == 0.8, ]$bad_sentence_percentage)
sd(df[df$anonymity == 0.2 & df$cues == 0.8, ]$bad_sentence_percentage)

## AN 0.8, IC 0.8
mean(df[df$anonymity == 0.8 & df$cues == 0.8, ]$bad_sentence_percentage)
sd(df[df$anonymity == 0.8 & df$cues == 0.8, ]$bad_sentence_percentage)


# plots with simulated data
p1 = ggplot(df, aes(x = anonymity, y = bad_sentence_percentage, color = factor(cues))) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange",
               position = position_dodge(width = 0.1)) +
  stat_summary(fun = mean, geom = "line",
               position = position_dodge(width = 0.1)) +
  labs(
    x="Anonymity",
    y="Curse word sentence percentage",
    color="Interpersonal cues"
  ) +
  theme_minimal()
print(p1) # plot with anonymity and cues as IV showing 
          # the mean differences between groups

p2 = ggplot(df, aes(x = factor(anonymity), y = bad_sentence_percentage, color = factor(cues)))+
  geom_boxplot() + 
  labs(
    x="Anonymity",
    y="Percentage of curse words",
    color="Interpersonal cues"
  ) +
  theme_minimal()
print(p2) # box plot for visualizing distribution of the outcome 
          # variable between groups
ggsave("simulation/plots/box_plot_simulation.png")


