# source simulated data
source("simulation/03-simulations.R")


# plots with simulated data
p1 = ggplot(df, aes(x = anonymity, y = bad_sentence_percentage, color = factor(cues))) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange",
               position = position_dodge(width = 0.1)) +
  stat_summary(fun = mean, geom = "line",
               position = position_dodge(width = 0.1)) +
  labs(
    x="Anonymity",
    y="Percentage of curse words",
    color="Interpersonal cues"
  ) +
  theme_minimal()
print(p1) # plot with anonymity and cues as IV showing 
          # the mean differences between groups
ggsave("simulation/plots/means_plot_simulation.png")

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


