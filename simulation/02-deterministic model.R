source("simulation/01-functions.R")
library(ggplot2)

# plot of the deterministic model output: state disinhibition
## define levels to be plotted
plot_data <- expand.grid(
  anonymity = c(0, 0.25, 0.5, 0.75, 1), #experimental factor
  MOD = c(1, 3, 5),         #measured trait variable
  cues = c(0, 0.5, 1),      #experimental factor
  base_resp = c(0.5, 0.9)   #measured trait variable
)

## compute outcome variable
plot_data$bad_sentence_percentage <- psi_function(
  plot_data$anonymity, 
  plot_data$cues, 
  plot_data$MOD, 
  plot_data$base_resp
  )

# plot
ggplot(plot_data, aes(x= anonymity, y = bad_sentence_percentage, color = factor(cues))) +
  facet_grid(base_resp ~ MOD) +
  geom_line() +
  labs(
    x="Anonymity",
    y="Bad sentence percentage",
    color="Interpersonal cues"
  ) +
  theme_bw()




