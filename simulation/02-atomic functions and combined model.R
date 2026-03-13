source("simulation/01-functions.R")
library(ggplot2)

# plots of atomic functions
## Compartmentalized Self
png(filename = "simulation/plots/CS_function.png", width = 800, height = 600)
par(mar = c(6, 6, 4, 2)) 
curve(CS_function(x), from = 0, to = 1,
      ylim = c(0,1),
      xlim = c(0,1),
      xlab = "Anonymity",
      ylab = "Compartmentalized Self",
      lwd = 3,
      col = "red3",
      cex.lab = 2.5,   # axis labels
      cex.axis = 2)  # axis numbers
title(main = "A", adj = 0, cex.main = 4)
dev.off()

## Felt Responsibility
png(filename = "simulation/plots/FR_function.png", width = 800, height = 600)
par(mar = c(6, 6, 4, 2)) 
curve(FR_function(x, 1), from = 0, to = 1,
      ylim = c(0,1),
      xlim = c(0,1),
      xlab = "Compartmentalized Self",
      ylab = "Felt Responsibility",
      lwd = 3,
      col = "blue",
      cex.lab = 2.5,   # axis labels
      cex.axis = 2)  # axis numbers
title(main = "B", adj = 0, cex.main = 4)
dev.off()

## Concern about Impression on others
png(filename = "simulation/plots/CAI_function.png", width = 800, height = 600)
par(mar = c(6, 6, 4, 2)) 
curve(CAI_function(x), from = 0, to = 1,
      ylim = c(0,1),
      xlim = c(0,1),
      xlab = "Interpersonal Cues",
      ylab = "Concern about impression on others",
      lwd  = 3,
      col = "orange",
      cex.lab = 2.5,   # axis labels
      cex.axis = 2)  # axis numbers
title(main = "C", adj = 0, cex.main = 4)
dev.off()

## Courage to express oneself
png(filename = "simulation/plots/CE_function.png", width = 800, height = 600)
par(mar = c(6, 6, 4, 2)) 
curve(CE_function(x), from = 0, to = 1,
      ylim = c(0,1),
      xlim = c(0,1),
      xlab = "Concern about impression on others",
      ylab = "Courage to express oneself",
      lwd = 3,
      col = "forestgreen",
      cex.lab = 2.5,   # axis labels
      cex.axis = 2)  # axis numbers
title(main = "D", adj = 0, cex.main = 4)
dev.off()



# plot of the combined model output: state disinhibition
## define levels to be plotted
plot_data <- expand.grid(
  anonymity = seq(0, 1, by = 0.1),      #experimental factor
  MOD = c(1, 3, 5),                     #measured trait variable
  cues = c(0, 0.5, 1),                  #experimental factor
  base_resp = c(0.5, 0.9)               #measured trait variable
)

## compute outcome variable
plot_data$state_disinhibition <- psi_function(
  plot_data$anonymity, 
  plot_data$cues, 
  plot_data$MOD, 
  plot_data$base_resp
  )

# plot
ggplot(plot_data, aes(x= anonymity, y = state_disinhibition, color = factor(cues))) +
  facet_grid(base_resp ~ MOD) +
  geom_line() +
  labs(
    x="Anonymity",
    y="State Online Disinhibition",
    color="Interpersonal cues"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(size = 7),   
    axis.text.y  = element_text(size = 7)    
  )
ggsave("simulation/plots/model_output.png")



