# goal: systematically analyze how the parameter base responsibility 
# (base_resp) impacts the phenomenon being produced by the formal model
# question: for which values of base_resp does the model (still) produce 
# the phenomenon?

library(ggplot2)
source("simulation/01-functions.R")

# simulation of the deterministic model
## define levels to be plotted 
## covering the entire range of base_resp in small intervals
df_iop <- expand.grid(
  anonymity = c(0, 0.5, 1),
  cues = c(0, 0.5, 1),
  MOD = 2.9, #mean based on results from validation study
  base_resp = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
)

## compute state disinhibition 
df_iop$bad_sentence_percentage <- round(psi_function(
  df_iop$anonymity, 
  df_iop$cues, 
  df_iop$MOD, 
  df_iop$base_resp
),2) 

## plot for visualisation
ggplot(df_iop, aes(x= anonymity, y = bad_sentence_percentage, color = factor(cues))) +
  facet_wrap(~ base_resp) +
  geom_line() +
  labs(
    x="Anonymity",
    y="Bad sentence percentage",
    color="Interpersonal cues"
  ) +
  theme_bw()


# simulation of the phenomenon for different base_resp values
n = 10

## base_resp = 0
df_br_0 <- expand.grid(
  id = 1:n,
  anonymity = c(0, 0.5, 1),
  cues = c(0, 0.5, 1),
  base_resp = 0
)
df_br_0$MOD <- round((rbeta(nrow(df_br_0), 1.6, 1.7)*4 + 1), 2) 
df_br_0$bad_sentence_percentage <- round(curse_function(df_br_0$anonymity, df_br_0$cues, df_br_0$MOD, df_br_0$base_resp),2) 



# barplot visualisation
n = 10
df_barplot <- expand.grid(
  id = 1:n,
  anonymity = c(0.2, 0.8),
  cues = c(0.2, 0.8),
  base_resp = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
)
df_barplot$MOD <- round((rbeta(nrow(df_barplot), 1.6, 1.7)*4 + 1), 2) 
df_barplot$bad_sentence_percentage <- round(curse_function(df_barplot$anonymity, df_barplot$cues, df_barplot$MOD, df_barplot$base_resp),2) 

ggplot(df_barplot, aes(x = factor(base_resp), y = bad_sentence_percentage, fill = factor(cues))) +
  stat_summary(
    fun = mean, geom = "bar",
    position = position_dodge(width = 0.9)
  ) +
  facet_wrap(~ anonymity, ncol = 1) +
  labs(
    x = "Base responsibility",
    y = "Bad sentence percentage",
    fill = "cues"
  ) +
  theme_bw()

