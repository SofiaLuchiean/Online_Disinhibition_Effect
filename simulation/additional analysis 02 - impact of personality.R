# goal: systematically analyze how the parameter base responsibility 
# (base_resp) impacts the phenomenon being produced by the formal model
# question: for which values of base_resp does the model (still) produce 
# the phenomenon in a simulated experiment?

library(ggplot2)
source("simulation/01-functions.R")

# deterministic model
## define levels to be plotted 
## covering the entire range of base_resp in small intervals
df_det <- expand.grid(
  anonymity = c(0.2, 0.8),
  cues = c(0.2, 0.8),
  MOD = 2.9, # mean MOD-score in the population
  base_resp = seq(0, 1, by = 0.1)
  )

## compute state online disinhibition
df_det$state_disinhibition <- round(psi_function(
  df_det$anonymity, 
  df_det$cues, 
  df_det$MOD, 
  df_det$base_resp
),2) 

## plot for visualisation
ggplot(df_det, aes(x= anonymity, y = state_disinhibition, color = factor(cues))) +
  facet_wrap(~ base_resp) +
  geom_line() +
  labs(
    x="Anonymity",
    y="Bad sentence percentage",
    color="Interpersonal cues"
  ) +
  theme_bw()


# simulation of an experiment for different base_resp values
n = 10
df_exp <- expand.grid(
  id = 1:n,
  anonymity = c(0.2, 0.8),
  cues = c(0.2, 0.8),
  base_resp = seq(0, 1, by = 0.1)
)
df_exp$MOD <- round((rbeta(nrow(df_exp), 1.6, 1.7)*4 + 1), 2) 
df_exp$bad_sentence_percentage <- round(curse_function(df_exp$anonymity, df_exp$cues, df_exp$MOD, df_exp$base_resp),2) 


# barplot visualisation
ggplot(df_exp, aes(x = factor(anonymity), y = bad_sentence_percentage, fill = factor(cues))) +
  stat_summary(
    fun = mean, geom = "bar",
    position = position_dodge(width = 0.9)
  ) +
  facet_wrap(~ base_resp) +
  labs(
    x = "Anonymity",
    y = "Bad sentence percentage",
    fill = "cues"
  ) +
  theme_bw()

#boxplot visualisation
ggplot(df_exp, aes(x = factor(anonymity), y = bad_sentence_percentage, color = factor(cues)))+
  geom_boxplot() + 
  facet_wrap(~ base_resp)
  labs(
    x="Anonymity",
    y="Bad sentence percentage",
    color="Interpersonal cues"
  ) +
  theme_bw()

# statistical analysis
## testing to see for which values of base responsibility the model still
## shows a main effect of anonymity and invisibility as well as an
## interaction effect
  


  
  
  