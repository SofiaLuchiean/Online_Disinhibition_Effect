# source functions
source("simulation/01-functions.R")
library(ggplot2)
set.seed(4)

# simulate data
n = 35 # n per condition
df <- expand.grid(
  id = 1:n,
  anonymity = c(0, 1),
  cues = c(0, 1)
)

#simulate MOD values
df$MOD <- round((rbeta(nrow(df), 1.6, 1.7)*4 + 1), 2) 

#simulate base responsibility values
df$base_resp <- round(rbeta(nrow(df), 6.6, 1.38), 2) 

#simulate disinhibited behavior
df$bad_sentence_percentage <- round(curse_function(df$anonymity, df$cues, df$MOD, df$base_resp),2) 

