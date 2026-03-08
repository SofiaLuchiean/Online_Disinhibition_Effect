# goal: analyze how the parameter base responsibility (base_resp) 
# impacts the phenomenon being produced by the formal model
# question: for which values of base_resp does the model (still) produce 
# the phenomenon in a simulated experiment?

library(ggplot2)
library(multcomp)
source("simulation/01-functions.R")

# 1. deterministic model
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
  geom_point() +
  labs(
    x="Anonymity",
    y="State Disinhibition",
    color="Interpersonal cues"
  ) +
  theme_bw()

## the deterministic model always produces the effect, since base_resp has no influence
## on the main effect of cues, which is built into the mathematical model 

# -------------------------------------------------------------------

# 2. simulation of an experiment for different base_resp values
n = 30  # number of individuals per condition
df_exp <- expand.grid(
  id = 1:n,
  anonymity = c(0.2, 0.8),
  cues = c(0.2, 0.8),
  base_resp = seq(0, 1, by = 0.1)
)
df_exp$MOD <- round((rbeta(nrow(df_exp), 1.6, 1.7)*4 + 1), 2) 
df_exp$bad_sentence_percentage <- round(curse_function(df_exp$anonymity, df_exp$cues, df_exp$MOD, df_exp$base_resp),2) 

## boxplot visualisation
ggplot(df_exp, aes(x = factor(anonymity), y = bad_sentence_percentage, color = factor(cues)))+
  geom_boxplot() + 
  facet_wrap(~ base_resp)
  labs(
    x="Anonymity",
    y="Bad sentence percentage",
    color="Interpersonal cues"
  ) +
  theme_bw()


# 3. statistical analysis
## which values of base responsibility (still) produce the phenomenon?

## create empty table for results
df_results <- data.frame(
   base_resp = numeric(0),
   significance = logical(0)
)

## null hypotheses 
hyps_less <- c("anonymity <= 0")
hyps_greater <- c("cues >= 0", "anonymity:cues >= 0")

## corrected alpha level for multiple testing
alpha <- 0.05
N <- 3   # as there is three individual hypotheses being tested
alpha_adj <- alpha / N

## created a function for statistical analysis:
#' Test composite hypothesis for a given base_resp value
#' 
#' This function evaluates whether the phenomenon of online disinhibition
#' is produced for a given  value of `base_resp`. 
#' It subsets the dataset to the selected value of `base_resp`, fits a linear 
#' model predicting `bad_sentence_percentage` from `anonymity`, `cues`, and their 
#' interaction, and tests three hypotheses: main effects of anonymity and cues 
#' respectively and their interaction effect. The composite hypothesis is considered 
#' supported if at least one of the individual hypothesis tests yields a p-value 
#' equal or smaller to the adjusted significance level (`alpha_adj`).
#'
#' @param df A data frame containing the experimental data.
#' @param base_resp_value Numeric value specifying which level of `base_resp` 
#'  should be tested. The value must correspond to an existing value in the
#'  `base_resp` column of `df`.
#' @param alpha_adj Adjusted significance level used for the hypothesis tests.
#' 
#' @return A logical value (`TRUE` or `FALSE`). `TRUE` indicates that at least
#'  one of the tested hypotheses is significant at the adjusted alpha level, thus
#'  confirming the composite hypothesis. In other words, the result supports 
#'  the presence of the online disinhibition effect.
#'       
significance_test <- function(df, base_resp_value, alpha_adj) {
  
  # select only data for the desired base_resp
  # allowing small tolerance to prevent errors with floating numbers
  df_exp_subset <- df[abs(df$base_resp - base_resp_value) < 1e-8, ]
  
  # fit model
  fit_lm_exp <- lm(bad_sentence_percentage ~ anonymity * cues, data=df_exp_subset)
  
  # formulate hypotheses
  hyps_less <- c("anonymity <= 0")
  hyps_greater <- c("cues >= 0", "anonymity:cues >= 0")
  
  # compute glht objects for null hypotheses
  fit_glht_less <- glht(fit_lm_exp, hyps_less)
  fit_glht_greater <- glht(fit_lm_exp, hyps_greater)
  
  # summary
  summary_h1 <- summary(fit_glht_less, test=univariate())
  summary_h2_h3 <- summary(fit_glht_greater, test=univariate())
  
  # extract p-values
  p_h1 <- as.numeric(summary_h1$test$pvalues)
  p_h2 <- as.numeric(summary_h2_h3$test$pvalues[1])
  p_h3 <- as.numeric(summary_h2_h3$test$pvalues[2])
  p_values <- c(p_h1, p_h2, p_h3)
  
  # significance test of composite hypothesis
  significance <- any(p_values <= alpha_adj)
  
  #result
  return(significance)
}

## testing for multiple base_resp values
### specify base_resp values from the dataframe
br_values <- seq(0, 1, by = 0.1)

### set up dataframe for results
df_results <- data.frame(base_resp = br_values) 

### simulations
n_sim <- 100   # number of simulation runs

for (i in 1:n_sim) {
  # simulate new data for each iteration 
  df_exp <- expand.grid(
    id = 1:n, 
    anonymity = c(0.2, 0.8),
    cues = c(0.2, 0.8),
    base_resp = seq(0, 1, by = 0.1)
  )
  df_exp$MOD <- round((rbeta(nrow(df_exp), 1.6, 1.7)*4 + 1), 2) 
  df_exp$bad_sentence_percentage <- round(curse_function(df_exp$anonymity, df_exp$cues, df_exp$MOD, df_exp$base_resp),2) 
  
  # test significance for each base_resp 
  results <- sapply(br_values, function(b) {
    significance_test(df_exp, b, alpha_adj)
  })
  
  # add results as a new column for each simulation iteration
  df_results[[paste0("Significance test ", i)]] <- results
}

### compute the percentage of TRUE per row
sim_cols <- grep("^Significance test", names(df_results))
df_results$TRUE_percentage <- rowMeans(df_results[, sim_cols])

