source("simulation/03-simulations.R")

library(multcomp)
library(MBESS)


# fit the linear model
fit_lm <- lm(bad_sentence_percentage ~ anonymity * cues, data=df)
summary(fit_lm)


# test hypotheses
## note: the overall hypothesis is a compound hypothesis, confirmed if 
## at least one of its constituent hypotheses is supported. Therefore,
## the alpha level needs to be adjusted to account for this multiple testing.
alpha <- 0.05
N <- 3   # as there is three individual hypotheses being tested
alpha_adj <- alpha / N
print(alpha_adj) # bonferroni-corrected alpha level


## define null hypotheses 
hyps_less <- c("anonymity <= 0")
hyps_greater <- c("cues >= 0", "anonymity:cues >= 0")

## glht objects 
### null hypotheses <= 0
fit_glht_less <- glht(fit_lm, hyps_less)

### null hypotheses >= 0
fit_glht_greater <- glht(fit_lm, hyps_greater)

## results
### use adjusted alpha here (alpha_adj)
summary(fit_glht_less, test=univariate())
summary(fit_glht_greater, test=univariate())


# effect sizes and their 95% CIs
## R^2
summary_fit <- summary(fit_lm)
R2 <- round(summary_fit$r.squared, 2)
print(R2)
ci.R2(R2 = R2, p = 3, N = nrow(df)) 

## beta_zj 
fit_z <- lm(scale(bad_sentence_percentage) ~ scale(anonymity) * scale(cues), data = df)
summary_fit_z <- summary(fit_z)
bz_anonymity <- round(summary_fit_z$coefficients[2,1],2)
print(bz_anonymity)
bz_cues <- round(summary_fit_z$coefficients[3,1],2)
print(bz_cues)
bz_interaction <- round(summary_fit_z$coefficients[4,1],2)
print(bz_interaction)
confint(fit_z)





