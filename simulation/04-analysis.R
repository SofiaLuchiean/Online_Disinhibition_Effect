source("simulation/03-simulations.R")

library(multcomp)
library(MBESS)


# fit the linear model
fit_lm <- lm(bad_sentence_percentage ~ MOD + anonymity * cues, data=df)
summary(fit_lm)


# test hypotheses
## define null hypotheses 
hyps_less <- c("anonymity <= 0", "MOD <= 0")
hyps_greater <- c("cues >= 0", "anonymity:cues >= 0")

## glht objects 
### null hypotheses <= 0
fit_glht_less <- glht(fit_lm, hyps_less)

### null hypotheses >= 0
fit_glht_greater <- glht(fit_lm, hyps_greater)

## results
summary(fit_glht_less, test=univariate())
summary(fit_glht_greater, test=univariate())


# effect sizes and their 95% CIs
## R^2
summary_fit <- summary(fit_lm)
R2 <- round(summary_fit$r.squared, 2)
ci.R2(R2 = R2, p = 4, N = nrow(df)) 

## beta_zj 
fit_z <- lm(scale(bad_sentence_percentage) ~ scale(MOD) + scale(anonymity) * scale(cues), data = df)
summary_fit_z <- summary(fit_z)
bz_MOD <- round(summary_fit_z$coefficients[2,1],2)
bz_anonymity <- round(summary_fit_z$coefficients[3,1],2)
bz_cues <- round(summary_fit_z$coefficients[4,1],2)
bz_interaction <- round(summary_fit_z$coefficients[5,1],2)
confint(fit_z)


# 





