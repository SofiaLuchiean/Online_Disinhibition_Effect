# goal: model accountability distribution in the population
# raw data from https://osf.io/gv6zs/files/tmkaw

# Witvliet, C. V., Johnson, B. R., Roberts, R., Jang, S., 
# Evans, C. S., Peteet, J., … Bradshaw, M. (2025, March 28). 
# Transcendent Accountability Scale Development Data and Materials. 
# https://doi.org/10.17605/OSF.IO/GV6ZS

# -------------------------------------------------
library(haven)
library(here)
library(ggplot2)


#transformed data into a csv2 file
data_raw <- read_sav(here("simulation", "raw_data", "TAcct Study 2 (N = 613) with Tr Guide deidentified OSF.sav"))
write.csv2(data_raw, file = here("simulation", "raw_data", "TAcct Study 2 (N = 613) with Tr Guide deidentified OSF.csv"), row.names = FALSE)

# imported and selected relevant data
data <- read.csv2(here("simulation", "raw_data", "TAcct Study 2 (N = 613) with Tr Guide deidentified OSF.csv"))
acc <- data[, c(3:12,14)] # 11 final accountability items 

# add columns for sum and mean score for each person
acc$sum <- rowSums(acc)
acc$mean <- rowMeans(acc[, 1:11])

# descriptive statistics
M <- mean(acc$mean)
SD <- sd(acc$mean)

# plot density function
ggplot(acc, aes(x = mean)) +
  geom_density() +
  xlab("mean accountability score") +
  xlim(1, 5) +  
  theme_minimal() 

# plot similar beta-distribution 
## required values after linear transformation:
M_beta <- (M-1)/4
SD_beta <- SD/4

## plot
alpha <- 6.6
beta  <- 1.38
curve(dbeta(x, alpha, beta),
      from = 0, to = 1, 
      ylab = "density", 
      xlab = "x")

## actual values of the plotted distribution
### mean
mean_plotted_beta <- alpha / (alpha + beta)
mean_plotted_beta

### standard deviation
sd_plotted_beta <- sqrt(alpha * beta / ((alpha + beta)^2 * (alpha + beta + 1)))
sd_plotted_beta

# -> plot and parameters closely resemble distribution of empirical data
#    with the exception of not being able to replicate the original bimodal
#    distribution

