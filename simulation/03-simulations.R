#1. simulating data
source("simulation/01-functions.R")
set.seed(41)

n = 10
df <- expand.grid(
  id = 1:n,
  anonymity = c(0, 0.5, 1),
  cues = c(0, 0.5, 1)
)

df$MOD <- round((rbeta(nrow(df), 1.6, 1.7)*4 + 1), 2) #simulated MOD values

df$base_resp <- round(rbeta(nrow(df), 6.6, 1.38), 2) #simulated base responsibility values

df$bad_sentence_percentage <- round(curse_function(df$anonymity, df$cues, df$MOD, df$base_resp),2) #simulated disinhibited behavior

# 2. plots with simulated data
p1 = ggplot(df, aes(x= anonymity, y = bad_sentence_percentage, color = factor(cues))) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange",
               position = position_dodge(width = 0.1)) +
  stat_summary(fun = mean, geom = "line",
               position = position_dodge(width = 0.1)) +
  theme_minimal()
print(p1) # original plot, with only anonymity and cues and IV

p2 = ggplot(df, aes(x= MOD, y = bad_sentence_percentage, color = factor(cues), fill = factor(cues))) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~ anonymity) + 
  theme_minimal()
print(p2) # plot with added MOD (!not very bw compatible, rework!)
