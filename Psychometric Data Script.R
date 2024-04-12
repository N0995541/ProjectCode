install.packages("tidyverse")
library(tidyverse)
install.packages("psyntur")
library(psyntur)
install.packages("emmeans")
library(emmeans)
install.packages("afex")
library(afex)
install.packages('lavaan')
library(lavaan)
install.packages('lavaanPlot')
library(lavaanPlot)
install.packages('lm.beta')
library(lm.beta)



psych_data <- rename(Psychometric_Data, 
                     binge_eating = bes.24h.mean, 
                     self_oriented = mps.SOP.24h.mean, 
                     socially_prescribed = mps.SPP.24h.mean, 
                     other_oriented = mps.OOP.24h.mean, 
                     social_anxiety = ssa.24h.mean)

psych_data <- select(psych_data,
                     binge_eating, 
                     self_oriented,
                     socially_prescribed,
                     other_oriented,
                     social_anxiety)

psych_data %>% drop_na()
social_anxiety

glimpse(psych_data)
view(psych_data)

describe(data = psych_data, mean_binge = mean(binge_eating), sd_binge = sd(binge_eating), skew_binge = psych::skew(binge_eating))
descriptive_info <- describe_across(psych_data, 
                                    variables = everything(),
                                    functions = list(mean = mean, sd = sd),
                                    pivot = TRUE)
glimpse(descriptive_info)

cor(psych_data)
scatterplot_matrix(psych_data,
                   binge_eating,
                   self_oriented,
                   socially_prescribed,
                   other_oriented,
                   social_anxiety
                   )

model_1 <- lm(binge_eating ~ self_oriented + socially_prescribed + other_oriented, data = psych_data)

summary(model_1)
confint(model_1)
coefficients(model_1)

model_s <- lm.beta(model_1)
summary(model_s)

model_residuals <- tibble(residuals = residuals(model_1))
histogram(data = model_residuals, residuals, bins = 25)

plot(model_1, which = 2)
plot(model_1, which = 1)
durbinWatsonTest(model_1)
plot(model_1, which = 3)

scatterplot_matrix(psych_data, self_oriented, social_anxiety, binge_eating)
scatterplot_matrix(psych_data, socially_prescribed, social_anxiety, binge_eating)
scatterplot_matrix(psych_data, other_oriented, social_anxiety, binge_eating)

mediation_self <- "
binge_eating ~ self_oriented + social_anxiety
social_anxiety ~ self_oriented
"
mediation_self_fit <- sem(mediation_self, data = psych_data)  
summary(mediation_self_fit)  
lavaanPlot(model = mediation_self_fit, coefs = TRUE)  

mediation_social <- "
binge_eating ~ socially_prescribed + social_anxiety
social_anxiety ~ socially_prescribed
"
mediation_social_fit <- sem(mediation_social, data = psych_data)  
summary(mediation_social_fit)  
lavaanPlot(model = mediation_social_fit, coefs = TRUE)  

mediation_other <- "
binge_eating ~ other_oriented + social_anxiety
social_anxiety ~ other_oriented
"
mediation_other_fit <- sem(mediation_other, data = psych_data)  
summary(mediation_other_fit)  
lavaanPlot(model = mediation_other_fit, coefs = TRUE)  

vif(model_1)
confint(model_1)

model_standardized <- lm.beta(model_1)
summary(model_standardized)


mediation_self_2 <- "
binge_eating ~ c_prime * self_oriented + b * social_anxiety
social_anxiety ~ a * self_oriented

indirect := a*b
direct := c_prime
total := indirect + direct
"
mediation_self_2_fit <- sem(mediation_self_2, data = psych_data)  
summary(mediation_self_2_fit)  
lavaanPlot(model = mediation_self_fit, coefs = TRUE)  

mediation_social_2 <- "
binge_eating ~ c_prime * socially_prescribed + b * social_anxiety
social_anxiety ~ a * socially_prescribed

indirect := a*b
direct := c_prime
total := indirect + direct
"
mediation_social_2_fit <- sem(mediation_social_2, data = psych_data)  
summary(mediation_social_2_fit)  
lavaanPlot(model = mediation_social_2_fit, coefs = TRUE)  

mediation_other_2 <- "
binge_eating ~ c_prime * other_oriented + b * social_anxiety
social_anxiety ~ a * other_oriented

indirect := a*b
direct := c_prime
total := indirect + direct
"
mediation_other_2_fit <- sem(mediation_other_2, data = psych_data)  
summary(mediation_other_2_fit)  
lavaanPlot(model = mediation_other_fit, coefs = TRUE)  




  
