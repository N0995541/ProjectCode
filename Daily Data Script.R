library(tidyverse)
library(lavaan)

remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")

Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)
install.packages("rstan", repos = "https://cloud.r-project.org/", dependancies = TRUE)

example(stan_model, package = "rstan", run.dontrun = TRUE)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
data_df <- tibble(x = rnorm(10))
M <- brm(x~1, data = data_df)

psych_data <- select(Total_Dataset,
                     id, 
                     day, 
                     other_oriented = mps.OOP.mean,
                     socially_prescribed = mps.SPP.mean, 
                     self_oriented = mps.SOP.mean,
                     social_anxiety = ssa.mean,
                     binge_eating = bes.mean)

mediation_other_2 <- "
binge_eating ~ c_prime * other_oriented + b * social_anxiety
social_anxiety ~ a * other_oriented

indirect := a*b
direct := c_prime
total := indirect + direct
"

mediation_other_2_fit <- sem(mediation_other_2, data = psych_data)

summary(mediation_other_2_fit) 

# Bayesian multilevel mediation -------------------------------------------

library(brms)

# m0 <- brm(binge_eating ~ other_oriented + social_anxiety + (1|id), 
#           data = psych_data, cores = 4)

bayes_mediation_model <- 
  bf(binge_eating ~ other_oriented + social_anxiety + (1|id)) +
  bf(social_anxiety ~ other_oriented + (1|id)) + 
  set_rescor(FALSE)

m <- brm(bayes_mediation_model, data = psych_data)

install.packages("bayestestR")
library(bayestestR)
mediation(m, ci = 0.95)
summary(m)



mediation_social_2 <- "
binge_eating ~ c_prime * socially_prescribed + b * social_anxiety
social_anxiety ~ a * socially_prescribed

indirect := a*b
direct := c_prime
total := indirect + direct
"

mediation_social_2_fit <- sem(mediation_social_2, data = psych_data)

summary(mediation_social_2_fit) 

# Bayesian multilevel mediation -------------------------------------------

library(brms)

# m0 <- brm(binge_eating ~ other_oriented + social_anxiety + (1|id), 
#           data = psych_data, cores = 4)

bayes_mediation_model_2 <- 
  bf(binge_eating ~ socially_prescribed + social_anxiety + (1|id)) +
  bf(social_anxiety ~ socially_prescribed + (1|id)) + 
  set_rescor(FALSE)

m2 <- brm(bayes_mediation_model_2, data = psych_data)


install.packages("bayestestR")
library(bayestestR)
mediation(m2, ci = 0.95)




mediation_self_2 <- "
binge_eating ~ c_prime * self_oriented + b * social_anxiety
social_anxiety ~ a * self_oriented

indirect := a*b
direct := c_prime
total := indirect + direct
"

mediation_self_2_fit <- sem(mediation_self_2, data = psych_data)

summary(mediation_self_2_fit) 

# Bayesian multilevel mediation -------------------------------------------

library(brms)

# m0 <- brm(binge_eating ~ other_oriented + social_anxiety + (1|id), 
#           data = psych_data, cores = 4)

bayes_mediation_model_3 <- 
  bf(binge_eating ~ self_oriented + social_anxiety + (1|id)) +
  bf(social_anxiety ~ self_oriented + (1|id)) + 
  set_rescor(FALSE)

m3 <- brm(bayes_mediation_model_3, data = psych_data)

install.packages("bayestestR")
library(bayestestR)
mediation(m3, ci = 0.95)
