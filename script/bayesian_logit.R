library(brms)
library(tidybayes)

# bayesian logit model 
bayesian_model <- brm(use ~ urban + age + livch,
                      data = df, 
                      family = bernoulli(link = "logit"),
                      warmup = 20,
                      iter = 800,
                      chains = 2,
                      init="0",
                      cores = 2,
                      seed = 831)
summary(bayesian_model)
set.seed(831)
df$use_pred_by_bayes <- predict(bayesian_model, newdata = df)

# get predict value by manual
coef_bayes_model <- bayesian_model |> 
  spread_draws(b_Intercept,b_urbanY,b_age,b_livch1,b_livch2,b_livch3P) |> 
  summarise_draws()
coef_bayes_model$mean[1]

func2 <- function(urban, age, livch){
  tmp<- coef_bayes_model$mean[1] + 
    coef_bayes_model$mean[2] * if_else(urban == 'Y',1,0) +
    coef_bayes_model$mean[3] * age +
    case_when(livch == '0' ~ 0,
              livch == '1' ~ coef_bayes_model$mean[4],
              livch == '2' ~ coef_bayes_model$mean[5],
              livch == '3+' ~ coef_bayes_model$mean[6])
  return(1 / (1 + exp(-tmp)))
}
# add column -> 一致しない.. parameter の平均で予測ではなく, iterごとのparameterで予測して, それらの平均? 
df <- df |> 
  mutate(use_pred_by_bayes_and_manual = func2(urban, age, livch))

## iterごとのparameterで予測して, それらの平均を取る. 
coef_bayes_model_raw <- bayesian_model |> 
  spread_draws(b_Intercept,b_urbanY,b_age,b_livch1,b_livch2,b_livch3P)
coef_bayes_model_raw$mean[1]

func3 <- function(urban, age, livch){
  tmp<- coef_bayes_model_raw$b_Intercept + 
    coef_bayes_model_raw$b_urbanY * if_else(urban == 'Y',1,0) +
    coef_bayes_model_raw$b_age * age +
    case_when(livch == '0' ~ 0,
              livch == '1' ~ coef_bayes_model_raw$b_livch1,
              livch == '2' ~ coef_bayes_model_raw$b_livch2,
              livch == '3+' ~ coef_bayes_model_raw$b_livch3P)
  return(1 / (1 + exp(-tmp)))
}

func3(df[1,]$urban, df[1,]$age, df[1,]$livch) |> 
  mean()

# 事後分布を利用する. 
set.seed(831)
posterior_predict(bayesian_model) |> 
  apply(2, mean) -> x
# https://cran.r-project.org/web/packages/brms/brms.pdf
