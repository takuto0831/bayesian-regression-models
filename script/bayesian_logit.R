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
df$pred_bayes_use <- predict(bayesian_model, newdata = df)

# bayesian multi level logit model 
bayesian_multi_model <- brm(use ~ urban + age + livch + (1|district),
                            data = df, 
                            family = bernoulli(link = "logit"),
                            warmup = 20,
                            iter = 800,
                            chains = 2,
                            init="0",
                            cores = 2,
                            seed = 831)
summary(bayesian_multi_model)
df$pred_bayes_multi_use <- predict(bayesian_multi_model, newdata = df)

# 予測について
tmp <- predict(bayesian_multi_model, newdata = df, probs = c(0.025,0.5,0.975))
tmp |> head()

# この予測をどうやって取得しているか?
bayesian_multi_model |> 
  get_variables()

bayesian_multi_model |> 
  spread_draws(b_Intercept,b_urbanY,b_age,b_livch1,b_livch2,b_livch3P) |> 
  summarise_draws()

bayesian_multi_model |> 
  spread_draws(r_district[district,]) |> 
  summarise_draws()

# column:1の予想がどう動いているか?
1 / (1 + exp( - ( -1.69 -0.742 + 0.734*1 + (-0.0266)*df$age[1] + 1.35*1) ))
# B0 + B0  (district) + B * X
# 数値がズレる問題? 
# https://discourse.mc-stan.org/t/brms-understanding-the-basics-of-predictions/21885/4