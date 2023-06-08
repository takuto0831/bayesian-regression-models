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
set.seed(831)
df$use_pred_multi_level_bayes <- predict(bayesian_multi_model, newdata = df)

# 予測について
predict(bayesian_multi_model, newdata = df, probs = c(0.025,0.5,0.975)) |>
  head()

# 予測値のmanual取得
set.seed(831)
posterior_predict(bayesian_multi_model) |> 
  apply(2, mean) -> x
