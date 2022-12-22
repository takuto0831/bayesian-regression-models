# package
library(tidyverse)
library(mlmRev)
library(skimr)

# data
data(Contraception)
df <- Contraception

#check
glimpse(df)
skim(df)

# logit model
glm_model <- glm(use ~ urban + age + livch,
                 data = df,
                 family = "binomial") 
summary(glm_model)
coef_model <- coef(glm_model)

df$pred_use <- predict(glm_model,newdata = df, type = "response")
# column:1の予想がどう動いているか?
1 / (1 + exp( - (coef_model[1] + coef_model[2]*1 + coef_model[3] * df$age[1] + coef_model[6]*1) ))
